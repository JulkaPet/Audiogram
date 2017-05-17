function varargout = ba1(varargin) %output variable
% BA1 M-file for ba1.fig
%      BA1, by itself, creates a new BA1 or raises the existing
%      singleton*.
%
%      H = BA1 returns the handle to a new BA1 or the handle to
%      the existing singleton*.
%
%      BA1('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in BA1.M with the given input arguments.
%
%      BA1('Property','Value',...) creates a new BA1 or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before ba1_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to ba1_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help ba1

% Last Modified by GUIDE v2.5 27-Apr-2017 10:33:49

% SETUP + EXPERIMENT NOTES:
%
%   Fireface UC settings:
%   - Gain LineIn 5-8: use -10dBV, which has a max level of +2 dBV
%     (0dBV = 1 V RMS. +2 dBV = 1.25 V RMS (= 1.78 V p for a sine wave of 1.25 V RMS)).  
%   - Gain LineOut 1-6: use +4dBu, which has a max level of +13 dBu
%     (0dBu = 0.77 V RMS. +13 dBu = 3.46 V RMS (= 4.89 Vpk for a sine wave of 3.46 V RMS)).  
%   - adjust Gain of Mic input so that the loudest signal that we want to
%     correctly record just triggers the CLIP LED (which reacts at -2 dBFS).
%   - adjust playback signals so that the loudest amplitude we play has a
%     pp-voltage of +/- 4V (AFTER filtering with the cIR).
%
%   Programmable Attenuator CS3310:
%   - Max. analog input voltage: +/- 5 V
%
%   Knowles MEMS microphone SPU0410:
%   - Sensitivity: -38 (-41 - -35) dBV/Pa @ 1kHz
%   - SNR: 63 dBA @ 1 kHz --> Dynamic range for us max ~60 dB
%
%   Power Amplifier:
%   - set to -10 dB
%
%   Playback Amplitudes:
%   - maximum: 0.031622777
%   - we present levels of 20 - 90 dB peSPL in steps of 2 dB
%
%   Frequencies:
%   - we test 5 to 95 kHz in steps of 5 kHz

% Implemented changes 27. April 2017 (TH --> Back-up of original version)
%
% - from 2dB steps to 5dB steps
% - added GUI: additional protocol (2-Sinus-4X4ms, Superstimulus), additional "treatment"
% (none, noise, light) --> this was also already changed in the back-up
% further changes 17.Mai 2017:
% - adapting script to new GUI
% 2-Sinus-4X4ms
% 3-Superstimulus
% dur was initially with 2*ramps, I deleted the ramps as they get added
% later





%% INITIALIZE GUI: =======================================================
% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
    'gui_Singleton',  gui_Singleton, ...
    'gui_OpeningFcn', @ba1_OpeningFcn, ...
    'gui_OutputFcn',  @ba1_OutputFcn, ...
    'gui_LayoutFcn',  [] , ...
    'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


%% INITIALIZE EXPERIMENT: ================================================
% --- Executes just before ba1 is made visible.
function ba1_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to ba1 (see VARARGIN)

% Choose default command line output for ba1
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes ba1 wait for user response (see UIRESUME)
% uiwait(handles.figure1);

% LOAD SPEAKER cIR (variables 'ir','irc',
% 'rec_uncompensated','rec_compensated','fn','cfn'):
load('compIR_neoCD1.0_BA1_2015-08-07.mat');
handles.irc = irc;

% SET PARAMETERS:
handles.FS = 192000;                            % sound sampling frequency
handles.messages = {};
guidata(hObject, handles);                      % update handles structure

% reset random number generator:
rand('state',sum(100*clock));

% SET GUI:
set(handles.push_stop, 'Enable','off');          % deactivate Start Button


% % --- Executes when user attempts to close figure1.
% function figure1_CloseRequestFcn(hObject, eventdata, handles)
% % hObject    handle to figure1 (see GCBO)
% % eventdata  reserved - to be defined in a future version of MATLAB
% % handles    structure with handles and user data (see GUIDATA)

% % Hint: delete(hObject) closes the figure
% if exist('vidfile'),
%     close(vidfile);
%     delete(vidfile);
%     clear vidfile;
% end
% delete(vid)
% clear vid
% clear src
% delete(hObject);



%% GUI OBJECT FUNCTIONS: =================================================
% --- Outputs from this function are returned to the command line.
function varargout = ba1_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on selection change in popup_spec.
function popup_spec_Callback(hObject, eventdata, handles)
% hObject    handle to popup_spec (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns popup_spec contents as cell array
%        contents{get(hObject,'Value')} returns selected item from popup_spec

% --- Executes during object creation, after setting all properties.
function popup_spec_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popup_spec (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in push_start.
function push_start_Callback(hObject, eventdata, handles)
% hObject    handle to push_start (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% CHECK CORRECT USER INPUT:
spec_s = get(handles.popup_spec,'String');
spec_v = get(handles.popup_spec,'Value');
spec_str = spec_s{spec_v};
if strcmp(spec_str, '[please select]'),
    error('Error:ba1:start_exp', 'Please select species first!');
end
spec_str = spec_str(1:5);

prot_s = get(handles.popup_protocol,'String');
prot_v = get(handles.popup_protocol,'Value');
prot_str = prot_s{prot_v};
if strcmp(prot_str, '[please select]'),
    error('Error:ba1:start_exp', 'Please select protocol first!');
end

indnum = get(handles.edit_individual, 'String');
if strcmp(indnum, '[edit_No]'),
    error('Error:ba1:start_exp', 'Please enter individual number first!');
end
indnum = str2double(indnum);
indnum_str = sprintf('%03.0f',indnum);

% SET GUI & PARAMETERS:
set(handles.push_start, 'Enable','off');        % deactivate Start Button
set(handles.popup_spec, 'Enable','off');        % deactivate Species Selection
set(handles.popup_protocol, 'Enable','off');    % deactivate Protocol Selection
set(handles.push_stop, 'Enable','on');          % activate Stop Button
disp(' ');
disp('=================================================================');
disp('===== Experiment started!!! =====================================');
disp('=================================================================');
disp(' ');
newmessage(hObject, handles, {' '; '=============== Experiment started!!! ==============='; ' '});
handles = guidata(hObject);

% GENERATE/LOAD INDIVIDUAL DATA:
% - check for / make data path:
data_pn = ['C:\Users\setupuser\Documents\BA1\data\' spec_str '_Ind' indnum_str '\'];
if isdir(data_pn)==0,
    mkdir(data_pn);
end
% - check for / make meta-results file:
if exist([data_pn '\results_' spec_str '_Ind' indnum_str '.mat'],'file')==2,
    % load data file with variables 'results', 'freqs', 'freq_order', & 'freq_counter:
    load([data_pn '\results_' spec_str '_Ind' indnum_str '.mat']);
    disp(['Existing data loaded of individual #' indnum_str ' of species ' spec_str]);
else
    freqs = 5 : 5 : 95;                     % playback frequencies (kHz)
    freq_order = randperm(length(freqs));   % order of playback frequencies
    freq_counter = 1;                       % frequency counter: indicates the number of the NEXT frequency to present (not the currently/last presented one)!
    % empty results file:
    results = [];
    % save data:
    save([data_pn '\results_' spec_str '_Ind' indnum_str '.mat'], 'results','freqs','freq_order','freq_counter');
    disp(['New data generated and saved for individual #' indnum_str ' of species ' spec_str]);
end

% RUN EXPERIMENT:
switch prot_str %Execute one of several groups of statements
    
    case '1-Sinus-1'
        protocol_BA_Sinus_1(hObject, handles, data_pn, results,freqs,freq_order,freq_counter, handles.irc);
        
    case '2-Silence'
        protocol_BA_silence(hObject, handles, data_pn, results,freqs,freq_order,freq_counter, handles.irc);
end



% --- Executes on button press in push_stop.
function push_stop_Callback(hObject, eventdata, handles)
% hObject    handle to push_stop (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% SET GUI & PARAMETERS:
set(handles.push_start, 'Enable','on');        % activate Start Button
set(handles.popup_spec, 'Enable','on');     % activate Species Selection
set(handles.popup_protocol, 'Enable','on');    % activate Protocol Selection
set(handles.push_stop, 'Enable','off');        % deactivate Start Button
disp(' ');
disp('=================================================================');
disp('===== Experiment stopped!!! =====================================');
disp('=================================================================');
disp(' ');
newmessage(hObject, handles, {' '; '=============== Experiment stopped!!! ==============='; ' '});
handles = guidata(hObject);


function edit_individual_Callback(hObject, eventdata, handles)
% hObject    handle to edit_individual (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit_individual as text
%        str2double(get(hObject,'String')) returns contents of edit_individual as a double

% --- Executes during object creation, after setting all properties.
function edit_individual_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit_individual (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in popup_protocol.
function popup_protocol_Callback(hObject, eventdata, handles)
% hObject    handle to popup_protocol (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns popup_protocol contents as cell array
%        contents{get(hObject,'Value')} returns selected item from popup_protocol


% --- Executes during object creation, after setting all properties.
function popup_protocol_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popup_protocol (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end





%% USER FUNCTIONS ========================================================

%% PROTOCOL_BA_SINUS_1: run Protocol 'BA Sinus 1' ==========
function protocol_BA_Sinus_1(hObject, handles, data_pn, results,freqs,freq_order,freq_counter, irc)

FS = handles.FS;

% set trial information:
spec_s = get(handles.popup_spec,'String');
spec_v = get(handles.popup_spec,'Value');
spec_str = spec_s{spec_v};
spec_str = spec_str(1:5);

prot_s = get(handles.popup_protocol,'String');
prot_v = get(handles.popup_protocol,'Value');
prot_str = prot_s{prot_v};

indnum = str2double(get(handles.edit_individual, 'String'));
indnum_str = sprintf('%03.0f',indnum);         % string with 3 leading digits; matrix of 000 which get filled up with "indnum"

date_num = now;
date_str = datestr(date_num,'YYYY-mm-DD_HH-MM-SS');

prot_num = 1;                           % protocol_number: 1 = Audiogram with sinusoids

% define signal parameters + playback sequence:
dBs = 20 : 5 : 90;                  % playback level (dB peSPL)
dBs = dBs - max(dBs);               % playback level (dB FS)
peak = 10.^(10/20) * 0.01;          % playback amplitude of loudest pulse (90 dB peSPL) (lin. units)
peaks = 10.^(dBs/20) * peak;        % playback amplitude of all pulses (lin. units)

sig_dur = 0.02;                         % duration of each sine wave without ramps (s)
ramp_dur = 0.002;                       % ramp duration
sildur = 0.5 - sig_dur - 2*ramp_dur;    % silence duration between signals (sec)

for fc = freq_counter:length(freqs),    % Freq-Counter indicates the number of the NEXT frequency to present (not the currently/last presented one)!
    
    freq = freqs(freq_order(fc));       % current frequency
    dur = sig_dur + 2*ramp_dur;         % total signal duration incl. ramps
    dur2 = dur + 0.1;                   % duration for initial generation of sinusoid, including additional time
    t = linspace(1/FS, dur, dur*FS);    % time vector of final playback sinusoids
    t2 = linspace(1/FS, dur2, dur2*FS); % time vector of initial playback sinusoids
    
    disp(' ');
    disp('==============================================================');
    disp(['BA1: NEW FREQUENCY: ' num2str(freq) ' kHz']);
    disp(['     (# ' num2str(fc) ' out of ' num2str(length(freqs)) ' frequencies)']);
    newmessage(hObject, handles, {' ';...
        '============================================';...
        ['BA1: NEW FREQUENCY: ' num2str(freq) ' kHz'];...
        ['     (# ' num2str(fc) ' out of ' num2str(length(freqs)) ' frequencies)'];...
        ' '});
    handles = guidata(hObject);
    
    % PREPARE PLAYBACK OF CURRENT FREQUENCY FOR ALL INTENSITIES:
    sig = NaN(length(t),length(peaks));
    for ic = 1:length(peaks),
        
        % generate sinusoidal signal:
        sig1 = peaks(ic) * sin(t2*2*pi * freq*1000);  % signal without ramps
        
        % apply compensatory IR:
        fsig1 = filter(irc,1,sig1);
        
        % cut-out a shorter part from fsig1, starting at the end, to
        % exclude the initial attack time caused by the filter:
        fsig1 = fsig1(end-0.01*FS-dur*FS+1 : end-0.01*FS);
        
        % apply raised cosine ramps:
        tr = linspace(0, pi, ramp_dur*FS);
        upramp = (-1*cos(tr)+1)/2;
        downramp = fliplr(upramp);
        fsig1(1:length(upramp)) =  fsig1(1:length(upramp)).* upramp;
        fsig1(end-length(downramp)+1 : end) = fsig1(end-length(downramp)+1 : end) .* downramp;
        
        % add fsig1 to sig:
        sig(:,ic) = col(fsig1);
    end
    
    % start video, play signal, retrieve behavioural data, stop video:
    freq_str = sprintf('%02.0f',freq);         % data into string with two leading figures (in front of the decimal point)
    vidfilename{1} = [data_pn prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str '_Cam01.avi'];  % definition of video file name
    vidfilename{2} = [data_pn prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str '_Cam02.avi'];
    [recsig, xruns] = smp_playrec_audiogram(sig,FS,sildur, vidfilename);
    disp(' ');
    disp(['BA1: Finished smp_playrec_audiogram (Sinus). size(recsig) = ' num2str(size(recsig)) ', dur(recsig) = ' num2str(length(recsig)/FS) ' sec, # xruns = ' num2str(xruns)]);
    
    % Save measured moth behavioural data as WAV-file:
    % - WAV-filename:
    wav_fn = [prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str];
    % - mic-recording. Assuming that we use the full dynamic range of the
    % Fireface Input of up to 1.78 Vpk, we divide the mic-recording by 2 to
    % come within the range of +/-1 that can be saved as wav-file:
    mic_rec = recsig(:,1) / 2;
    % - speaker recordings:
    sp_rec = recsig(:,2:3);
    if max(max(abs(sp_rec))) > 0.99,    % to check for cliping
        f = max([2, ceil(max(max(abs(sp_rec)))/0.99)]);
        sp_rec = sp_rec./f;
        fstr = sprintf('%02.0f',f);
        wav_fn = [wav_fn '_DIV' fstr];  % adds value of division to the filename
    end
    audiowrite([data_pn wav_fn '.wav'], [sp_rec, mic_rec] ,FS, 'BitsPerSample',16);
      
    % save meta trial data:
    counter = size(results,1) + 1;
    trial_info = [counter, date_num/1e5, indnum, prot_num, freq];
    results = [results; trial_info];
    freq_counter = freq_counter + 1;    % Freq-Counter indicates the number of the NEXT frequency to present (not the currently/last presented one)!
    save([data_pn '\results_' spec_str '_Ind' indnum_str '.mat'], 'results','freqs','freq_order','freq_counter');
    disp('BA1: All Data (Video, Audio + Meta) Saved!');
    
    % Plot recorded data:
    t = linspace(1/FS, length(sp_rec)/FS, length(sp_rec));
    figure(2)
    subplot(3,1,1);
    plot(t,mic_rec, 'k');
    ylabel('mic (arb. lin. units)', 'FontSize',8)
    
    subplot(3,1,2);
    plot(t,sp_rec(:,1), 'r');   % speaker 1 = Left
    ylabel({'speaker 1 = ';'LEFT (arb. lin. units)'}, 'FontSize',8)
    
    subplot(3,1,3);
    plot(t,sp_rec(:,2), 'b');   % speaker 2 = Right
    xlabel('time (sec)', 'FontSize',8);
    ylabel({'speaker 2 = ';'RIGHT (arb. lin. units)'}, 'FontSize',8)
    
    % check for User Input to play next frequency:
    if fc < length(freqs),
        in = 'x';
        while ~strcmp('n',in),
            in = input(['BA1: Type "n" to continue with next frequency (' num2str(freqs(fc+1)) ' kHz): '],'s');
            pause(0.01);
        end
        
    else
        disp(' ');
        disp('BA1: FINISHED WITH LAST FREQUENCY OF AUDIOGRAM!');
        disp(' ');
        disp('=========================================================');
        disp('=========================================================');
        disp(' ');
    end
end

% SET GUI & PARAMETERS:
set(handles.push_start, 'Enable','on');        % activate Start Button
set(handles.popup_spec, 'Enable','on');        % activate Species Selection
set(handles.popup_protocol, 'Enable','on');    % activate Protocol Selection
set(handles.push_stop, 'Enable','off');          % deactivate Stop Button
newmessage(hObject, handles, {'===== BA1: ALL FREQS PRESENTED to this Moth! =====';' '});
handles = guidata(hObject);


%% PROTOCOL_BA_SINUS_2: run Protocol 'BA Sinus 2 4x4ms' ==========
function protocol_BA_Sinus_2 (hObject, handles, data_pn, results,freqs,freq_order,freq_counter, irc)

FS = handles.FS;

% set trial information:
spec_s = get(handles.popup_spec,'String');
spec_v = get(handles.popup_spec,'Value');
spec_str = spec_s{spec_v};
spec_str = spec_str(1:5);

prot_s = get(handles.popup_protocol,'String');
prot_v = get(handles.popup_protocol,'Value');
prot_str = prot_s{prot_v};

indnum = str2double(get(handles.edit_individual, 'String'));
indnum_str = sprintf('%03.0f',indnum);         % string with 3 leading digits; matrix of 000 which get filled up with "indnum"

date_num = now;
date_str = datestr(date_num,'YYYY-mm-DD_HH-MM-SS');

prot_num = 2;                           % protocol_number: 2 = Audiogram with repetitive sinusoids

% define signal parameters + playback sequence:
dBs = 20 : 5 : 90;                  % playback level (dB peSPL)
dBs = dBs - max(dBs);               % playback level (dB FS)
peak = 10.^(10/20) * 0.01;          % playback amplitude of loudest pulse (90 dB peSPL) (lin. units)
peaks = 10.^(dBs/20) * peak;        % playback amplitude of all pulses (lin. units)

sig_dur = 0.004;                            % duration of each sine wave without ramps (s)
ramp_dur = 0.0002;                          % ramp duration
sig_int = 0.03;                             % signal interval
int_dur = sig_int - (sig_dur + 2*ramp_dur); % duration of silence after each signal (intersignal interval)
t3 = linespace(1/FS, int_dur, int_dur*FS);
rep_rate = 4;                               % number of signals
sildur = 0.5 - (rep_rate * sig_int);        % silence duration between signals (sec)


for fc = freq_counter:length(freqs),    % Freq-Counter indicates the number of the NEXT frequency to present (not the currently/last presented one)!
    
    freq = freqs(freq_order(fc));       % current frequency
    dur = sig_dur 2*ramp_dur ;          % total signal duration incl. ramps
    dur2 = dur + 0.1;                   % duration for initial generation of sinusoid, including additional time
    %%%%%%%%%%%%%%%% --> do I have to change additional time? what is it
    %%%%%%%%%%%%%%%% for?
    t = linspace(1/FS, dur, dur*FS);    % time vector of final playback sinusoids
    t2 = linspace(1/FS, dur2, dur2*FS); % time vector of initial playback sinusoids
    t4=rep_rate*(length(t)+length(t3)); % length of total time vector
    
        
    disp(' ');
    disp('==============================================================');
    disp(['BA1: NEW FREQUENCY: ' num2str(freq) ' kHz']);
    disp(['     (# ' num2str(fc) ' out of ' num2str(length(freqs)) ' frequencies)']);
    newmessage(hObject, handles, {' ';...
        '============================================';...
        ['BA1: NEW FREQUENCY: ' num2str(freq) ' kHz'];...
        ['     (# ' num2str(fc) ' out of ' num2str(length(freqs)) ' frequencies)'];...
        ' '});
    handles = guidata(hObject);
    
    % PREPARE PLAYBACK OF CURRENT FREQUENCY FOR ALL INTENSITIES:
    sig = NaN(length(t4),length(peaks));
    for ic = 1:length(peaks),
        
        % generate sinusoidal signal:
        single_sig = peaks(ic) * sin(t*2*pi * freq*1000);   % signal length enlongated for the time of the ramps which will be applied later
        %sig1 =  single_sig + zeros(size(t3));               % one signal with intersignal intervall  
        
        % apply compensatory IR:
        fsig1 = filter(irc,1,single_sig);
        
        % cut-out a shorter part from fsig1, starting at the end, to
        % exclude the initial attack time caused by the filter:
        fsig1 = fsig1(end-0.01*FS-dur*FS+1 : end-0.01*FS);
        
        % apply raised cosine ramps:
        tr = linspace(0, pi, ramp_dur*FS);
        upramp = (-1*cos(tr)+1)/2;
        downramp = fliplr(upramp);
        fsig1(1:length(upramp)) =  fsig1(1:length(upramp)).* upramp;
        fsig1(end-length(downramp)+1 : end) = fsig1(end-length(downramp)+1 : end) .* downramp;
        
        % introduce repetition
        fsig1= fsig1 + zeros(size(t3)) + fsig1 + zeros(size(t3)) + fsig1 + zeros(size(t3)) + fsig1 + zeros(size(t3))
        
        % add fsig1 to sig:
        sig(:,ic) = col(fsig1);
    end
    
    % start video, play signal, retrieve behavioural data, stop video:
    freq_str = sprintf('%02.0f',freq);         % data into string with two leading figures (in front of the decimal point)
    vidfilename{1} = [data_pn prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str '_Cam01.avi'];  % definition of video file name
    vidfilename{2} = [data_pn prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str '_Cam02.avi'];
    [recsig, xruns] = smp_playrec_audiogram(sig,FS,sildur, vidfilename);
    disp(' ');
    disp(['BA1: Finished smp_playrec_audiogram (Sinus). size(recsig) = ' num2str(size(recsig)) ', dur(recsig) = ' num2str(length(recsig)/FS) ' sec, # xruns = ' num2str(xruns)]);
    
    % Save measured moth behavioural data as WAV-file:
    % - WAV-filename:
    wav_fn = [prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str];
    % - mic-recording. Assuming that we use the full dynamic range of the
    % Fireface Input of up to 1.78 Vpk, we divide the mic-recording by 2 to
    % come within the range of +/-1 that can be saved as wav-file:
    mic_rec = recsig(:,1) / 2;
    % - speaker recordings:
    sp_rec = recsig(:,2:3);
    if max(max(abs(sp_rec))) > 0.99,    % to check for cliping
        f = max([2, ceil(max(max(abs(sp_rec)))/0.99)]);
        sp_rec = sp_rec./f;
        fstr = sprintf('%02.0f',f);
        wav_fn = [wav_fn '_DIV' fstr];  % adds value of division to the filename
    end
    audiowrite([data_pn wav_fn '.wav'], [sp_rec, mic_rec] ,FS, 'BitsPerSample',16);
      
    % save meta trial data:
    counter = size(results,1) + 1;
    trial_info = [counter, date_num/1e5, indnum, prot_num, freq];
    results = [results; trial_info];
    freq_counter = freq_counter + 1;    % Freq-Counter indicates the number of the NEXT frequency to present (not the currently/last presented one)!
    save([data_pn '\results_' spec_str '_Ind' indnum_str '.mat'], 'results','freqs','freq_order','freq_counter');
    disp('BA1: All Data (Video, Audio + Meta) Saved!');
    
    % Plot recorded data:
    t = linspace(1/FS, length(sp_rec)/FS, length(sp_rec));
    figure(2)
    subplot(3,1,1);
    plot(t,mic_rec, 'k');
    ylabel('mic (arb. lin. units)', 'FontSize',8)
    
    subplot(3,1,2);
    plot(t,sp_rec(:,1), 'r');   % speaker 1 = Left
    ylabel({'speaker 1 = ';'LEFT (arb. lin. units)'}, 'FontSize',8)
    
    subplot(3,1,3);
    plot(t,sp_rec(:,2), 'b');   % speaker 2 = Right
    xlabel('time (sec)', 'FontSize',8);
    ylabel({'speaker 2 = ';'RIGHT (arb. lin. units)'}, 'FontSize',8)
    
    % check for User Input to play next frequency:
    if fc < length(freqs),
        in = 'x';
        while ~strcmp('n',in),
            in = input(['BA1: Type "n" to continue with next frequency (' num2str(freqs(fc+1)) ' kHz): '],'s');
            pause(0.01);
        end
        
    else
        disp(' ');
        disp('BA1: FINISHED WITH LAST FREQUENCY OF AUDIOGRAM!');
        disp(' ');
        disp('=========================================================');
        disp('=========================================================');
        disp(' ');
    end
end

% SET GUI & PARAMETERS:
set(handles.push_start, 'Enable','on');        % activate Start Button
set(handles.popup_spec, 'Enable','on');        % activate Species Selection
set(handles.popup_protocol, 'Enable','on');    % activate Protocol Selection
set(handles.push_stop, 'Enable','off');          % deactivate Stop Button
newmessage(hObject, handles, {'===== BA1: ALL FREQS PRESENTED to this Moth! =====';' '});
handles = guidata(hObject);


%% PROTOCOL_BA_SINUS_2: run Protocol 'BA Superstimulus' ==========
function protocol_BA_Sinus_2 (hObject, handles, data_pn, results,freqs,freq_order,freq_counter, irc)

FS = handles.FS;

% set trial information:
spec_s = get(handles.popup_spec,'String');
spec_v = get(handles.popup_spec,'Value');
spec_str = spec_s{spec_v};
spec_str = spec_str(1:5);

prot_s = get(handles.popup_protocol,'String');
prot_v = get(handles.popup_protocol,'Value');
prot_str = prot_s{prot_v};

indnum = str2double(get(handles.edit_individual, 'String'));
indnum_str = sprintf('%03.0f',indnum);         % string with 3 leading digits; matrix of 000 which get filled up with "indnum"

date_num = now;
date_str = datestr(date_num,'YYYY-mm-DD_HH-MM-SS');

prot_num = 3;                           % protocol_number: 2 = Audiogram with repetitive sinusoids

% define signal parameters + playback sequence:
dBs = 20 : 5 : 90;                  % playback level (dB peSPL)
dBs = dBs - max(dBs);               % playback level (dB FS)
peak = 10.^(10/20) * 0.01;          % playback amplitude of loudest pulse (90 dB peSPL) (lin. units)
peaks = 10.^(dBs/20) * peak;        % playback amplitude of all pulses (lin. units)

sig_dur = 0.004;                            % duration of each sine wave without ramps (s)
ramp_dur = 0.0002;                          % ramp duration
sig_int = 0.03;                             % signal interval
int_dur = sig_int - (sig_dur + 2*ramp_dur); % duration of silence after each signal (intersignal interval)
t3 = linespace(1/FS, int_dur, int_dur*FS);
rep_rate = 4;                               % number of signals
sildur = 0.5 - (rep_rate * sig_int);        % silence duration between signals (sec)


for fc = freq_counter:length(freqs),    % Freq-Counter indicates the number of the NEXT frequency to present (not the currently/last presented one)!
    
    freq = freqs(freq_order(fc));       % current frequency
    dur = sig_dur 2*ramp_dur ;          % total signal duration incl. ramps
    dur2 = dur + 0.1;                   % duration for initial generation of sinusoid, including additional time
    %%%%%%%%%%%%%%%% --> do I have to change additional time? what is it
    %%%%%%%%%%%%%%%% for?
    t = linspace(1/FS, dur, dur*FS);    % time vector of final playback sinusoids
    t2 = linspace(1/FS, dur2, dur2*FS); % time vector of initial playback sinusoids
    t4=rep_rate*(length(t)+length(t3)); % length of total time vector
    
        
    disp(' ');
    disp('==============================================================');
    disp(['BA1: NEW FREQUENCY: ' num2str(freq) ' kHz']);
    disp(['     (# ' num2str(fc) ' out of ' num2str(length(freqs)) ' frequencies)']);
    newmessage(hObject, handles, {' ';...
        '============================================';...
        ['BA1: NEW FREQUENCY: ' num2str(freq) ' kHz'];...
        ['     (# ' num2str(fc) ' out of ' num2str(length(freqs)) ' frequencies)'];...
        ' '});
    handles = guidata(hObject);
    
    % PREPARE PLAYBACK OF CURRENT FREQUENCY FOR ALL INTENSITIES:
    sig = NaN(length(t4),length(peaks));
    for ic = 1:length(peaks),
        
        % generate sinusoidal signal:
        single_sig = peaks(ic) * sin(t*2*pi * freq*1000);   % signal length enlongated for the time of the ramps which will be applied later
        %sig1 =  single_sig + zeros(size(t3));               % one signal with intersignal intervall  
        
        % apply compensatory IR:
        fsig1 = filter(irc,1,single_sig);
        
        % cut-out a shorter part from fsig1, starting at the end, to
        % exclude the initial attack time caused by the filter:
        fsig1 = fsig1(end-0.01*FS-dur*FS+1 : end-0.01*FS);
        
        % apply raised cosine ramps:
        tr = linspace(0, pi, ramp_dur*FS);
        upramp = (-1*cos(tr)+1)/2;
        downramp = fliplr(upramp);
        fsig1(1:length(upramp)) =  fsig1(1:length(upramp)).* upramp;
        fsig1(end-length(downramp)+1 : end) = fsig1(end-length(downramp)+1 : end) .* downramp;
        
        % introduce repetition
        fsig1= fsig1 + zeros(size(t3)) + fsig1 + zeros(size(t3)) + fsig1 + zeros(size(t3)) + fsig1 + zeros(size(t3))
        
        % add fsig1 to sig:
        sig(:,ic) = col(fsig1);
    end
    
    % start video, play signal, retrieve behavioural data, stop video:
    freq_str = sprintf('%02.0f',freq);         % data into string with two leading figures (in front of the decimal point)
    vidfilename{1} = [data_pn prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str '_Cam01.avi'];  % definition of video file name
    vidfilename{2} = [data_pn prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str '_Cam02.avi'];
    [recsig, xruns] = smp_playrec_audiogram(sig,FS,sildur, vidfilename);
    disp(' ');
    disp(['BA1: Finished smp_playrec_audiogram (Sinus). size(recsig) = ' num2str(size(recsig)) ', dur(recsig) = ' num2str(length(recsig)/FS) ' sec, # xruns = ' num2str(xruns)]);
    
    % Save measured moth behavioural data as WAV-file:
    % - WAV-filename:
    wav_fn = [prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str];
    % - mic-recording. Assuming that we use the full dynamic range of the
    % Fireface Input of up to 1.78 Vpk, we divide the mic-recording by 2 to
    % come within the range of +/-1 that can be saved as wav-file:
    mic_rec = recsig(:,1) / 2;
    % - speaker recordings:
    sp_rec = recsig(:,2:3);
    if max(max(abs(sp_rec))) > 0.99,    % to check for cliping
        f = max([2, ceil(max(max(abs(sp_rec)))/0.99)]);
        sp_rec = sp_rec./f;
        fstr = sprintf('%02.0f',f);
        wav_fn = [wav_fn '_DIV' fstr];  % adds value of division to the filename
    end
    audiowrite([data_pn wav_fn '.wav'], [sp_rec, mic_rec] ,FS, 'BitsPerSample',16);
      
    % save meta trial data:
    counter = size(results,1) + 1;
    trial_info = [counter, date_num/1e5, indnum, prot_num, freq];
    results = [results; trial_info];
    freq_counter = freq_counter + 1;    % Freq-Counter indicates the number of the NEXT frequency to present (not the currently/last presented one)!
    save([data_pn '\results_' spec_str '_Ind' indnum_str '.mat'], 'results','freqs','freq_order','freq_counter');
    disp('BA1: All Data (Video, Audio + Meta) Saved!');
    
    % Plot recorded data:
    t = linspace(1/FS, length(sp_rec)/FS, length(sp_rec));
    figure(2)
    subplot(3,1,1);
    plot(t,mic_rec, 'k');
    ylabel('mic (arb. lin. units)', 'FontSize',8)
    
    subplot(3,1,2);
    plot(t,sp_rec(:,1), 'r');   % speaker 1 = Left
    ylabel({'speaker 1 = ';'LEFT (arb. lin. units)'}, 'FontSize',8)
    
    subplot(3,1,3);
    plot(t,sp_rec(:,2), 'b');   % speaker 2 = Right
    xlabel('time (sec)', 'FontSize',8);
    ylabel({'speaker 2 = ';'RIGHT (arb. lin. units)'}, 'FontSize',8)
    
    % check for User Input to play next frequency:
    if fc < length(freqs),
        in = 'x';
        while ~strcmp('n',in),
            in = input(['BA1: Type "n" to continue with next frequency (' num2str(freqs(fc+1)) ' kHz): '],'s');
            pause(0.01);
        end
        
    else
        disp(' ');
        disp('BA1: FINISHED WITH LAST FREQUENCY OF AUDIOGRAM!');
        disp(' ');
        disp('=========================================================');
        disp('=========================================================');
        disp(' ');
    end
end

% SET GUI & PARAMETERS:
set(handles.push_start, 'Enable','on');        % activate Start Button
set(handles.popup_spec, 'Enable','on');        % activate Species Selection
set(handles.popup_protocol, 'Enable','on');    % activate Protocol Selection
set(handles.push_stop, 'Enable','off');          % deactivate Stop Button
newmessage(hObject, handles, {'===== BA1: ALL FREQS PRESENTED to this Moth! =====';' '});
handles = guidata(hObject);


%% PROTOCOL_BA_silence: run Protocol 'silence control' ==========
function protocol_BA_silence(hObject, handles, data_pn, results,freqs,freq_order,freq_counter, irc)

FS = handles.FS;

% set trial information:
spec_s = get(handles.popup_spec,'String');
spec_v = get(handles.popup_spec,'Value');
spec_str = spec_s{spec_v};
spec_str = spec_str(1:5);

prot_s = get(handles.popup_protocol,'String');
prot_v = get(handles.popup_protocol,'Value');
prot_str = prot_s{prot_v};

indnum = str2double(get(handles.edit_individual, 'String'));
indnum_str = sprintf('%03.0f',indnum);         % matrix of 000 which get filled up with "indnum"

date_num = now;
date_str = datestr(date_num,'YYYY-mm-DD_HH-MM-SS');

prot_num = 4;                           % protocol_number: 2 = Silence control / pre-audiogram

% define signal parameters + playback sequence:
dBs = 20 : 5 : 90;                  % playback level (dB peSPL)
dBs = dBs - max(dBs);               % playback level (dB FS)
peak = 10.^(10/20) * 0.01;          % playback amplitude of loudest pulse (90 dB peSPL) (lin. units)
peaks = 10.^(dBs/20) * peak;        % playback amplitude of all pulses (lin. units)

sig_dur = 0.02;                         % duration of each sine wave without ramps (s)
ramp_dur = 0.002;                       % ramp duration
sildur = 0.5 - sig_dur - 2*ramp_dur;    % silence duration between signals (sec)

dur = sig_dur + 2*ramp_dur;             % total signal duration incl. ramps
dur2 = dur + 0.1;                       % duration for initial generation of sinusoid, including additional time
t = linspace(1/FS, dur, dur*FS);        % time vector of final playback sinusoids
t2 = linspace(1/FS, dur2, dur2*FS);     % time vector of initial playback sinusoids

% fake frequency:
freq = 0;       % current frequency

disp(' ');
disp('==============================================================');
disp('BA1: SILENCE CONTROL');
disp(' ');
newmessage(hObject, handles, {' ';...
    '============================================';...
    'BA1: SILENCE CONTROL';...
    ' '});
handles = guidata(hObject);

% PREPARE PLAYBACK OF CURRENT FREQUENCY FOR ALL INTENSITIES:
sig = NaN(length(t),length(peaks));
for ic = 1:length(peaks),

    % generate sinusoidal signal:
    sig1 = peaks(ic) * zeros(size(t2));  % signal without ramps
    
    % apply compensatory IR:
    fsig1 = filter(irc,1,sig1);
    
    % cut-out a shorter part from fsig1, starting at the end, to
    % exclude the initial attack time caused by the filter:
    fsig1 = fsig1(end-0.01*FS-dur*FS+1 : end-0.01*FS);
    
    % apply raised cosine ramps:
    tr = linspace(0, pi, ramp_dur*FS);
    upramp = (-1*cos(tr)+1)/2;
    downramp = fliplr(upramp);
    fsig1(1:length(upramp)) =  fsig1(1:length(upramp)).* upramp;
    fsig1(end-length(downramp)+1 : end) = fsig1(end-length(downramp)+1 : end) .* downramp;
    
    % add fsig1 to sig:
    sig(:,ic) = col(fsig1);
end

% start video, play signal, retrieve behavioural data, stop video:
freq_str = sprintf('%02.0f',freq);         % data into string with two leading figures (in front of the decimal point)
vidfilename{1} = [data_pn prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str '_Cam01.avi'];  % definition of file name
vidfilename{2} = [data_pn prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str '_Cam02.avi'];
[recsig, xruns] = smp_playrec_audiogram(sig,FS,sildur, vidfilename);
disp(' ');
disp(['BA1: Finished smp_playrec_audiogram (Silence). size(recsig) = ' num2str(size(recsig)) ', dur(recsig) = ' num2str(length(recsig)/FS) ' sec, # xruns = ' num2str(xruns)]);


% save measured moth behavioural data as WAV-file:
 wav_fn = [prot_str '_' freq_str '-kHz_' spec_str '_' indnum_str '_' date_str];
% mic-recording. Assuming that we use the full dynamic range of the 
% Fireface Input of up to 1.78 Vpk, we divide the mic-recording by 2 to
% come within the range of +/-1 that can be saved as wav-file:
mic_rec = recsig(:,1) / 2;
% speaker recordings:
sp_rec = recsig(:,2:3);
if max(max(abs(sp_rec))) > 0.99,    % to check for cliping
    f = max([2, ceil(max(max(abs(sp_rec)))/0.99)]);
    sp_rec = sp_rec./f;
    fstr = sprintf('%02.0f',f);
    wav_fn = [wav_fn '_DIV' fstr];  % adds value of division to the filename
end
audiowrite([data_pn wav_fn '.wav'], [sp_rec, mic_rec] ,FS, 'BitsPerSample',16);

% save meta trial data:
counter = size(results,1) + 1;
trial_info = [counter, date_num/1e5, indnum, prot_num, NaN];
results = [results; trial_info];
save([data_pn '\results_' spec_str '_Ind' indnum_str '.mat'], 'results','freqs','freq_order','freq_counter');
disp('BA1: All Data (Video, Audio + Meta) Saved!');


% Plot recorded data:
t = linspace(1/FS, length(sp_rec)/FS, length(sp_rec));
figure(2)
subplot(3,1,1);
plot(t,mic_rec, 'k');
ylabel('mic (arb. lin. units)')

subplot(3,1,2);
plot(t,sp_rec(:,1), 'r');   % speaker 1 = Left
ylabel({'speaker 1 = LEFT';'(arb. lin. units)'})

subplot(3,1,3);
plot(t,sp_rec(:,2), 'b');   % speaker 2 = Right
xlabel('time (sec)');
ylabel({'speaker 2 = RIGHT';'(arb. lin. units)'})


disp(' ');
disp('BA1: FINISHED WITH SILENCE CONTROL!');
disp(' ');
disp('=========================================================');
disp('=========================================================');
disp(' ');


% SET GUI & PARAMETERS:
set(handles.push_start, 'Enable','on');        % activate Start Button
set(handles.popup_spec, 'Enable','on');        % activate Species Selection
set(handles.popup_protocol, 'Enable','on');    % activate Protocol Selection
set(handles.push_stop, 'Enable','off');          % deactivate Stop Button
newmessage(hObject, handles, {'===== BA1: SILENCE CONTROL PRESENTED to this Moth! =====';' '});
handles = guidata(hObject);



%% NEWMESSAGE =============================================================
function newmessage(hObject, handles, new_message)

handles.messages = [handles.messages; new_message];
if length(handles.messages) > 35,
    handles.messages = handles.messages(end-24:end);
end
set(handles.static_messages, 'string',handles.messages);

guidata(hObject, handles);  % update handles structure
pause(0.01);
drawnow
pause(0.01);






% --- Executes on selection change in popupmenu3.
function popupmenu3_Callback(hObject, eventdata, handles)
% hObject    handle to popupmenu3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns popupmenu3 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from popupmenu3


% --- Executes during object creation, after setting all properties.
function popupmenu3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popupmenu3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
