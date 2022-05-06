                      %% Results: UG2 BEH -- IC vs NC %%
% Last update: 5/5/2022
% Orginal Author: Soojung Na
% Adadapted by: Sarah Banker

                            %%% CONTENT %%%
% 1. Input file
% 2. Output file
% 3. Summary of beh (M)
% 4. Stats (IC vs NC)
% 5. Save
clear
close all

%% 1. Input file
% rawdata (beh matlab files)
indir = '/Users/sarahbanker/Documents/SinaiYear3/UG_Analysis/0.data';
infile = 'beh_noFlat_1342.mat';
load(fullfile(indir, infile));


%% 2. Output file
outdir = '/Users/sarahbanker/Documents/SinaiYear3/UG_Analysis/1.beh';
outfile = 'results.mat';


%% 3. Summary of beh (M)
nP = length(A);
for i = 1:nP
    for c = 1:length(C)
        
        oo = A(i).(C{c}).offer; % responded offers        
        xLO = find(oo==1 | oo==2 | oo==3);
        xMO = find(oo==4 | oo==5 | oo==6);
        xHO = find(oo==7 | oo==8 | oo==9);

        ID{i,1} = A(i).ID;
        offer(i, c)  = mean(A(i).(C{c}).offer);
        rejR(i, c)   = 1 - mean(A(i).(C{c}).choice);
        rejR_L(i, c) = 1 - mean(A(i).(C{c}).choice(xLO));
        rejR_M(i, c) = 1 - mean(A(i).(C{c}).choice(xMO));
        rejR_H(i, c) = 1 - mean(A(i).(C{c}).choice(xHO));
        reward(i, c) = mean(A(i).(C{c}).reward);
        emo(i, c)    = nanmean(A(i).(C{c}).emo);                        
        pc(i, c)     = A(i).(C{c}).pc;                                               
    end
end

Mname = {'offer' 'rejR' 'rejR_L' 'rejR_M' 'rejR_H' 'reward' 'emo' 'pc'};
M = [offer rejR rejR_L rejR_M rejR_H reward emo pc];
M_mean = nanmean(M(1:length(A), :));
M_std = nanstd(M(1:length(A), :));


%% 4. Stats (IC vs NC)
%for v = 1:length(Mname)
%    Y_IC(:, v) = M(:, 2*v-1);
%    Y_NC(:, v) = M(:, 2*v);
%end
%stat_ICvNC = stat_compare2cond(Y_IC, Y_NC, Mname);



%% 5. Save
cd(outdir);
save(outfile, 'ID', 'Mname', 'M', 'M_mean', 'M_std');
% include 'stat_ICvNC' in save if its run
clear;
