data = xlsread('data_full.csv');
data_minor = xlsread('data_minor.csv');
distance_var = xlsread('distance.csv');
distance_varX = zeros(8,8,11);
[n,p] = size(data);
[n_minor,~] = size(data_minor);

for i = 1:(p-1)
    distance_varX(:,:,i) = distance_var(:,((i-1)*8)+1:i*8);
end

KNN_data =  zeros(n_minor,100);
KNN_index =  zeros(n_minor,100);
for i = 1:n_minor
  data_distance = zeros(n,1);
  for j = 1:n
    for k = 1:(p-1)
      data_distance(j) = data_distance(j) + distance_varX(data_minor(i,k),data(j,k),k);                              
    end
    data_distance(j) = sqrt(data_distance(j));
  end
  [x,y] = sort(data_distance);
  KNN_data(i,:) = x(1:100);
  KNN_index(i,:) = y(1:100);
  fprintf('%f %% \n',i/n_minor*100);
end

[~,y] = sort(data(:,p));
index_minor = y(1:n_minor);
proportional = zeros(n_minor,1);
k = 5;
for i = 1:n_minor
    x = 0;
    for j = 1:k
        %Ubah bagian untuk modfied ADASYN-N
        if((data(KNN_index(i,j+1),p)==1))
            x = x + 1;
        end
        %if(KNN_index(i,j)==index_minor(i))
        %    if((data(KNN_index(i,k+1),p)==1))
        %        x = x + 1;
        %    end
        %end
    end
    proportional(i) = x/k;
    fprintf('%f %% \n',i/n_minor*100);
end
sum_pro = proportional / sum(proportional);

n25 = ceil((560781 - (2*79029))*25/100);
n50 = ceil((560781 - (2*79029))*50/100);
n75 = ceil((560781 - (2*79029))*75/100);
n100 = ceil((560781 - (2*79029)));

pro25 = round(sum_pro * n25);
pro50 = round(sum_pro * n50);
pro75 = round(sum_pro * n75);
pro100 = round(sum_pro * n100);
