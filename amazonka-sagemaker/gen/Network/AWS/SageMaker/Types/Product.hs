{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.Sum

-- | Specifies the training algorithm to use in a <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateTrainingJob.html CreateTrainingJob> request.
--
--
-- For more information about algorithms provided by Amazon SageMaker, see <http://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about using your own algorithms, see 'your-algorithms' .
--
--
-- /See:/ 'algorithmSpecification' smart constructor.
data AlgorithmSpecification = AlgorithmSpecification'
  { _asTrainingImage     :: !Text
  , _asTrainingInputMode :: !TrainingInputMode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AlgorithmSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asTrainingImage' - The registry path of the Docker image that contains the training algorithm. For information about docker registry paths for built-in algorithms, see 'sagemaker-algo-docker-registry-paths' .
--
-- * 'asTrainingInputMode' - The input mode that the algorithm supports. For the input modes that Amazon SageMaker algorithms support, see <http://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.  In File mode, make sure you provision ML storage volume with sufficient capacity to accommodate the data download from S3. In addition to the training data, the ML storage volume also stores the output model. The algorithm container use ML storage volume to also store intermediate information, if any.  For distributed algorithms using File mode, training data is distributed uniformly, and your training duration is predictable if the input data objects size is approximately same. Amazon SageMaker does not split the files any further for model training. If the object sizes are skewed, training won't be optimal as the data distribution is also skewed where one host in a training cluster is overloaded, thus becoming bottleneck in training.
algorithmSpecification
    :: Text -- ^ 'asTrainingImage'
    -> TrainingInputMode -- ^ 'asTrainingInputMode'
    -> AlgorithmSpecification
algorithmSpecification pTrainingImage_ pTrainingInputMode_ =
  AlgorithmSpecification'
    { _asTrainingImage = pTrainingImage_
    , _asTrainingInputMode = pTrainingInputMode_
    }


-- | The registry path of the Docker image that contains the training algorithm. For information about docker registry paths for built-in algorithms, see 'sagemaker-algo-docker-registry-paths' .
asTrainingImage :: Lens' AlgorithmSpecification Text
asTrainingImage = lens _asTrainingImage (\ s a -> s{_asTrainingImage = a})

-- | The input mode that the algorithm supports. For the input modes that Amazon SageMaker algorithms support, see <http://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.  In File mode, make sure you provision ML storage volume with sufficient capacity to accommodate the data download from S3. In addition to the training data, the ML storage volume also stores the output model. The algorithm container use ML storage volume to also store intermediate information, if any.  For distributed algorithms using File mode, training data is distributed uniformly, and your training duration is predictable if the input data objects size is approximately same. Amazon SageMaker does not split the files any further for model training. If the object sizes are skewed, training won't be optimal as the data distribution is also skewed where one host in a training cluster is overloaded, thus becoming bottleneck in training.
asTrainingInputMode :: Lens' AlgorithmSpecification TrainingInputMode
asTrainingInputMode = lens _asTrainingInputMode (\ s a -> s{_asTrainingInputMode = a})

instance FromJSON AlgorithmSpecification where
        parseJSON
          = withObject "AlgorithmSpecification"
              (\ x ->
                 AlgorithmSpecification' <$>
                   (x .: "TrainingImage") <*>
                     (x .: "TrainingInputMode"))

instance Hashable AlgorithmSpecification where

instance NFData AlgorithmSpecification where

instance ToJSON AlgorithmSpecification where
        toJSON AlgorithmSpecification'{..}
          = object
              (catMaybes
                 [Just ("TrainingImage" .= _asTrainingImage),
                  Just ("TrainingInputMode" .= _asTrainingInputMode)])

-- | A channel is a named input source that training algorithms can consume.
--
--
--
-- /See:/ 'channel' smart constructor.
data Channel = Channel'
  { _cRecordWrapperType :: !(Maybe RecordWrapper)
  , _cCompressionType   :: !(Maybe CompressionType)
  , _cContentType       :: !(Maybe Text)
  , _cChannelName       :: !Text
  , _cDataSource        :: !DataSource
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cRecordWrapperType' -  Specify RecordIO as the value when input data is in raw format but the training algorithm requires the RecordIO format, in which caseAmazon SageMaker wraps each individual S3 object in a RecordIO record. If the input data is already in RecordIO format, you don't need to set this attribute. For more information, see <https://mxnet.incubator.apache.org/how_to/recordio.html?highlight=im2rec Create a Dataset Using RecordIO> .  In FILE mode, leave this field unset or set it to None.
--
-- * 'cCompressionType' - If training data is compressed, the compression type. The default value is @None@ . @CompressionType@ is used only in PIPE input mode. In FILE mode, leave this field unset or set it to None.
--
-- * 'cContentType' - The MIME type of the data.
--
-- * 'cChannelName' - The name of the channel.
--
-- * 'cDataSource' - The location of the channel data.
channel
    :: Text -- ^ 'cChannelName'
    -> DataSource -- ^ 'cDataSource'
    -> Channel
channel pChannelName_ pDataSource_ =
  Channel'
    { _cRecordWrapperType = Nothing
    , _cCompressionType = Nothing
    , _cContentType = Nothing
    , _cChannelName = pChannelName_
    , _cDataSource = pDataSource_
    }


-- |  Specify RecordIO as the value when input data is in raw format but the training algorithm requires the RecordIO format, in which caseAmazon SageMaker wraps each individual S3 object in a RecordIO record. If the input data is already in RecordIO format, you don't need to set this attribute. For more information, see <https://mxnet.incubator.apache.org/how_to/recordio.html?highlight=im2rec Create a Dataset Using RecordIO> .  In FILE mode, leave this field unset or set it to None.
cRecordWrapperType :: Lens' Channel (Maybe RecordWrapper)
cRecordWrapperType = lens _cRecordWrapperType (\ s a -> s{_cRecordWrapperType = a})

-- | If training data is compressed, the compression type. The default value is @None@ . @CompressionType@ is used only in PIPE input mode. In FILE mode, leave this field unset or set it to None.
cCompressionType :: Lens' Channel (Maybe CompressionType)
cCompressionType = lens _cCompressionType (\ s a -> s{_cCompressionType = a})

-- | The MIME type of the data.
cContentType :: Lens' Channel (Maybe Text)
cContentType = lens _cContentType (\ s a -> s{_cContentType = a})

-- | The name of the channel.
cChannelName :: Lens' Channel Text
cChannelName = lens _cChannelName (\ s a -> s{_cChannelName = a})

-- | The location of the channel data.
cDataSource :: Lens' Channel DataSource
cDataSource = lens _cDataSource (\ s a -> s{_cDataSource = a})

instance FromJSON Channel where
        parseJSON
          = withObject "Channel"
              (\ x ->
                 Channel' <$>
                   (x .:? "RecordWrapperType") <*>
                     (x .:? "CompressionType")
                     <*> (x .:? "ContentType")
                     <*> (x .: "ChannelName")
                     <*> (x .: "DataSource"))

instance Hashable Channel where

instance NFData Channel where

instance ToJSON Channel where
        toJSON Channel'{..}
          = object
              (catMaybes
                 [("RecordWrapperType" .=) <$> _cRecordWrapperType,
                  ("CompressionType" .=) <$> _cCompressionType,
                  ("ContentType" .=) <$> _cContentType,
                  Just ("ChannelName" .= _cChannelName),
                  Just ("DataSource" .= _cDataSource)])

-- | Describes the container, as part of model definition.
--
--
--
-- /See:/ 'containerDefinition' smart constructor.
data ContainerDefinition = ContainerDefinition'
  { _cdModelDataURL      :: !(Maybe Text)
  , _cdEnvironment       :: !(Maybe (Map Text Text))
  , _cdContainerHostname :: !(Maybe Text)
  , _cdImage             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContainerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdModelDataURL' - The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix).
--
-- * 'cdEnvironment' - The environment variables to set in the Docker container. Each key and value in the @Environment@ string to string map can have length of up to 1024. We support up to 16 entries in the map.
--
-- * 'cdContainerHostname' - The DNS host name for the container after Amazon SageMaker deploys it.
--
-- * 'cdImage' - The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
containerDefinition
    :: Text -- ^ 'cdImage'
    -> ContainerDefinition
containerDefinition pImage_ =
  ContainerDefinition'
    { _cdModelDataURL = Nothing
    , _cdEnvironment = Nothing
    , _cdContainerHostname = Nothing
    , _cdImage = pImage_
    }


-- | The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix).
cdModelDataURL :: Lens' ContainerDefinition (Maybe Text)
cdModelDataURL = lens _cdModelDataURL (\ s a -> s{_cdModelDataURL = a})

-- | The environment variables to set in the Docker container. Each key and value in the @Environment@ string to string map can have length of up to 1024. We support up to 16 entries in the map.
cdEnvironment :: Lens' ContainerDefinition (HashMap Text Text)
cdEnvironment = lens _cdEnvironment (\ s a -> s{_cdEnvironment = a}) . _Default . _Map

-- | The DNS host name for the container after Amazon SageMaker deploys it.
cdContainerHostname :: Lens' ContainerDefinition (Maybe Text)
cdContainerHostname = lens _cdContainerHostname (\ s a -> s{_cdContainerHostname = a})

-- | The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
cdImage :: Lens' ContainerDefinition Text
cdImage = lens _cdImage (\ s a -> s{_cdImage = a})

instance FromJSON ContainerDefinition where
        parseJSON
          = withObject "ContainerDefinition"
              (\ x ->
                 ContainerDefinition' <$>
                   (x .:? "ModelDataUrl") <*>
                     (x .:? "Environment" .!= mempty)
                     <*> (x .:? "ContainerHostname")
                     <*> (x .: "Image"))

instance Hashable ContainerDefinition where

instance NFData ContainerDefinition where

instance ToJSON ContainerDefinition where
        toJSON ContainerDefinition'{..}
          = object
              (catMaybes
                 [("ModelDataUrl" .=) <$> _cdModelDataURL,
                  ("Environment" .=) <$> _cdEnvironment,
                  ("ContainerHostname" .=) <$> _cdContainerHostname,
                  Just ("Image" .= _cdImage)])

-- | Describes the location of the channel data.
--
--
--
-- /See:/ 'dataSource' smart constructor.
newtype DataSource = DataSource'
  { _dsS3DataSource :: S3DataSource
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsS3DataSource' - The S3 location of the data source that is associated with a channel.
dataSource
    :: S3DataSource -- ^ 'dsS3DataSource'
    -> DataSource
dataSource pS3DataSource_ = DataSource' {_dsS3DataSource = pS3DataSource_}


-- | The S3 location of the data source that is associated with a channel.
dsS3DataSource :: Lens' DataSource S3DataSource
dsS3DataSource = lens _dsS3DataSource (\ s a -> s{_dsS3DataSource = a})

instance FromJSON DataSource where
        parseJSON
          = withObject "DataSource"
              (\ x -> DataSource' <$> (x .: "S3DataSource"))

instance Hashable DataSource where

instance NFData DataSource where

instance ToJSON DataSource where
        toJSON DataSource'{..}
          = object
              (catMaybes
                 [Just ("S3DataSource" .= _dsS3DataSource)])

-- | Specifies weight and capacity values for a production variant.
--
--
--
-- /See:/ 'desiredWeightAndCapacity' smart constructor.
data DesiredWeightAndCapacity = DesiredWeightAndCapacity'
  { _dwacDesiredInstanceCount :: !(Maybe Nat)
  , _dwacDesiredWeight        :: !(Maybe Double)
  , _dwacVariantName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DesiredWeightAndCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwacDesiredInstanceCount' - The variant's capacity.
--
-- * 'dwacDesiredWeight' - The variant's weight.
--
-- * 'dwacVariantName' - The name of the variant to update.
desiredWeightAndCapacity
    :: Text -- ^ 'dwacVariantName'
    -> DesiredWeightAndCapacity
desiredWeightAndCapacity pVariantName_ =
  DesiredWeightAndCapacity'
    { _dwacDesiredInstanceCount = Nothing
    , _dwacDesiredWeight = Nothing
    , _dwacVariantName = pVariantName_
    }


-- | The variant's capacity.
dwacDesiredInstanceCount :: Lens' DesiredWeightAndCapacity (Maybe Natural)
dwacDesiredInstanceCount = lens _dwacDesiredInstanceCount (\ s a -> s{_dwacDesiredInstanceCount = a}) . mapping _Nat

-- | The variant's weight.
dwacDesiredWeight :: Lens' DesiredWeightAndCapacity (Maybe Double)
dwacDesiredWeight = lens _dwacDesiredWeight (\ s a -> s{_dwacDesiredWeight = a})

-- | The name of the variant to update.
dwacVariantName :: Lens' DesiredWeightAndCapacity Text
dwacVariantName = lens _dwacVariantName (\ s a -> s{_dwacVariantName = a})

instance Hashable DesiredWeightAndCapacity where

instance NFData DesiredWeightAndCapacity where

instance ToJSON DesiredWeightAndCapacity where
        toJSON DesiredWeightAndCapacity'{..}
          = object
              (catMaybes
                 [("DesiredInstanceCount" .=) <$>
                    _dwacDesiredInstanceCount,
                  ("DesiredWeight" .=) <$> _dwacDesiredWeight,
                  Just ("VariantName" .= _dwacVariantName)])

-- | Provides summary information for an endpoint configuration.
--
--
--
-- /See:/ 'endpointConfigSummary' smart constructor.
data EndpointConfigSummary = EndpointConfigSummary'
  { _ecsEndpointConfigName :: !Text
  , _ecsEndpointConfigARN  :: !Text
  , _ecsCreationTime       :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndpointConfigSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecsEndpointConfigName' - The name of the endpoint configuration.
--
-- * 'ecsEndpointConfigARN' - The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- * 'ecsCreationTime' - A timestamp that shows when the endpoint configuration was created.
endpointConfigSummary
    :: Text -- ^ 'ecsEndpointConfigName'
    -> Text -- ^ 'ecsEndpointConfigARN'
    -> UTCTime -- ^ 'ecsCreationTime'
    -> EndpointConfigSummary
endpointConfigSummary pEndpointConfigName_ pEndpointConfigARN_ pCreationTime_ =
  EndpointConfigSummary'
    { _ecsEndpointConfigName = pEndpointConfigName_
    , _ecsEndpointConfigARN = pEndpointConfigARN_
    , _ecsCreationTime = _Time # pCreationTime_
    }


-- | The name of the endpoint configuration.
ecsEndpointConfigName :: Lens' EndpointConfigSummary Text
ecsEndpointConfigName = lens _ecsEndpointConfigName (\ s a -> s{_ecsEndpointConfigName = a})

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
ecsEndpointConfigARN :: Lens' EndpointConfigSummary Text
ecsEndpointConfigARN = lens _ecsEndpointConfigARN (\ s a -> s{_ecsEndpointConfigARN = a})

-- | A timestamp that shows when the endpoint configuration was created.
ecsCreationTime :: Lens' EndpointConfigSummary UTCTime
ecsCreationTime = lens _ecsCreationTime (\ s a -> s{_ecsCreationTime = a}) . _Time

instance FromJSON EndpointConfigSummary where
        parseJSON
          = withObject "EndpointConfigSummary"
              (\ x ->
                 EndpointConfigSummary' <$>
                   (x .: "EndpointConfigName") <*>
                     (x .: "EndpointConfigArn")
                     <*> (x .: "CreationTime"))

instance Hashable EndpointConfigSummary where

instance NFData EndpointConfigSummary where

-- | Provides summary information for an endpoint.
--
--
--
-- /See:/ 'endpointSummary' smart constructor.
data EndpointSummary = EndpointSummary'
  { _esEndpointName     :: !Text
  , _esEndpointARN      :: !Text
  , _esCreationTime     :: !POSIX
  , _esLastModifiedTime :: !POSIX
  , _esEndpointStatus   :: !EndpointStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndpointSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esEndpointName' - The name of the endpoint.
--
-- * 'esEndpointARN' - The Amazon Resource Name (ARN) of the endpoint.
--
-- * 'esCreationTime' - A timestamp that shows when the endpoint was created.
--
-- * 'esLastModifiedTime' - A timestamp that shows when the endpoint was last modified.
--
-- * 'esEndpointStatus' - The status of the endpoint.
endpointSummary
    :: Text -- ^ 'esEndpointName'
    -> Text -- ^ 'esEndpointARN'
    -> UTCTime -- ^ 'esCreationTime'
    -> UTCTime -- ^ 'esLastModifiedTime'
    -> EndpointStatus -- ^ 'esEndpointStatus'
    -> EndpointSummary
endpointSummary pEndpointName_ pEndpointARN_ pCreationTime_ pLastModifiedTime_ pEndpointStatus_ =
  EndpointSummary'
    { _esEndpointName = pEndpointName_
    , _esEndpointARN = pEndpointARN_
    , _esCreationTime = _Time # pCreationTime_
    , _esLastModifiedTime = _Time # pLastModifiedTime_
    , _esEndpointStatus = pEndpointStatus_
    }


-- | The name of the endpoint.
esEndpointName :: Lens' EndpointSummary Text
esEndpointName = lens _esEndpointName (\ s a -> s{_esEndpointName = a})

-- | The Amazon Resource Name (ARN) of the endpoint.
esEndpointARN :: Lens' EndpointSummary Text
esEndpointARN = lens _esEndpointARN (\ s a -> s{_esEndpointARN = a})

-- | A timestamp that shows when the endpoint was created.
esCreationTime :: Lens' EndpointSummary UTCTime
esCreationTime = lens _esCreationTime (\ s a -> s{_esCreationTime = a}) . _Time

-- | A timestamp that shows when the endpoint was last modified.
esLastModifiedTime :: Lens' EndpointSummary UTCTime
esLastModifiedTime = lens _esLastModifiedTime (\ s a -> s{_esLastModifiedTime = a}) . _Time

-- | The status of the endpoint.
esEndpointStatus :: Lens' EndpointSummary EndpointStatus
esEndpointStatus = lens _esEndpointStatus (\ s a -> s{_esEndpointStatus = a})

instance FromJSON EndpointSummary where
        parseJSON
          = withObject "EndpointSummary"
              (\ x ->
                 EndpointSummary' <$>
                   (x .: "EndpointName") <*> (x .: "EndpointArn") <*>
                     (x .: "CreationTime")
                     <*> (x .: "LastModifiedTime")
                     <*> (x .: "EndpointStatus"))

instance Hashable EndpointSummary where

instance NFData EndpointSummary where

-- | Provides information about the location that is configured for storing model artifacts.
--
--
--
-- /See:/ 'modelArtifacts' smart constructor.
newtype ModelArtifacts = ModelArtifacts'
  { _maS3ModelArtifacts :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModelArtifacts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maS3ModelArtifacts' - The path of the S3 object that contains the model artifacts. For example, @s3://bucket-name/keynameprefix/model.tar.gz@ .
modelArtifacts
    :: Text -- ^ 'maS3ModelArtifacts'
    -> ModelArtifacts
modelArtifacts pS3ModelArtifacts_ =
  ModelArtifacts' {_maS3ModelArtifacts = pS3ModelArtifacts_}


-- | The path of the S3 object that contains the model artifacts. For example, @s3://bucket-name/keynameprefix/model.tar.gz@ .
maS3ModelArtifacts :: Lens' ModelArtifacts Text
maS3ModelArtifacts = lens _maS3ModelArtifacts (\ s a -> s{_maS3ModelArtifacts = a})

instance FromJSON ModelArtifacts where
        parseJSON
          = withObject "ModelArtifacts"
              (\ x ->
                 ModelArtifacts' <$> (x .: "S3ModelArtifacts"))

instance Hashable ModelArtifacts where

instance NFData ModelArtifacts where

-- | Provides summary information about a model.
--
--
--
-- /See:/ 'modelSummary' smart constructor.
data ModelSummary = ModelSummary'
  { _msModelName    :: !Text
  , _msModelARN     :: !Text
  , _msCreationTime :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModelSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msModelName' - The name of the model that you want a summary for.
--
-- * 'msModelARN' - The Amazon Resource Name (ARN) of the model.
--
-- * 'msCreationTime' - A timestamp that indicates when the model was created.
modelSummary
    :: Text -- ^ 'msModelName'
    -> Text -- ^ 'msModelARN'
    -> UTCTime -- ^ 'msCreationTime'
    -> ModelSummary
modelSummary pModelName_ pModelARN_ pCreationTime_ =
  ModelSummary'
    { _msModelName = pModelName_
    , _msModelARN = pModelARN_
    , _msCreationTime = _Time # pCreationTime_
    }


-- | The name of the model that you want a summary for.
msModelName :: Lens' ModelSummary Text
msModelName = lens _msModelName (\ s a -> s{_msModelName = a})

-- | The Amazon Resource Name (ARN) of the model.
msModelARN :: Lens' ModelSummary Text
msModelARN = lens _msModelARN (\ s a -> s{_msModelARN = a})

-- | A timestamp that indicates when the model was created.
msCreationTime :: Lens' ModelSummary UTCTime
msCreationTime = lens _msCreationTime (\ s a -> s{_msCreationTime = a}) . _Time

instance FromJSON ModelSummary where
        parseJSON
          = withObject "ModelSummary"
              (\ x ->
                 ModelSummary' <$>
                   (x .: "ModelName") <*> (x .: "ModelArn") <*>
                     (x .: "CreationTime"))

instance Hashable ModelSummary where

instance NFData ModelSummary where

-- | Provides a summary of a notebook instance lifecycle configuration.
--
--
--
-- /See:/ 'notebookInstanceLifecycleConfigSummary' smart constructor.
data NotebookInstanceLifecycleConfigSummary = NotebookInstanceLifecycleConfigSummary'
  { _nilcsCreationTime                        :: !(Maybe POSIX)
  , _nilcsLastModifiedTime                    :: !(Maybe POSIX)
  , _nilcsNotebookInstanceLifecycleConfigName :: !Text
  , _nilcsNotebookInstanceLifecycleConfigARN  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotebookInstanceLifecycleConfigSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nilcsCreationTime' - A timestamp that tells when the lifecycle configuration was created.
--
-- * 'nilcsLastModifiedTime' - A timestamp that tells when the lifecycle configuration was last modified.
--
-- * 'nilcsNotebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
--
-- * 'nilcsNotebookInstanceLifecycleConfigARN' - The Amazon Resource Name (ARN) of the lifecycle configuration.
notebookInstanceLifecycleConfigSummary
    :: Text -- ^ 'nilcsNotebookInstanceLifecycleConfigName'
    -> Text -- ^ 'nilcsNotebookInstanceLifecycleConfigARN'
    -> NotebookInstanceLifecycleConfigSummary
notebookInstanceLifecycleConfigSummary pNotebookInstanceLifecycleConfigName_ pNotebookInstanceLifecycleConfigARN_ =
  NotebookInstanceLifecycleConfigSummary'
    { _nilcsCreationTime = Nothing
    , _nilcsLastModifiedTime = Nothing
    , _nilcsNotebookInstanceLifecycleConfigName =
        pNotebookInstanceLifecycleConfigName_
    , _nilcsNotebookInstanceLifecycleConfigARN =
        pNotebookInstanceLifecycleConfigARN_
    }


-- | A timestamp that tells when the lifecycle configuration was created.
nilcsCreationTime :: Lens' NotebookInstanceLifecycleConfigSummary (Maybe UTCTime)
nilcsCreationTime = lens _nilcsCreationTime (\ s a -> s{_nilcsCreationTime = a}) . mapping _Time

-- | A timestamp that tells when the lifecycle configuration was last modified.
nilcsLastModifiedTime :: Lens' NotebookInstanceLifecycleConfigSummary (Maybe UTCTime)
nilcsLastModifiedTime = lens _nilcsLastModifiedTime (\ s a -> s{_nilcsLastModifiedTime = a}) . mapping _Time

-- | The name of the lifecycle configuration.
nilcsNotebookInstanceLifecycleConfigName :: Lens' NotebookInstanceLifecycleConfigSummary Text
nilcsNotebookInstanceLifecycleConfigName = lens _nilcsNotebookInstanceLifecycleConfigName (\ s a -> s{_nilcsNotebookInstanceLifecycleConfigName = a})

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
nilcsNotebookInstanceLifecycleConfigARN :: Lens' NotebookInstanceLifecycleConfigSummary Text
nilcsNotebookInstanceLifecycleConfigARN = lens _nilcsNotebookInstanceLifecycleConfigARN (\ s a -> s{_nilcsNotebookInstanceLifecycleConfigARN = a})

instance FromJSON
           NotebookInstanceLifecycleConfigSummary
         where
        parseJSON
          = withObject "NotebookInstanceLifecycleConfigSummary"
              (\ x ->
                 NotebookInstanceLifecycleConfigSummary' <$>
                   (x .:? "CreationTime") <*> (x .:? "LastModifiedTime")
                     <*> (x .: "NotebookInstanceLifecycleConfigName")
                     <*> (x .: "NotebookInstanceLifecycleConfigArn"))

instance Hashable
           NotebookInstanceLifecycleConfigSummary
         where

instance NFData
           NotebookInstanceLifecycleConfigSummary
         where

-- | Contains the notebook instance lifecycle configuration script.
--
--
-- Each lifecycle configuration script has a limit of 16384 characters.
--
-- The value of the @> PATH@ environment variable that is available to both scripts is @/sbin:bin:/usr/sbin:/usr/bin@ .
--
-- View CloudWatch Logs for notebook instance lifecycle configurations in log group @/aws/sagemaker/NotebookInstances@ in log stream @[notebook-instance-name]/[LifecycleConfigHook]@ .
--
-- Lifecycle configuration scripts cannot run for longer than 5 minutes. If a script runs for longer than 5 minutes, it fails and the notebook instance is not created or started.
--
-- For information about notebook instance lifestyle configurations, see 'notebook-lifecycle-config' .
--
--
-- /See:/ 'notebookInstanceLifecycleHook' smart constructor.
newtype NotebookInstanceLifecycleHook = NotebookInstanceLifecycleHook'
  { _nilhContent :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotebookInstanceLifecycleHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nilhContent' - A base64-encoded string that contains a shell script for a notebook instance lifecycle configuration.
notebookInstanceLifecycleHook
    :: NotebookInstanceLifecycleHook
notebookInstanceLifecycleHook =
  NotebookInstanceLifecycleHook' {_nilhContent = Nothing}


-- | A base64-encoded string that contains a shell script for a notebook instance lifecycle configuration.
nilhContent :: Lens' NotebookInstanceLifecycleHook (Maybe Text)
nilhContent = lens _nilhContent (\ s a -> s{_nilhContent = a})

instance FromJSON NotebookInstanceLifecycleHook where
        parseJSON
          = withObject "NotebookInstanceLifecycleHook"
              (\ x ->
                 NotebookInstanceLifecycleHook' <$> (x .:? "Content"))

instance Hashable NotebookInstanceLifecycleHook where

instance NFData NotebookInstanceLifecycleHook where

instance ToJSON NotebookInstanceLifecycleHook where
        toJSON NotebookInstanceLifecycleHook'{..}
          = object
              (catMaybes [("Content" .=) <$> _nilhContent])

-- | Provides summary information for an Amazon SageMaker notebook instance.
--
--
--
-- /See:/ 'notebookInstanceSummary' smart constructor.
data NotebookInstanceSummary = NotebookInstanceSummary'
  { _nisCreationTime                        :: !(Maybe POSIX)
  , _nisURL                                 :: !(Maybe Text)
  , _nisLastModifiedTime                    :: !(Maybe POSIX)
  , _nisInstanceType                        :: !(Maybe InstanceType)
  , _nisNotebookInstanceStatus              :: !(Maybe NotebookInstanceStatus)
  , _nisNotebookInstanceLifecycleConfigName :: !(Maybe Text)
  , _nisNotebookInstanceName                :: !Text
  , _nisNotebookInstanceARN                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotebookInstanceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nisCreationTime' - A timestamp that shows when the notebook instance was created.
--
-- * 'nisURL' - The URL that you use to connect to the Jupyter instance running in your notebook instance.
--
-- * 'nisLastModifiedTime' - A timestamp that shows when the notebook instance was last modified.
--
-- * 'nisInstanceType' - The type of ML compute instance that the notebook instance is running on.
--
-- * 'nisNotebookInstanceStatus' - The status of the notebook instance.
--
-- * 'nisNotebookInstanceLifecycleConfigName' - The name of a notebook instance lifecycle configuration associated with this notebook instance. For information about notebook instance lifestyle configurations, see 'notebook-lifecycle-config' .
--
-- * 'nisNotebookInstanceName' - The name of the notebook instance that you want a summary for.
--
-- * 'nisNotebookInstanceARN' - The Amazon Resource Name (ARN) of the notebook instance.
notebookInstanceSummary
    :: Text -- ^ 'nisNotebookInstanceName'
    -> Text -- ^ 'nisNotebookInstanceARN'
    -> NotebookInstanceSummary
notebookInstanceSummary pNotebookInstanceName_ pNotebookInstanceARN_ =
  NotebookInstanceSummary'
    { _nisCreationTime = Nothing
    , _nisURL = Nothing
    , _nisLastModifiedTime = Nothing
    , _nisInstanceType = Nothing
    , _nisNotebookInstanceStatus = Nothing
    , _nisNotebookInstanceLifecycleConfigName = Nothing
    , _nisNotebookInstanceName = pNotebookInstanceName_
    , _nisNotebookInstanceARN = pNotebookInstanceARN_
    }


-- | A timestamp that shows when the notebook instance was created.
nisCreationTime :: Lens' NotebookInstanceSummary (Maybe UTCTime)
nisCreationTime = lens _nisCreationTime (\ s a -> s{_nisCreationTime = a}) . mapping _Time

-- | The URL that you use to connect to the Jupyter instance running in your notebook instance.
nisURL :: Lens' NotebookInstanceSummary (Maybe Text)
nisURL = lens _nisURL (\ s a -> s{_nisURL = a})

-- | A timestamp that shows when the notebook instance was last modified.
nisLastModifiedTime :: Lens' NotebookInstanceSummary (Maybe UTCTime)
nisLastModifiedTime = lens _nisLastModifiedTime (\ s a -> s{_nisLastModifiedTime = a}) . mapping _Time

-- | The type of ML compute instance that the notebook instance is running on.
nisInstanceType :: Lens' NotebookInstanceSummary (Maybe InstanceType)
nisInstanceType = lens _nisInstanceType (\ s a -> s{_nisInstanceType = a})

-- | The status of the notebook instance.
nisNotebookInstanceStatus :: Lens' NotebookInstanceSummary (Maybe NotebookInstanceStatus)
nisNotebookInstanceStatus = lens _nisNotebookInstanceStatus (\ s a -> s{_nisNotebookInstanceStatus = a})

-- | The name of a notebook instance lifecycle configuration associated with this notebook instance. For information about notebook instance lifestyle configurations, see 'notebook-lifecycle-config' .
nisNotebookInstanceLifecycleConfigName :: Lens' NotebookInstanceSummary (Maybe Text)
nisNotebookInstanceLifecycleConfigName = lens _nisNotebookInstanceLifecycleConfigName (\ s a -> s{_nisNotebookInstanceLifecycleConfigName = a})

-- | The name of the notebook instance that you want a summary for.
nisNotebookInstanceName :: Lens' NotebookInstanceSummary Text
nisNotebookInstanceName = lens _nisNotebookInstanceName (\ s a -> s{_nisNotebookInstanceName = a})

-- | The Amazon Resource Name (ARN) of the notebook instance.
nisNotebookInstanceARN :: Lens' NotebookInstanceSummary Text
nisNotebookInstanceARN = lens _nisNotebookInstanceARN (\ s a -> s{_nisNotebookInstanceARN = a})

instance FromJSON NotebookInstanceSummary where
        parseJSON
          = withObject "NotebookInstanceSummary"
              (\ x ->
                 NotebookInstanceSummary' <$>
                   (x .:? "CreationTime") <*> (x .:? "Url") <*>
                     (x .:? "LastModifiedTime")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "NotebookInstanceStatus")
                     <*> (x .:? "NotebookInstanceLifecycleConfigName")
                     <*> (x .: "NotebookInstanceName")
                     <*> (x .: "NotebookInstanceArn"))

instance Hashable NotebookInstanceSummary where

instance NFData NotebookInstanceSummary where

-- | Provides information about how to store model training results (model artifacts).
--
--
--
-- /See:/ 'outputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { _odcKMSKeyId     :: !(Maybe Text)
  , _odcS3OutputPath :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odcKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption.
--
-- * 'odcS3OutputPath' - Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
outputDataConfig
    :: Text -- ^ 'odcS3OutputPath'
    -> OutputDataConfig
outputDataConfig pS3OutputPath_ =
  OutputDataConfig' {_odcKMSKeyId = Nothing, _odcS3OutputPath = pS3OutputPath_}


-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption.
odcKMSKeyId :: Lens' OutputDataConfig (Maybe Text)
odcKMSKeyId = lens _odcKMSKeyId (\ s a -> s{_odcKMSKeyId = a})

-- | Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
odcS3OutputPath :: Lens' OutputDataConfig Text
odcS3OutputPath = lens _odcS3OutputPath (\ s a -> s{_odcS3OutputPath = a})

instance FromJSON OutputDataConfig where
        parseJSON
          = withObject "OutputDataConfig"
              (\ x ->
                 OutputDataConfig' <$>
                   (x .:? "KmsKeyId") <*> (x .: "S3OutputPath"))

instance Hashable OutputDataConfig where

instance NFData OutputDataConfig where

instance ToJSON OutputDataConfig where
        toJSON OutputDataConfig'{..}
          = object
              (catMaybes
                 [("KmsKeyId" .=) <$> _odcKMSKeyId,
                  Just ("S3OutputPath" .= _odcS3OutputPath)])

-- | Identifies a model that you want to host and the resources to deploy for hosting it. If you are deploying multiple models, tell Amazon SageMaker how to distribute traffic among the models by specifying variant weights.
--
--
--
-- /See:/ 'productionVariant' smart constructor.
data ProductionVariant = ProductionVariant'
  { _pvInitialVariantWeight :: !(Maybe Double)
  , _pvVariantName          :: !Text
  , _pvModelName            :: !Text
  , _pvInitialInstanceCount :: !Nat
  , _pvInstanceType         :: !ProductionVariantInstanceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProductionVariant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvInitialVariantWeight' - Determines initial traffic distribution among all of the models that you specify in the endpoint configuration. The traffic to a production variant is determined by the ratio of the @VariantWeight@ to the sum of all @VariantWeight@ values across all ProductionVariants. If unspecified, it defaults to 1.0.
--
-- * 'pvVariantName' - The name of the production variant.
--
-- * 'pvModelName' - The name of the model that you want to host. This is the name that you specified when creating the model.
--
-- * 'pvInitialInstanceCount' - Number of instances to launch initially.
--
-- * 'pvInstanceType' - The ML compute instance type.
productionVariant
    :: Text -- ^ 'pvVariantName'
    -> Text -- ^ 'pvModelName'
    -> Natural -- ^ 'pvInitialInstanceCount'
    -> ProductionVariantInstanceType -- ^ 'pvInstanceType'
    -> ProductionVariant
productionVariant pVariantName_ pModelName_ pInitialInstanceCount_ pInstanceType_ =
  ProductionVariant'
    { _pvInitialVariantWeight = Nothing
    , _pvVariantName = pVariantName_
    , _pvModelName = pModelName_
    , _pvInitialInstanceCount = _Nat # pInitialInstanceCount_
    , _pvInstanceType = pInstanceType_
    }


-- | Determines initial traffic distribution among all of the models that you specify in the endpoint configuration. The traffic to a production variant is determined by the ratio of the @VariantWeight@ to the sum of all @VariantWeight@ values across all ProductionVariants. If unspecified, it defaults to 1.0.
pvInitialVariantWeight :: Lens' ProductionVariant (Maybe Double)
pvInitialVariantWeight = lens _pvInitialVariantWeight (\ s a -> s{_pvInitialVariantWeight = a})

-- | The name of the production variant.
pvVariantName :: Lens' ProductionVariant Text
pvVariantName = lens _pvVariantName (\ s a -> s{_pvVariantName = a})

-- | The name of the model that you want to host. This is the name that you specified when creating the model.
pvModelName :: Lens' ProductionVariant Text
pvModelName = lens _pvModelName (\ s a -> s{_pvModelName = a})

-- | Number of instances to launch initially.
pvInitialInstanceCount :: Lens' ProductionVariant Natural
pvInitialInstanceCount = lens _pvInitialInstanceCount (\ s a -> s{_pvInitialInstanceCount = a}) . _Nat

-- | The ML compute instance type.
pvInstanceType :: Lens' ProductionVariant ProductionVariantInstanceType
pvInstanceType = lens _pvInstanceType (\ s a -> s{_pvInstanceType = a})

instance FromJSON ProductionVariant where
        parseJSON
          = withObject "ProductionVariant"
              (\ x ->
                 ProductionVariant' <$>
                   (x .:? "InitialVariantWeight") <*>
                     (x .: "VariantName")
                     <*> (x .: "ModelName")
                     <*> (x .: "InitialInstanceCount")
                     <*> (x .: "InstanceType"))

instance Hashable ProductionVariant where

instance NFData ProductionVariant where

instance ToJSON ProductionVariant where
        toJSON ProductionVariant'{..}
          = object
              (catMaybes
                 [("InitialVariantWeight" .=) <$>
                    _pvInitialVariantWeight,
                  Just ("VariantName" .= _pvVariantName),
                  Just ("ModelName" .= _pvModelName),
                  Just
                    ("InitialInstanceCount" .= _pvInitialInstanceCount),
                  Just ("InstanceType" .= _pvInstanceType)])

-- | Describes weight and capacities for a production variant associated with an endpoint. If you sent a request to the @UpdateEndpointWeightsAndCapacities@ API and the endpoint status is @Updating@ , you get different desired and current values.
--
--
--
-- /See:/ 'productionVariantSummary' smart constructor.
data ProductionVariantSummary = ProductionVariantSummary'
  { _pvsDesiredInstanceCount :: !(Maybe Nat)
  , _pvsDesiredWeight        :: !(Maybe Double)
  , _pvsCurrentWeight        :: !(Maybe Double)
  , _pvsCurrentInstanceCount :: !(Maybe Nat)
  , _pvsVariantName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProductionVariantSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvsDesiredInstanceCount' - The number of instances requested in the @UpdateEndpointWeightsAndCapacities@ request.
--
-- * 'pvsDesiredWeight' - The requested weight, as specified in the @UpdateEndpointWeightsAndCapacities@ request.
--
-- * 'pvsCurrentWeight' - The weight associated with the variant.
--
-- * 'pvsCurrentInstanceCount' - The number of instances associated with the variant.
--
-- * 'pvsVariantName' - The name of the variant.
productionVariantSummary
    :: Text -- ^ 'pvsVariantName'
    -> ProductionVariantSummary
productionVariantSummary pVariantName_ =
  ProductionVariantSummary'
    { _pvsDesiredInstanceCount = Nothing
    , _pvsDesiredWeight = Nothing
    , _pvsCurrentWeight = Nothing
    , _pvsCurrentInstanceCount = Nothing
    , _pvsVariantName = pVariantName_
    }


-- | The number of instances requested in the @UpdateEndpointWeightsAndCapacities@ request.
pvsDesiredInstanceCount :: Lens' ProductionVariantSummary (Maybe Natural)
pvsDesiredInstanceCount = lens _pvsDesiredInstanceCount (\ s a -> s{_pvsDesiredInstanceCount = a}) . mapping _Nat

-- | The requested weight, as specified in the @UpdateEndpointWeightsAndCapacities@ request.
pvsDesiredWeight :: Lens' ProductionVariantSummary (Maybe Double)
pvsDesiredWeight = lens _pvsDesiredWeight (\ s a -> s{_pvsDesiredWeight = a})

-- | The weight associated with the variant.
pvsCurrentWeight :: Lens' ProductionVariantSummary (Maybe Double)
pvsCurrentWeight = lens _pvsCurrentWeight (\ s a -> s{_pvsCurrentWeight = a})

-- | The number of instances associated with the variant.
pvsCurrentInstanceCount :: Lens' ProductionVariantSummary (Maybe Natural)
pvsCurrentInstanceCount = lens _pvsCurrentInstanceCount (\ s a -> s{_pvsCurrentInstanceCount = a}) . mapping _Nat

-- | The name of the variant.
pvsVariantName :: Lens' ProductionVariantSummary Text
pvsVariantName = lens _pvsVariantName (\ s a -> s{_pvsVariantName = a})

instance FromJSON ProductionVariantSummary where
        parseJSON
          = withObject "ProductionVariantSummary"
              (\ x ->
                 ProductionVariantSummary' <$>
                   (x .:? "DesiredInstanceCount") <*>
                     (x .:? "DesiredWeight")
                     <*> (x .:? "CurrentWeight")
                     <*> (x .:? "CurrentInstanceCount")
                     <*> (x .: "VariantName"))

instance Hashable ProductionVariantSummary where

instance NFData ProductionVariantSummary where

-- | Describes the resources, including ML compute instances and ML storage volumes, to use for model training.
--
--
--
-- /See:/ 'resourceConfig' smart constructor.
data ResourceConfig = ResourceConfig'
  { _rcVolumeKMSKeyId :: !(Maybe Text)
  , _rcInstanceType   :: !TrainingInstanceType
  , _rcInstanceCount  :: !Nat
  , _rcVolumeSizeInGB :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcVolumeKMSKeyId' - The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job.
--
-- * 'rcInstanceType' - The ML compute instance type.
--
-- * 'rcInstanceCount' - The number of ML compute instances to use. For distributed training, provide a value greater than 1.
--
-- * 'rcVolumeSizeInGB' - The size of the ML storage volume that you want to provision.  ML storage volumes store model artifacts and incremental states. Training algorithms might also use the ML storage volume for scratch space. If you want to store the training data in the ML storage volume, choose @File@ as the @TrainingInputMode@ in the algorithm specification.  You must specify sufficient ML storage for your scenario.
resourceConfig
    :: TrainingInstanceType -- ^ 'rcInstanceType'
    -> Natural -- ^ 'rcInstanceCount'
    -> Natural -- ^ 'rcVolumeSizeInGB'
    -> ResourceConfig
resourceConfig pInstanceType_ pInstanceCount_ pVolumeSizeInGB_ =
  ResourceConfig'
    { _rcVolumeKMSKeyId = Nothing
    , _rcInstanceType = pInstanceType_
    , _rcInstanceCount = _Nat # pInstanceCount_
    , _rcVolumeSizeInGB = _Nat # pVolumeSizeInGB_
    }


-- | The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job.
rcVolumeKMSKeyId :: Lens' ResourceConfig (Maybe Text)
rcVolumeKMSKeyId = lens _rcVolumeKMSKeyId (\ s a -> s{_rcVolumeKMSKeyId = a})

-- | The ML compute instance type.
rcInstanceType :: Lens' ResourceConfig TrainingInstanceType
rcInstanceType = lens _rcInstanceType (\ s a -> s{_rcInstanceType = a})

-- | The number of ML compute instances to use. For distributed training, provide a value greater than 1.
rcInstanceCount :: Lens' ResourceConfig Natural
rcInstanceCount = lens _rcInstanceCount (\ s a -> s{_rcInstanceCount = a}) . _Nat

-- | The size of the ML storage volume that you want to provision.  ML storage volumes store model artifacts and incremental states. Training algorithms might also use the ML storage volume for scratch space. If you want to store the training data in the ML storage volume, choose @File@ as the @TrainingInputMode@ in the algorithm specification.  You must specify sufficient ML storage for your scenario.
rcVolumeSizeInGB :: Lens' ResourceConfig Natural
rcVolumeSizeInGB = lens _rcVolumeSizeInGB (\ s a -> s{_rcVolumeSizeInGB = a}) . _Nat

instance FromJSON ResourceConfig where
        parseJSON
          = withObject "ResourceConfig"
              (\ x ->
                 ResourceConfig' <$>
                   (x .:? "VolumeKmsKeyId") <*> (x .: "InstanceType")
                     <*> (x .: "InstanceCount")
                     <*> (x .: "VolumeSizeInGB"))

instance Hashable ResourceConfig where

instance NFData ResourceConfig where

instance ToJSON ResourceConfig where
        toJSON ResourceConfig'{..}
          = object
              (catMaybes
                 [("VolumeKmsKeyId" .=) <$> _rcVolumeKMSKeyId,
                  Just ("InstanceType" .= _rcInstanceType),
                  Just ("InstanceCount" .= _rcInstanceCount),
                  Just ("VolumeSizeInGB" .= _rcVolumeSizeInGB)])

-- | Describes the S3 data source.
--
--
--
-- /See:/ 's3DataSource' smart constructor.
data S3DataSource = S3DataSource'
  { _sdsS3DataDistributionType :: !(Maybe S3DataDistribution)
  , _sdsS3DataType             :: !S3DataType
  , _sdsS3URI                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsS3DataDistributionType' - If you want Amazon SageMaker to replicate the entire dataset on each ML compute instance that is launched for model training, specify @FullyReplicated@ .  If you want Amazon SageMaker to replicate a subset of data on each ML compute instance that is launched for model training, specify @ShardedByS3Key@ . If there are /n/ ML compute instances launched for a training job, each instance gets approximately 1//n/ of the number of S3 objects. In this case, model training on each machine uses only the subset of training data.  Don't choose more ML compute instances for training than available S3 objects. If you do, some nodes won't get any data and you will pay for nodes that aren't getting any training data. This applies in both FILE and PIPE modes. Keep this in mind when developing algorithms.  In distributed training, where you use multiple ML compute EC2 instances, you might choose @ShardedByS3Key@ . If the algorithm requires copying training data to the ML storage volume (when @TrainingInputMode@ is set to @File@ ), this copies 1//n/ of the number of objects.
--
-- * 'sdsS3DataType' - If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for model training.  If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for model training.
--
-- * 'sdsS3URI' - Depending on the value specified for the @S3DataType@ , identifies either a key name prefix or a manifest. For example:      * A key name prefix might look like this: @s3://bucketname/exampleprefix@ .      * A manifest might look like this: @s3://bucketname/example.manifest@  The manifest is an S3 object which is a JSON file with the following format:  @[@  @{"prefix": "s3://customer_bucket/some/prefix/"},@  @"relative/path/to/custdata-1",@  @"relative/path/custdata-2",@  @...@  @]@  The preceding JSON matches the following @s3Uris@ :  @s3://customer_bucket/some/prefix/relative/path/to/custdata-1@  @s3://customer_bucket/some/prefix/relative/path/custdata-1@  @...@  The complete set of @s3uris@ in this manifest constitutes the input data for the channel for this datasource. The object that each @s3uris@ points to must readable by the IAM role that Amazon SageMaker uses to perform tasks on your behalf.
s3DataSource
    :: S3DataType -- ^ 'sdsS3DataType'
    -> Text -- ^ 'sdsS3URI'
    -> S3DataSource
s3DataSource pS3DataType_ pS3URI_ =
  S3DataSource'
    { _sdsS3DataDistributionType = Nothing
    , _sdsS3DataType = pS3DataType_
    , _sdsS3URI = pS3URI_
    }


-- | If you want Amazon SageMaker to replicate the entire dataset on each ML compute instance that is launched for model training, specify @FullyReplicated@ .  If you want Amazon SageMaker to replicate a subset of data on each ML compute instance that is launched for model training, specify @ShardedByS3Key@ . If there are /n/ ML compute instances launched for a training job, each instance gets approximately 1//n/ of the number of S3 objects. In this case, model training on each machine uses only the subset of training data.  Don't choose more ML compute instances for training than available S3 objects. If you do, some nodes won't get any data and you will pay for nodes that aren't getting any training data. This applies in both FILE and PIPE modes. Keep this in mind when developing algorithms.  In distributed training, where you use multiple ML compute EC2 instances, you might choose @ShardedByS3Key@ . If the algorithm requires copying training data to the ML storage volume (when @TrainingInputMode@ is set to @File@ ), this copies 1//n/ of the number of objects.
sdsS3DataDistributionType :: Lens' S3DataSource (Maybe S3DataDistribution)
sdsS3DataDistributionType = lens _sdsS3DataDistributionType (\ s a -> s{_sdsS3DataDistributionType = a})

-- | If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for model training.  If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for model training.
sdsS3DataType :: Lens' S3DataSource S3DataType
sdsS3DataType = lens _sdsS3DataType (\ s a -> s{_sdsS3DataType = a})

-- | Depending on the value specified for the @S3DataType@ , identifies either a key name prefix or a manifest. For example:      * A key name prefix might look like this: @s3://bucketname/exampleprefix@ .      * A manifest might look like this: @s3://bucketname/example.manifest@  The manifest is an S3 object which is a JSON file with the following format:  @[@  @{"prefix": "s3://customer_bucket/some/prefix/"},@  @"relative/path/to/custdata-1",@  @"relative/path/custdata-2",@  @...@  @]@  The preceding JSON matches the following @s3Uris@ :  @s3://customer_bucket/some/prefix/relative/path/to/custdata-1@  @s3://customer_bucket/some/prefix/relative/path/custdata-1@  @...@  The complete set of @s3uris@ in this manifest constitutes the input data for the channel for this datasource. The object that each @s3uris@ points to must readable by the IAM role that Amazon SageMaker uses to perform tasks on your behalf.
sdsS3URI :: Lens' S3DataSource Text
sdsS3URI = lens _sdsS3URI (\ s a -> s{_sdsS3URI = a})

instance FromJSON S3DataSource where
        parseJSON
          = withObject "S3DataSource"
              (\ x ->
                 S3DataSource' <$>
                   (x .:? "S3DataDistributionType") <*>
                     (x .: "S3DataType")
                     <*> (x .: "S3Uri"))

instance Hashable S3DataSource where

instance NFData S3DataSource where

instance ToJSON S3DataSource where
        toJSON S3DataSource'{..}
          = object
              (catMaybes
                 [("S3DataDistributionType" .=) <$>
                    _sdsS3DataDistributionType,
                  Just ("S3DataType" .= _sdsS3DataType),
                  Just ("S3Uri" .= _sdsS3URI)])

-- | Specifies how long model training can run. When model training reaches the limit, Amazon SageMaker ends the training job. Use this API to cap model training cost.
--
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for120 seconds. Algorithms might use this 120-second window to save the model artifacts, so the results of training is not lost.
--
-- Training algorithms provided by Amazon SageMaker automatically saves the intermediate results of a model training job (it is best effort case, as model might not be ready to save as some stages, for example training just started). This intermediate data is a valid model artifact. You can use it to create a model (@CreateModel@ ).
--
--
-- /See:/ 'stoppingCondition' smart constructor.
newtype StoppingCondition = StoppingCondition'
  { _scMaxRuntimeInSeconds :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StoppingCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scMaxRuntimeInSeconds' - The maximum length of time, in seconds, that the training job can run. If model training does not complete during this time, Amazon SageMaker ends the job. If value is not specified, default value is 1 day. Maximum value is 5 days.
stoppingCondition
    :: StoppingCondition
stoppingCondition = StoppingCondition' {_scMaxRuntimeInSeconds = Nothing}


-- | The maximum length of time, in seconds, that the training job can run. If model training does not complete during this time, Amazon SageMaker ends the job. If value is not specified, default value is 1 day. Maximum value is 5 days.
scMaxRuntimeInSeconds :: Lens' StoppingCondition (Maybe Natural)
scMaxRuntimeInSeconds = lens _scMaxRuntimeInSeconds (\ s a -> s{_scMaxRuntimeInSeconds = a}) . mapping _Nat

instance FromJSON StoppingCondition where
        parseJSON
          = withObject "StoppingCondition"
              (\ x ->
                 StoppingCondition' <$> (x .:? "MaxRuntimeInSeconds"))

instance Hashable StoppingCondition where

instance NFData StoppingCondition where

instance ToJSON StoppingCondition where
        toJSON StoppingCondition'{..}
          = object
              (catMaybes
                 [("MaxRuntimeInSeconds" .=) <$>
                    _scMaxRuntimeInSeconds])

-- | Describes a tag.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The tag key.
--
-- * 'tagValue' - The tag value.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The tag key.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The tag value.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])

-- | Provides summary information about a training job.
--
--
--
-- /See:/ 'trainingJobSummary' smart constructor.
data TrainingJobSummary = TrainingJobSummary'
  { _tjsTrainingEndTime   :: !(Maybe POSIX)
  , _tjsLastModifiedTime  :: !(Maybe POSIX)
  , _tjsTrainingJobName   :: !Text
  , _tjsTrainingJobARN    :: !Text
  , _tjsCreationTime      :: !POSIX
  , _tjsTrainingJobStatus :: !TrainingJobStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrainingJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjsTrainingEndTime' - A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
--
-- * 'tjsLastModifiedTime' - Timestamp when the training job was last modified.
--
-- * 'tjsTrainingJobName' - The name of the training job that you want a summary for.
--
-- * 'tjsTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
--
-- * 'tjsCreationTime' - A timestamp that shows when the training job was created.
--
-- * 'tjsTrainingJobStatus' - The status of the training job.
trainingJobSummary
    :: Text -- ^ 'tjsTrainingJobName'
    -> Text -- ^ 'tjsTrainingJobARN'
    -> UTCTime -- ^ 'tjsCreationTime'
    -> TrainingJobStatus -- ^ 'tjsTrainingJobStatus'
    -> TrainingJobSummary
trainingJobSummary pTrainingJobName_ pTrainingJobARN_ pCreationTime_ pTrainingJobStatus_ =
  TrainingJobSummary'
    { _tjsTrainingEndTime = Nothing
    , _tjsLastModifiedTime = Nothing
    , _tjsTrainingJobName = pTrainingJobName_
    , _tjsTrainingJobARN = pTrainingJobARN_
    , _tjsCreationTime = _Time # pCreationTime_
    , _tjsTrainingJobStatus = pTrainingJobStatus_
    }


-- | A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
tjsTrainingEndTime :: Lens' TrainingJobSummary (Maybe UTCTime)
tjsTrainingEndTime = lens _tjsTrainingEndTime (\ s a -> s{_tjsTrainingEndTime = a}) . mapping _Time

-- | Timestamp when the training job was last modified.
tjsLastModifiedTime :: Lens' TrainingJobSummary (Maybe UTCTime)
tjsLastModifiedTime = lens _tjsLastModifiedTime (\ s a -> s{_tjsLastModifiedTime = a}) . mapping _Time

-- | The name of the training job that you want a summary for.
tjsTrainingJobName :: Lens' TrainingJobSummary Text
tjsTrainingJobName = lens _tjsTrainingJobName (\ s a -> s{_tjsTrainingJobName = a})

-- | The Amazon Resource Name (ARN) of the training job.
tjsTrainingJobARN :: Lens' TrainingJobSummary Text
tjsTrainingJobARN = lens _tjsTrainingJobARN (\ s a -> s{_tjsTrainingJobARN = a})

-- | A timestamp that shows when the training job was created.
tjsCreationTime :: Lens' TrainingJobSummary UTCTime
tjsCreationTime = lens _tjsCreationTime (\ s a -> s{_tjsCreationTime = a}) . _Time

-- | The status of the training job.
tjsTrainingJobStatus :: Lens' TrainingJobSummary TrainingJobStatus
tjsTrainingJobStatus = lens _tjsTrainingJobStatus (\ s a -> s{_tjsTrainingJobStatus = a})

instance FromJSON TrainingJobSummary where
        parseJSON
          = withObject "TrainingJobSummary"
              (\ x ->
                 TrainingJobSummary' <$>
                   (x .:? "TrainingEndTime") <*>
                     (x .:? "LastModifiedTime")
                     <*> (x .: "TrainingJobName")
                     <*> (x .: "TrainingJobArn")
                     <*> (x .: "CreationTime")
                     <*> (x .: "TrainingJobStatus"))

instance Hashable TrainingJobSummary where

instance NFData TrainingJobSummary where

-- | Specifies a VPC that your training jobs and hosted models have access to. Control access to and from your training and model containers by configuring the VPC. For more information, see 'host-vpc' and 'train-vpc' .
--
--
--
-- /See:/ 'vpcConfig' smart constructor.
data VPCConfig = VPCConfig'
  { _vcSecurityGroupIds :: !(List1 Text)
  , _vcSubnets          :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSecurityGroupIds' - The VPC security group IDs, in the form sg-xxxxxxxx. Specify the security groups for the VPC that is specified in the @Subnets@ field.
--
-- * 'vcSubnets' - The ID of the subnets in the VPC to which you want to connect your training job or model.
vpcConfig
    :: NonEmpty Text -- ^ 'vcSecurityGroupIds'
    -> NonEmpty Text -- ^ 'vcSubnets'
    -> VPCConfig
vpcConfig pSecurityGroupIds_ pSubnets_ =
  VPCConfig'
    { _vcSecurityGroupIds = _List1 # pSecurityGroupIds_
    , _vcSubnets = _List1 # pSubnets_
    }


-- | The VPC security group IDs, in the form sg-xxxxxxxx. Specify the security groups for the VPC that is specified in the @Subnets@ field.
vcSecurityGroupIds :: Lens' VPCConfig (NonEmpty Text)
vcSecurityGroupIds = lens _vcSecurityGroupIds (\ s a -> s{_vcSecurityGroupIds = a}) . _List1

-- | The ID of the subnets in the VPC to which you want to connect your training job or model.
vcSubnets :: Lens' VPCConfig (NonEmpty Text)
vcSubnets = lens _vcSubnets (\ s a -> s{_vcSubnets = a}) . _List1

instance FromJSON VPCConfig where
        parseJSON
          = withObject "VPCConfig"
              (\ x ->
                 VPCConfig' <$>
                   (x .: "SecurityGroupIds") <*> (x .: "Subnets"))

instance Hashable VPCConfig where

instance NFData VPCConfig where

instance ToJSON VPCConfig where
        toJSON VPCConfig'{..}
          = object
              (catMaybes
                 [Just ("SecurityGroupIds" .= _vcSecurityGroupIds),
                  Just ("Subnets" .= _vcSubnets)])
