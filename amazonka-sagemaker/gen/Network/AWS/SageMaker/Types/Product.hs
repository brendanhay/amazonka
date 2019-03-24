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

-- | Specifies the training algorithm to use in a <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateTrainingJob.html CreateTrainingJob> request.
--
--
-- For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about using your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
--
-- /See:/ 'algorithmSpecification' smart constructor.
data AlgorithmSpecification = AlgorithmSpecification'
  { _asAlgorithmName     :: !(Maybe Text)
  , _asTrainingImage     :: !(Maybe Text)
  , _asMetricDefinitions :: !(Maybe [MetricDefinition])
  , _asTrainingInputMode :: !TrainingInputMode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AlgorithmSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAlgorithmName' - The name of the algorithm resource to use for the training job. This must be an algorithm resource that you created or subscribe to on AWS Marketplace. If you specify a value for this parameter, you can't specify a value for @TrainingImage@ .
--
-- * 'asTrainingImage' - The registry path of the Docker image that contains the training algorithm. For information about docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- * 'asMetricDefinitions' - A list of metric definition objects. Each object specifies the metric name and regular expressions used to parse algorithm logs. Amazon SageMaker publishes each metric to Amazon CloudWatch.
--
-- * 'asTrainingInputMode' - The input mode that the algorithm supports. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.  In File mode, make sure you provision ML storage volume with sufficient capacity to accommodate the data download from S3. In addition to the training data, the ML storage volume also stores the output model. The algorithm container use ML storage volume to also store intermediate information, if any.  For distributed algorithms using File mode, training data is distributed uniformly, and your training duration is predictable if the input data objects size is approximately same. Amazon SageMaker does not split the files any further for model training. If the object sizes are skewed, training won't be optimal as the data distribution is also skewed where one host in a training cluster is overloaded, thus becoming bottleneck in training.
algorithmSpecification
    :: TrainingInputMode -- ^ 'asTrainingInputMode'
    -> AlgorithmSpecification
algorithmSpecification pTrainingInputMode_ =
  AlgorithmSpecification'
    { _asAlgorithmName = Nothing
    , _asTrainingImage = Nothing
    , _asMetricDefinitions = Nothing
    , _asTrainingInputMode = pTrainingInputMode_
    }


-- | The name of the algorithm resource to use for the training job. This must be an algorithm resource that you created or subscribe to on AWS Marketplace. If you specify a value for this parameter, you can't specify a value for @TrainingImage@ .
asAlgorithmName :: Lens' AlgorithmSpecification (Maybe Text)
asAlgorithmName = lens _asAlgorithmName (\ s a -> s{_asAlgorithmName = a})

-- | The registry path of the Docker image that contains the training algorithm. For information about docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
asTrainingImage :: Lens' AlgorithmSpecification (Maybe Text)
asTrainingImage = lens _asTrainingImage (\ s a -> s{_asTrainingImage = a})

-- | A list of metric definition objects. Each object specifies the metric name and regular expressions used to parse algorithm logs. Amazon SageMaker publishes each metric to Amazon CloudWatch.
asMetricDefinitions :: Lens' AlgorithmSpecification [MetricDefinition]
asMetricDefinitions = lens _asMetricDefinitions (\ s a -> s{_asMetricDefinitions = a}) . _Default . _Coerce

-- | The input mode that the algorithm supports. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.  In File mode, make sure you provision ML storage volume with sufficient capacity to accommodate the data download from S3. In addition to the training data, the ML storage volume also stores the output model. The algorithm container use ML storage volume to also store intermediate information, if any.  For distributed algorithms using File mode, training data is distributed uniformly, and your training duration is predictable if the input data objects size is approximately same. Amazon SageMaker does not split the files any further for model training. If the object sizes are skewed, training won't be optimal as the data distribution is also skewed where one host in a training cluster is overloaded, thus becoming bottleneck in training.
asTrainingInputMode :: Lens' AlgorithmSpecification TrainingInputMode
asTrainingInputMode = lens _asTrainingInputMode (\ s a -> s{_asTrainingInputMode = a})

instance FromJSON AlgorithmSpecification where
        parseJSON
          = withObject "AlgorithmSpecification"
              (\ x ->
                 AlgorithmSpecification' <$>
                   (x .:? "AlgorithmName") <*> (x .:? "TrainingImage")
                     <*> (x .:? "MetricDefinitions" .!= mempty)
                     <*> (x .: "TrainingInputMode"))

instance Hashable AlgorithmSpecification where

instance NFData AlgorithmSpecification where

instance ToJSON AlgorithmSpecification where
        toJSON AlgorithmSpecification'{..}
          = object
              (catMaybes
                 [("AlgorithmName" .=) <$> _asAlgorithmName,
                  ("TrainingImage" .=) <$> _asTrainingImage,
                  ("MetricDefinitions" .=) <$> _asMetricDefinitions,
                  Just ("TrainingInputMode" .= _asTrainingInputMode)])

-- | Specifies the validation and image scan statuses of the algorithm.
--
--
--
-- /See:/ 'algorithmStatusDetails' smart constructor.
data AlgorithmStatusDetails = AlgorithmStatusDetails'
  { _asdImageScanStatuses  :: !(Maybe [AlgorithmStatusItem])
  , _asdValidationStatuses :: !(Maybe [AlgorithmStatusItem])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AlgorithmStatusDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asdImageScanStatuses' - The status of the scan of the algorithm's Docker image container.
--
-- * 'asdValidationStatuses' - The status of algorithm validation.
algorithmStatusDetails
    :: AlgorithmStatusDetails
algorithmStatusDetails =
  AlgorithmStatusDetails'
    {_asdImageScanStatuses = Nothing, _asdValidationStatuses = Nothing}


-- | The status of the scan of the algorithm's Docker image container.
asdImageScanStatuses :: Lens' AlgorithmStatusDetails [AlgorithmStatusItem]
asdImageScanStatuses = lens _asdImageScanStatuses (\ s a -> s{_asdImageScanStatuses = a}) . _Default . _Coerce

-- | The status of algorithm validation.
asdValidationStatuses :: Lens' AlgorithmStatusDetails [AlgorithmStatusItem]
asdValidationStatuses = lens _asdValidationStatuses (\ s a -> s{_asdValidationStatuses = a}) . _Default . _Coerce

instance FromJSON AlgorithmStatusDetails where
        parseJSON
          = withObject "AlgorithmStatusDetails"
              (\ x ->
                 AlgorithmStatusDetails' <$>
                   (x .:? "ImageScanStatuses" .!= mempty) <*>
                     (x .:? "ValidationStatuses" .!= mempty))

instance Hashable AlgorithmStatusDetails where

instance NFData AlgorithmStatusDetails where

-- | Represents the overall status of an algorithm.
--
--
--
-- /See:/ 'algorithmStatusItem' smart constructor.
data AlgorithmStatusItem = AlgorithmStatusItem'
  { _asiFailureReason :: !(Maybe Text)
  , _asiName          :: !Text
  , _asiStatus        :: !DetailedAlgorithmStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AlgorithmStatusItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asiFailureReason' - if the overall status is @Failed@ , the reason for the failure.
--
-- * 'asiName' - The name of the algorithm for which the overall status is being reported.
--
-- * 'asiStatus' - The current status.
algorithmStatusItem
    :: Text -- ^ 'asiName'
    -> DetailedAlgorithmStatus -- ^ 'asiStatus'
    -> AlgorithmStatusItem
algorithmStatusItem pName_ pStatus_ =
  AlgorithmStatusItem'
    {_asiFailureReason = Nothing, _asiName = pName_, _asiStatus = pStatus_}


-- | if the overall status is @Failed@ , the reason for the failure.
asiFailureReason :: Lens' AlgorithmStatusItem (Maybe Text)
asiFailureReason = lens _asiFailureReason (\ s a -> s{_asiFailureReason = a})

-- | The name of the algorithm for which the overall status is being reported.
asiName :: Lens' AlgorithmStatusItem Text
asiName = lens _asiName (\ s a -> s{_asiName = a})

-- | The current status.
asiStatus :: Lens' AlgorithmStatusItem DetailedAlgorithmStatus
asiStatus = lens _asiStatus (\ s a -> s{_asiStatus = a})

instance FromJSON AlgorithmStatusItem where
        parseJSON
          = withObject "AlgorithmStatusItem"
              (\ x ->
                 AlgorithmStatusItem' <$>
                   (x .:? "FailureReason") <*> (x .: "Name") <*>
                     (x .: "Status"))

instance Hashable AlgorithmStatusItem where

instance NFData AlgorithmStatusItem where

-- | Provides summary information about an algorithm.
--
--
--
-- /See:/ 'algorithmSummary' smart constructor.
data AlgorithmSummary = AlgorithmSummary'
  { _aAlgorithmDescription :: !(Maybe Text)
  , _aAlgorithmName        :: !Text
  , _aAlgorithmARN         :: !Text
  , _aCreationTime         :: !POSIX
  , _aAlgorithmStatus      :: !AlgorithmStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AlgorithmSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAlgorithmDescription' - A brief description of the algorithm.
--
-- * 'aAlgorithmName' - The name of the algorithm that is described by the summary.
--
-- * 'aAlgorithmARN' - The Amazon Resource Name (ARN) of the algorithm.
--
-- * 'aCreationTime' - A timestamp that shows when the algorithm was created.
--
-- * 'aAlgorithmStatus' - The overall status of the algorithm.
algorithmSummary
    :: Text -- ^ 'aAlgorithmName'
    -> Text -- ^ 'aAlgorithmARN'
    -> UTCTime -- ^ 'aCreationTime'
    -> AlgorithmStatus -- ^ 'aAlgorithmStatus'
    -> AlgorithmSummary
algorithmSummary pAlgorithmName_ pAlgorithmARN_ pCreationTime_ pAlgorithmStatus_ =
  AlgorithmSummary'
    { _aAlgorithmDescription = Nothing
    , _aAlgorithmName = pAlgorithmName_
    , _aAlgorithmARN = pAlgorithmARN_
    , _aCreationTime = _Time # pCreationTime_
    , _aAlgorithmStatus = pAlgorithmStatus_
    }


-- | A brief description of the algorithm.
aAlgorithmDescription :: Lens' AlgorithmSummary (Maybe Text)
aAlgorithmDescription = lens _aAlgorithmDescription (\ s a -> s{_aAlgorithmDescription = a})

-- | The name of the algorithm that is described by the summary.
aAlgorithmName :: Lens' AlgorithmSummary Text
aAlgorithmName = lens _aAlgorithmName (\ s a -> s{_aAlgorithmName = a})

-- | The Amazon Resource Name (ARN) of the algorithm.
aAlgorithmARN :: Lens' AlgorithmSummary Text
aAlgorithmARN = lens _aAlgorithmARN (\ s a -> s{_aAlgorithmARN = a})

-- | A timestamp that shows when the algorithm was created.
aCreationTime :: Lens' AlgorithmSummary UTCTime
aCreationTime = lens _aCreationTime (\ s a -> s{_aCreationTime = a}) . _Time

-- | The overall status of the algorithm.
aAlgorithmStatus :: Lens' AlgorithmSummary AlgorithmStatus
aAlgorithmStatus = lens _aAlgorithmStatus (\ s a -> s{_aAlgorithmStatus = a})

instance FromJSON AlgorithmSummary where
        parseJSON
          = withObject "AlgorithmSummary"
              (\ x ->
                 AlgorithmSummary' <$>
                   (x .:? "AlgorithmDescription") <*>
                     (x .: "AlgorithmName")
                     <*> (x .: "AlgorithmArn")
                     <*> (x .: "CreationTime")
                     <*> (x .: "AlgorithmStatus"))

instance Hashable AlgorithmSummary where

instance NFData AlgorithmSummary where

-- | Defines a training job and a batch transform job that Amazon SageMaker runs to validate your algorithm.
--
--
-- The data provided in the validation profile is made available to your buyers on AWS Marketplace.
--
--
-- /See:/ 'algorithmValidationProfile' smart constructor.
data AlgorithmValidationProfile = AlgorithmValidationProfile'
  { _avpTransformJobDefinition :: !(Maybe TransformJobDefinition)
  , _avpProfileName            :: !Text
  , _avpTrainingJobDefinition  :: !TrainingJobDefinition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AlgorithmValidationProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avpTransformJobDefinition' - The @TransformJobDefinition@ object that describes the transform job that Amazon SageMaker runs to validate your algorithm.
--
-- * 'avpProfileName' - The name of the profile for the algorithm. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- * 'avpTrainingJobDefinition' - The @TrainingJobDefinition@ object that describes the training job that Amazon SageMaker runs to validate your algorithm.
algorithmValidationProfile
    :: Text -- ^ 'avpProfileName'
    -> TrainingJobDefinition -- ^ 'avpTrainingJobDefinition'
    -> AlgorithmValidationProfile
algorithmValidationProfile pProfileName_ pTrainingJobDefinition_ =
  AlgorithmValidationProfile'
    { _avpTransformJobDefinition = Nothing
    , _avpProfileName = pProfileName_
    , _avpTrainingJobDefinition = pTrainingJobDefinition_
    }


-- | The @TransformJobDefinition@ object that describes the transform job that Amazon SageMaker runs to validate your algorithm.
avpTransformJobDefinition :: Lens' AlgorithmValidationProfile (Maybe TransformJobDefinition)
avpTransformJobDefinition = lens _avpTransformJobDefinition (\ s a -> s{_avpTransformJobDefinition = a})

-- | The name of the profile for the algorithm. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
avpProfileName :: Lens' AlgorithmValidationProfile Text
avpProfileName = lens _avpProfileName (\ s a -> s{_avpProfileName = a})

-- | The @TrainingJobDefinition@ object that describes the training job that Amazon SageMaker runs to validate your algorithm.
avpTrainingJobDefinition :: Lens' AlgorithmValidationProfile TrainingJobDefinition
avpTrainingJobDefinition = lens _avpTrainingJobDefinition (\ s a -> s{_avpTrainingJobDefinition = a})

instance FromJSON AlgorithmValidationProfile where
        parseJSON
          = withObject "AlgorithmValidationProfile"
              (\ x ->
                 AlgorithmValidationProfile' <$>
                   (x .:? "TransformJobDefinition") <*>
                     (x .: "ProfileName")
                     <*> (x .: "TrainingJobDefinition"))

instance Hashable AlgorithmValidationProfile where

instance NFData AlgorithmValidationProfile where

instance ToJSON AlgorithmValidationProfile where
        toJSON AlgorithmValidationProfile'{..}
          = object
              (catMaybes
                 [("TransformJobDefinition" .=) <$>
                    _avpTransformJobDefinition,
                  Just ("ProfileName" .= _avpProfileName),
                  Just
                    ("TrainingJobDefinition" .=
                       _avpTrainingJobDefinition)])

-- | Specifies configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
--
--
--
-- /See:/ 'algorithmValidationSpecification' smart constructor.
data AlgorithmValidationSpecification = AlgorithmValidationSpecification'
  { _avsValidationRole     :: !Text
  , _avsValidationProfiles :: !(List1 AlgorithmValidationProfile)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AlgorithmValidationSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avsValidationRole' - The IAM roles that Amazon SageMaker uses to run the training jobs.
--
-- * 'avsValidationProfiles' - An array of @AlgorithmValidationProfile@ objects, each of which specifies a training job and batch transform job that Amazon SageMaker runs to validate your algorithm.
algorithmValidationSpecification
    :: Text -- ^ 'avsValidationRole'
    -> NonEmpty AlgorithmValidationProfile -- ^ 'avsValidationProfiles'
    -> AlgorithmValidationSpecification
algorithmValidationSpecification pValidationRole_ pValidationProfiles_ =
  AlgorithmValidationSpecification'
    { _avsValidationRole = pValidationRole_
    , _avsValidationProfiles = _List1 # pValidationProfiles_
    }


-- | The IAM roles that Amazon SageMaker uses to run the training jobs.
avsValidationRole :: Lens' AlgorithmValidationSpecification Text
avsValidationRole = lens _avsValidationRole (\ s a -> s{_avsValidationRole = a})

-- | An array of @AlgorithmValidationProfile@ objects, each of which specifies a training job and batch transform job that Amazon SageMaker runs to validate your algorithm.
avsValidationProfiles :: Lens' AlgorithmValidationSpecification (NonEmpty AlgorithmValidationProfile)
avsValidationProfiles = lens _avsValidationProfiles (\ s a -> s{_avsValidationProfiles = a}) . _List1

instance FromJSON AlgorithmValidationSpecification
         where
        parseJSON
          = withObject "AlgorithmValidationSpecification"
              (\ x ->
                 AlgorithmValidationSpecification' <$>
                   (x .: "ValidationRole") <*>
                     (x .: "ValidationProfiles"))

instance Hashable AlgorithmValidationSpecification
         where

instance NFData AlgorithmValidationSpecification
         where

instance ToJSON AlgorithmValidationSpecification
         where
        toJSON AlgorithmValidationSpecification'{..}
          = object
              (catMaybes
                 [Just ("ValidationRole" .= _avsValidationRole),
                  Just
                    ("ValidationProfiles" .= _avsValidationProfiles)])

-- | Configures how labels are consolidated across human workers.
--
--
--
-- /See:/ 'annotationConsolidationConfig' smart constructor.
newtype AnnotationConsolidationConfig = AnnotationConsolidationConfig'
  { _accAnnotationConsolidationLambdaARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnnotationConsolidationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'accAnnotationConsolidationLambdaARN' - The Amazon Resource Name (ARN) of a Lambda function implements the logic for annotation consolidation. For the built-in bounding box, image classification, semantic segmentation, and text classification task types, Amazon SageMaker Ground Truth provides the following Lambda functions:     * /Bounding box/ - Finds the most similar boxes from different workers based on the Jaccard index of the boxes. @arn:aws:lambda:us-east-1:432418664414:function:ACS-BoundingBox@  @arn:aws:lambda:us-east-2:266458841044:function:ACS-BoundingBox@  @arn:aws:lambda:us-west-2:081040173940:function:ACS-BoundingBox@  @arn:aws:lambda:eu-west-1:568282634449:function:ACS-BoundingBox@  @arn:aws:lambda:ap-northeast-1:477331159723:function:ACS-BoundingBox@      * /Image classification/ - Uses a variant of the Expectation Maximization approach to estimate the true class of an image based on annotations from individual workers. @arn:aws:lambda:us-east-1:432418664414:function:ACS-ImageMultiClass@  @arn:aws:lambda:us-east-2:266458841044:function:ACS-ImageMultiClass@  @arn:aws:lambda:us-west-2:081040173940:function:ACS-ImageMultiClass@  @arn:aws:lambda:eu-west-1:568282634449:function:ACS-ImageMultiClass@  @arn:aws:lambda:ap-northeast-1:477331159723:function:ACS-ImageMultiClass@      * /Semantic segmentation/ - Treats each pixel in an image as a multi-class classification and treats pixel annotations from workers as "votes" for the correct label. @arn:aws:lambda:us-east-1:432418664414:function:ACS-SemanticSegmentation@  @arn:aws:lambda:us-east-2:266458841044:function:ACS-SemanticSegmentation@  @arn:aws:lambda:us-west-2:081040173940:function:ACS-SemanticSegmentation@  @arn:aws:lambda:eu-west-1:568282634449:function:ACS-SemanticSegmentation@  @arn:aws:lambda:ap-northeast-1:477331159723:function:ACS-SemanticSegmentation@      * /Text classification/ - Uses a variant of the Expectation Maximization approach to estimate the true class of text based on annotations from individual workers. @arn:aws:lambda:us-east-1:432418664414:function:ACS-TextMultiClass@  @arn:aws:lambda:us-east-2:266458841044:function:ACS-TextMultiClass@  @arn:aws:lambda:us-west-2:081040173940:function:ACS-TextMultiClass@  @arn:aws:lambda:eu-west-1:568282634449:function:ACS-TextMultiClass@  @arn:aws:lambda:ap-northeast-1:477331159723:function:ACS-TextMultiClass@  For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
annotationConsolidationConfig
    :: Text -- ^ 'accAnnotationConsolidationLambdaARN'
    -> AnnotationConsolidationConfig
annotationConsolidationConfig pAnnotationConsolidationLambdaARN_ =
  AnnotationConsolidationConfig'
    {_accAnnotationConsolidationLambdaARN = pAnnotationConsolidationLambdaARN_}


-- | The Amazon Resource Name (ARN) of a Lambda function implements the logic for annotation consolidation. For the built-in bounding box, image classification, semantic segmentation, and text classification task types, Amazon SageMaker Ground Truth provides the following Lambda functions:     * /Bounding box/ - Finds the most similar boxes from different workers based on the Jaccard index of the boxes. @arn:aws:lambda:us-east-1:432418664414:function:ACS-BoundingBox@  @arn:aws:lambda:us-east-2:266458841044:function:ACS-BoundingBox@  @arn:aws:lambda:us-west-2:081040173940:function:ACS-BoundingBox@  @arn:aws:lambda:eu-west-1:568282634449:function:ACS-BoundingBox@  @arn:aws:lambda:ap-northeast-1:477331159723:function:ACS-BoundingBox@      * /Image classification/ - Uses a variant of the Expectation Maximization approach to estimate the true class of an image based on annotations from individual workers. @arn:aws:lambda:us-east-1:432418664414:function:ACS-ImageMultiClass@  @arn:aws:lambda:us-east-2:266458841044:function:ACS-ImageMultiClass@  @arn:aws:lambda:us-west-2:081040173940:function:ACS-ImageMultiClass@  @arn:aws:lambda:eu-west-1:568282634449:function:ACS-ImageMultiClass@  @arn:aws:lambda:ap-northeast-1:477331159723:function:ACS-ImageMultiClass@      * /Semantic segmentation/ - Treats each pixel in an image as a multi-class classification and treats pixel annotations from workers as "votes" for the correct label. @arn:aws:lambda:us-east-1:432418664414:function:ACS-SemanticSegmentation@  @arn:aws:lambda:us-east-2:266458841044:function:ACS-SemanticSegmentation@  @arn:aws:lambda:us-west-2:081040173940:function:ACS-SemanticSegmentation@  @arn:aws:lambda:eu-west-1:568282634449:function:ACS-SemanticSegmentation@  @arn:aws:lambda:ap-northeast-1:477331159723:function:ACS-SemanticSegmentation@      * /Text classification/ - Uses a variant of the Expectation Maximization approach to estimate the true class of text based on annotations from individual workers. @arn:aws:lambda:us-east-1:432418664414:function:ACS-TextMultiClass@  @arn:aws:lambda:us-east-2:266458841044:function:ACS-TextMultiClass@  @arn:aws:lambda:us-west-2:081040173940:function:ACS-TextMultiClass@  @arn:aws:lambda:eu-west-1:568282634449:function:ACS-TextMultiClass@  @arn:aws:lambda:ap-northeast-1:477331159723:function:ACS-TextMultiClass@  For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
accAnnotationConsolidationLambdaARN :: Lens' AnnotationConsolidationConfig Text
accAnnotationConsolidationLambdaARN = lens _accAnnotationConsolidationLambdaARN (\ s a -> s{_accAnnotationConsolidationLambdaARN = a})

instance FromJSON AnnotationConsolidationConfig where
        parseJSON
          = withObject "AnnotationConsolidationConfig"
              (\ x ->
                 AnnotationConsolidationConfig' <$>
                   (x .: "AnnotationConsolidationLambdaArn"))

instance Hashable AnnotationConsolidationConfig where

instance NFData AnnotationConsolidationConfig where

instance ToJSON AnnotationConsolidationConfig where
        toJSON AnnotationConsolidationConfig'{..}
          = object
              (catMaybes
                 [Just
                    ("AnnotationConsolidationLambdaArn" .=
                       _accAnnotationConsolidationLambdaARN)])

-- | A list of categorical hyperparameters to tune.
--
--
--
-- /See:/ 'categoricalParameterRange' smart constructor.
data CategoricalParameterRange = CategoricalParameterRange'
  { _cprName   :: !Text
  , _cprValues :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CategoricalParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprName' - The name of the categorical hyperparameter to tune.
--
-- * 'cprValues' - A list of the categories for the hyperparameter.
categoricalParameterRange
    :: Text -- ^ 'cprName'
    -> NonEmpty Text -- ^ 'cprValues'
    -> CategoricalParameterRange
categoricalParameterRange pName_ pValues_ =
  CategoricalParameterRange' {_cprName = pName_, _cprValues = _List1 # pValues_}


-- | The name of the categorical hyperparameter to tune.
cprName :: Lens' CategoricalParameterRange Text
cprName = lens _cprName (\ s a -> s{_cprName = a})

-- | A list of the categories for the hyperparameter.
cprValues :: Lens' CategoricalParameterRange (NonEmpty Text)
cprValues = lens _cprValues (\ s a -> s{_cprValues = a}) . _List1

instance FromJSON CategoricalParameterRange where
        parseJSON
          = withObject "CategoricalParameterRange"
              (\ x ->
                 CategoricalParameterRange' <$>
                   (x .: "Name") <*> (x .: "Values"))

instance Hashable CategoricalParameterRange where

instance NFData CategoricalParameterRange where

instance ToJSON CategoricalParameterRange where
        toJSON CategoricalParameterRange'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _cprName),
                  Just ("Values" .= _cprValues)])

-- | Defines the possible values for a categorical hyperparameter.
--
--
--
-- /See:/ 'categoricalParameterRangeSpecification' smart constructor.
newtype CategoricalParameterRangeSpecification = CategoricalParameterRangeSpecification'
  { _cprsValues :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CategoricalParameterRangeSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsValues' - The allowed categories for the hyperparameter.
categoricalParameterRangeSpecification
    :: NonEmpty Text -- ^ 'cprsValues'
    -> CategoricalParameterRangeSpecification
categoricalParameterRangeSpecification pValues_ =
  CategoricalParameterRangeSpecification' {_cprsValues = _List1 # pValues_}


-- | The allowed categories for the hyperparameter.
cprsValues :: Lens' CategoricalParameterRangeSpecification (NonEmpty Text)
cprsValues = lens _cprsValues (\ s a -> s{_cprsValues = a}) . _List1

instance FromJSON
           CategoricalParameterRangeSpecification
         where
        parseJSON
          = withObject "CategoricalParameterRangeSpecification"
              (\ x ->
                 CategoricalParameterRangeSpecification' <$>
                   (x .: "Values"))

instance Hashable
           CategoricalParameterRangeSpecification
         where

instance NFData
           CategoricalParameterRangeSpecification
         where

instance ToJSON
           CategoricalParameterRangeSpecification
         where
        toJSON CategoricalParameterRangeSpecification'{..}
          = object (catMaybes [Just ("Values" .= _cprsValues)])

-- | A channel is a named input source that training algorithms can consume.
--
--
--
-- /See:/ 'channel' smart constructor.
data Channel = Channel'
  { _cShuffleConfig     :: !(Maybe ShuffleConfig)
  , _cRecordWrapperType :: !(Maybe RecordWrapper)
  , _cInputMode         :: !(Maybe TrainingInputMode)
  , _cCompressionType   :: !(Maybe CompressionType)
  , _cContentType       :: !(Maybe Text)
  , _cChannelName       :: !Text
  , _cDataSource        :: !DataSource
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cShuffleConfig' - A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , this shuffles the results of the S3 key prefix matches. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value. For Pipe input mode, shuffling is done at the start of every epoch. With large datasets this ensures that the order of the training data is different for each epoch, it helps reduce bias and possible overfitting. In a multi-node training job when ShuffleConfig is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
--
-- * 'cRecordWrapperType' -  Specify RecordIO as the value when input data is in raw format but the training algorithm requires the RecordIO format. In this case, Amazon SageMaker wraps each individual S3 object in a RecordIO record. If the input data is already in RecordIO format, you don't need to set this attribute. For more information, see <https://mxnet.incubator.apache.org/architecture/note_data_loading.html#data-format Create a Dataset Using RecordIO> .  In File mode, leave this field unset or set it to None.
--
-- * 'cInputMode' - (Optional) The input mode to use for the data channel in a training job. If you don't set a value for @InputMode@ , Amazon SageMaker uses the value set for @TrainingInputMode@ . Use this parameter to override the @TrainingInputMode@ setting in a 'AlgorithmSpecification' request when you have a channel that needs a different input mode from the training job's general setting. To download the data from Amazon Simple Storage Service (Amazon S3) to the provisioned ML storage volume, and mount the directory to a Docker volume, use @File@ input mode. To stream data directly from Amazon S3 to the container, choose @Pipe@ input mode. To use a model for incremental training, choose @File@ input model.
--
-- * 'cCompressionType' - If training data is compressed, the compression type. The default value is @None@ . @CompressionType@ is used only in Pipe input mode. In File mode, leave this field unset or set it to None.
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
    { _cShuffleConfig = Nothing
    , _cRecordWrapperType = Nothing
    , _cInputMode = Nothing
    , _cCompressionType = Nothing
    , _cContentType = Nothing
    , _cChannelName = pChannelName_
    , _cDataSource = pDataSource_
    }


-- | A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , this shuffles the results of the S3 key prefix matches. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value. For Pipe input mode, shuffling is done at the start of every epoch. With large datasets this ensures that the order of the training data is different for each epoch, it helps reduce bias and possible overfitting. In a multi-node training job when ShuffleConfig is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
cShuffleConfig :: Lens' Channel (Maybe ShuffleConfig)
cShuffleConfig = lens _cShuffleConfig (\ s a -> s{_cShuffleConfig = a})

-- |  Specify RecordIO as the value when input data is in raw format but the training algorithm requires the RecordIO format. In this case, Amazon SageMaker wraps each individual S3 object in a RecordIO record. If the input data is already in RecordIO format, you don't need to set this attribute. For more information, see <https://mxnet.incubator.apache.org/architecture/note_data_loading.html#data-format Create a Dataset Using RecordIO> .  In File mode, leave this field unset or set it to None.
cRecordWrapperType :: Lens' Channel (Maybe RecordWrapper)
cRecordWrapperType = lens _cRecordWrapperType (\ s a -> s{_cRecordWrapperType = a})

-- | (Optional) The input mode to use for the data channel in a training job. If you don't set a value for @InputMode@ , Amazon SageMaker uses the value set for @TrainingInputMode@ . Use this parameter to override the @TrainingInputMode@ setting in a 'AlgorithmSpecification' request when you have a channel that needs a different input mode from the training job's general setting. To download the data from Amazon Simple Storage Service (Amazon S3) to the provisioned ML storage volume, and mount the directory to a Docker volume, use @File@ input mode. To stream data directly from Amazon S3 to the container, choose @Pipe@ input mode. To use a model for incremental training, choose @File@ input model.
cInputMode :: Lens' Channel (Maybe TrainingInputMode)
cInputMode = lens _cInputMode (\ s a -> s{_cInputMode = a})

-- | If training data is compressed, the compression type. The default value is @None@ . @CompressionType@ is used only in Pipe input mode. In File mode, leave this field unset or set it to None.
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
                   (x .:? "ShuffleConfig") <*>
                     (x .:? "RecordWrapperType")
                     <*> (x .:? "InputMode")
                     <*> (x .:? "CompressionType")
                     <*> (x .:? "ContentType")
                     <*> (x .: "ChannelName")
                     <*> (x .: "DataSource"))

instance Hashable Channel where

instance NFData Channel where

instance ToJSON Channel where
        toJSON Channel'{..}
          = object
              (catMaybes
                 [("ShuffleConfig" .=) <$> _cShuffleConfig,
                  ("RecordWrapperType" .=) <$> _cRecordWrapperType,
                  ("InputMode" .=) <$> _cInputMode,
                  ("CompressionType" .=) <$> _cCompressionType,
                  ("ContentType" .=) <$> _cContentType,
                  Just ("ChannelName" .= _cChannelName),
                  Just ("DataSource" .= _cDataSource)])

-- | Defines a named input source, called a channel, to be used by an algorithm.
--
--
--
-- /See:/ 'channelSpecification' smart constructor.
data ChannelSpecification = ChannelSpecification'
  { _csSupportedCompressionTypes :: !(Maybe [CompressionType])
  , _csIsRequired                :: !(Maybe Bool)
  , _csDescription               :: !(Maybe Text)
  , _csName                      :: !Text
  , _csSupportedContentTypes     :: ![Text]
  , _csSupportedInputModes       :: !(List1 TrainingInputMode)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChannelSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csSupportedCompressionTypes' - The allowed compression types, if data compression is used.
--
-- * 'csIsRequired' - Indicates whether the channel is required by the algorithm.
--
-- * 'csDescription' - A brief description of the channel.
--
-- * 'csName' - The name of the channel.
--
-- * 'csSupportedContentTypes' - The supported MIME types for the data.
--
-- * 'csSupportedInputModes' - The allowed input mode, either FILE or PIPE. In FILE mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode. In PIPE mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
channelSpecification
    :: Text -- ^ 'csName'
    -> NonEmpty TrainingInputMode -- ^ 'csSupportedInputModes'
    -> ChannelSpecification
channelSpecification pName_ pSupportedInputModes_ =
  ChannelSpecification'
    { _csSupportedCompressionTypes = Nothing
    , _csIsRequired = Nothing
    , _csDescription = Nothing
    , _csName = pName_
    , _csSupportedContentTypes = mempty
    , _csSupportedInputModes = _List1 # pSupportedInputModes_
    }


-- | The allowed compression types, if data compression is used.
csSupportedCompressionTypes :: Lens' ChannelSpecification [CompressionType]
csSupportedCompressionTypes = lens _csSupportedCompressionTypes (\ s a -> s{_csSupportedCompressionTypes = a}) . _Default . _Coerce

-- | Indicates whether the channel is required by the algorithm.
csIsRequired :: Lens' ChannelSpecification (Maybe Bool)
csIsRequired = lens _csIsRequired (\ s a -> s{_csIsRequired = a})

-- | A brief description of the channel.
csDescription :: Lens' ChannelSpecification (Maybe Text)
csDescription = lens _csDescription (\ s a -> s{_csDescription = a})

-- | The name of the channel.
csName :: Lens' ChannelSpecification Text
csName = lens _csName (\ s a -> s{_csName = a})

-- | The supported MIME types for the data.
csSupportedContentTypes :: Lens' ChannelSpecification [Text]
csSupportedContentTypes = lens _csSupportedContentTypes (\ s a -> s{_csSupportedContentTypes = a}) . _Coerce

-- | The allowed input mode, either FILE or PIPE. In FILE mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode. In PIPE mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
csSupportedInputModes :: Lens' ChannelSpecification (NonEmpty TrainingInputMode)
csSupportedInputModes = lens _csSupportedInputModes (\ s a -> s{_csSupportedInputModes = a}) . _List1

instance FromJSON ChannelSpecification where
        parseJSON
          = withObject "ChannelSpecification"
              (\ x ->
                 ChannelSpecification' <$>
                   (x .:? "SupportedCompressionTypes" .!= mempty) <*>
                     (x .:? "IsRequired")
                     <*> (x .:? "Description")
                     <*> (x .: "Name")
                     <*> (x .:? "SupportedContentTypes" .!= mempty)
                     <*> (x .: "SupportedInputModes"))

instance Hashable ChannelSpecification where

instance NFData ChannelSpecification where

instance ToJSON ChannelSpecification where
        toJSON ChannelSpecification'{..}
          = object
              (catMaybes
                 [("SupportedCompressionTypes" .=) <$>
                    _csSupportedCompressionTypes,
                  ("IsRequired" .=) <$> _csIsRequired,
                  ("Description" .=) <$> _csDescription,
                  Just ("Name" .= _csName),
                  Just
                    ("SupportedContentTypes" .=
                       _csSupportedContentTypes),
                  Just
                    ("SupportedInputModes" .= _csSupportedInputModes)])

-- | Specifies summary information about a Git repository.
--
--
--
-- /See:/ 'codeRepositorySummary' smart constructor.
data CodeRepositorySummary = CodeRepositorySummary'
  { _crsGitConfig          :: !(Maybe GitConfig)
  , _crsCodeRepositoryName :: !Text
  , _crsCodeRepositoryARN  :: !Text
  , _crsCreationTime       :: !POSIX
  , _crsLastModifiedTime   :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CodeRepositorySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsGitConfig' - Configuration details for the Git repository, including the URL where it is located and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
--
-- * 'crsCodeRepositoryName' - The name of the Git repository.
--
-- * 'crsCodeRepositoryARN' - The Amazon Resource Name (ARN) of the Git repository.
--
-- * 'crsCreationTime' - The date and time that the Git repository was created.
--
-- * 'crsLastModifiedTime' - The date and time that the Git repository was last modified.
codeRepositorySummary
    :: Text -- ^ 'crsCodeRepositoryName'
    -> Text -- ^ 'crsCodeRepositoryARN'
    -> UTCTime -- ^ 'crsCreationTime'
    -> UTCTime -- ^ 'crsLastModifiedTime'
    -> CodeRepositorySummary
codeRepositorySummary pCodeRepositoryName_ pCodeRepositoryARN_ pCreationTime_ pLastModifiedTime_ =
  CodeRepositorySummary'
    { _crsGitConfig = Nothing
    , _crsCodeRepositoryName = pCodeRepositoryName_
    , _crsCodeRepositoryARN = pCodeRepositoryARN_
    , _crsCreationTime = _Time # pCreationTime_
    , _crsLastModifiedTime = _Time # pLastModifiedTime_
    }


-- | Configuration details for the Git repository, including the URL where it is located and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
crsGitConfig :: Lens' CodeRepositorySummary (Maybe GitConfig)
crsGitConfig = lens _crsGitConfig (\ s a -> s{_crsGitConfig = a})

-- | The name of the Git repository.
crsCodeRepositoryName :: Lens' CodeRepositorySummary Text
crsCodeRepositoryName = lens _crsCodeRepositoryName (\ s a -> s{_crsCodeRepositoryName = a})

-- | The Amazon Resource Name (ARN) of the Git repository.
crsCodeRepositoryARN :: Lens' CodeRepositorySummary Text
crsCodeRepositoryARN = lens _crsCodeRepositoryARN (\ s a -> s{_crsCodeRepositoryARN = a})

-- | The date and time that the Git repository was created.
crsCreationTime :: Lens' CodeRepositorySummary UTCTime
crsCreationTime = lens _crsCreationTime (\ s a -> s{_crsCreationTime = a}) . _Time

-- | The date and time that the Git repository was last modified.
crsLastModifiedTime :: Lens' CodeRepositorySummary UTCTime
crsLastModifiedTime = lens _crsLastModifiedTime (\ s a -> s{_crsLastModifiedTime = a}) . _Time

instance FromJSON CodeRepositorySummary where
        parseJSON
          = withObject "CodeRepositorySummary"
              (\ x ->
                 CodeRepositorySummary' <$>
                   (x .:? "GitConfig") <*> (x .: "CodeRepositoryName")
                     <*> (x .: "CodeRepositoryArn")
                     <*> (x .: "CreationTime")
                     <*> (x .: "LastModifiedTime"))

instance Hashable CodeRepositorySummary where

instance NFData CodeRepositorySummary where

-- | Identifies a Amazon Cognito user group. A user group can be used in on or more work teams.
--
--
--
-- /See:/ 'cognitoMemberDefinition' smart constructor.
data CognitoMemberDefinition = CognitoMemberDefinition'
  { _cmdUserPool  :: !Text
  , _cmdUserGroup :: !Text
  , _cmdClientId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CognitoMemberDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmdUserPool' - An identifier for a user pool. The user pool must be in the same region as the service that you are calling.
--
-- * 'cmdUserGroup' - An identifier for a user group.
--
-- * 'cmdClientId' - An identifier for an application client. You must create the app client ID using Amazon Cognito.
cognitoMemberDefinition
    :: Text -- ^ 'cmdUserPool'
    -> Text -- ^ 'cmdUserGroup'
    -> Text -- ^ 'cmdClientId'
    -> CognitoMemberDefinition
cognitoMemberDefinition pUserPool_ pUserGroup_ pClientId_ =
  CognitoMemberDefinition'
    { _cmdUserPool = pUserPool_
    , _cmdUserGroup = pUserGroup_
    , _cmdClientId = pClientId_
    }


-- | An identifier for a user pool. The user pool must be in the same region as the service that you are calling.
cmdUserPool :: Lens' CognitoMemberDefinition Text
cmdUserPool = lens _cmdUserPool (\ s a -> s{_cmdUserPool = a})

-- | An identifier for a user group.
cmdUserGroup :: Lens' CognitoMemberDefinition Text
cmdUserGroup = lens _cmdUserGroup (\ s a -> s{_cmdUserGroup = a})

-- | An identifier for an application client. You must create the app client ID using Amazon Cognito.
cmdClientId :: Lens' CognitoMemberDefinition Text
cmdClientId = lens _cmdClientId (\ s a -> s{_cmdClientId = a})

instance FromJSON CognitoMemberDefinition where
        parseJSON
          = withObject "CognitoMemberDefinition"
              (\ x ->
                 CognitoMemberDefinition' <$>
                   (x .: "UserPool") <*> (x .: "UserGroup") <*>
                     (x .: "ClientId"))

instance Hashable CognitoMemberDefinition where

instance NFData CognitoMemberDefinition where

instance ToJSON CognitoMemberDefinition where
        toJSON CognitoMemberDefinition'{..}
          = object
              (catMaybes
                 [Just ("UserPool" .= _cmdUserPool),
                  Just ("UserGroup" .= _cmdUserGroup),
                  Just ("ClientId" .= _cmdClientId)])

-- | A summary of a model compilation job.
--
--
--
-- /See:/ 'compilationJobSummary' smart constructor.
data CompilationJobSummary = CompilationJobSummary'
  { _cjsCompilationStartTime    :: !(Maybe POSIX)
  , _cjsLastModifiedTime        :: !(Maybe POSIX)
  , _cjsCompilationEndTime      :: !(Maybe POSIX)
  , _cjsCompilationJobName      :: !Text
  , _cjsCompilationJobARN       :: !Text
  , _cjsCreationTime            :: !POSIX
  , _cjsCompilationTargetDevice :: !TargetDevice
  , _cjsCompilationJobStatus    :: !CompilationJobStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompilationJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjsCompilationStartTime' - The time when the model compilation job started.
--
-- * 'cjsLastModifiedTime' - The time when the model compilation job was last modified.
--
-- * 'cjsCompilationEndTime' - The time when the model compilation job completed.
--
-- * 'cjsCompilationJobName' - The name of the model compilation job that you want a summary for.
--
-- * 'cjsCompilationJobARN' - The Amazon Resource Name (ARN) of the model compilation job.
--
-- * 'cjsCreationTime' - The time when the model compilation job was created.
--
-- * 'cjsCompilationTargetDevice' - The type of device that the model will run on after compilation has completed.
--
-- * 'cjsCompilationJobStatus' - The status of the model compilation job.
compilationJobSummary
    :: Text -- ^ 'cjsCompilationJobName'
    -> Text -- ^ 'cjsCompilationJobARN'
    -> UTCTime -- ^ 'cjsCreationTime'
    -> TargetDevice -- ^ 'cjsCompilationTargetDevice'
    -> CompilationJobStatus -- ^ 'cjsCompilationJobStatus'
    -> CompilationJobSummary
compilationJobSummary pCompilationJobName_ pCompilationJobARN_ pCreationTime_ pCompilationTargetDevice_ pCompilationJobStatus_ =
  CompilationJobSummary'
    { _cjsCompilationStartTime = Nothing
    , _cjsLastModifiedTime = Nothing
    , _cjsCompilationEndTime = Nothing
    , _cjsCompilationJobName = pCompilationJobName_
    , _cjsCompilationJobARN = pCompilationJobARN_
    , _cjsCreationTime = _Time # pCreationTime_
    , _cjsCompilationTargetDevice = pCompilationTargetDevice_
    , _cjsCompilationJobStatus = pCompilationJobStatus_
    }


-- | The time when the model compilation job started.
cjsCompilationStartTime :: Lens' CompilationJobSummary (Maybe UTCTime)
cjsCompilationStartTime = lens _cjsCompilationStartTime (\ s a -> s{_cjsCompilationStartTime = a}) . mapping _Time

-- | The time when the model compilation job was last modified.
cjsLastModifiedTime :: Lens' CompilationJobSummary (Maybe UTCTime)
cjsLastModifiedTime = lens _cjsLastModifiedTime (\ s a -> s{_cjsLastModifiedTime = a}) . mapping _Time

-- | The time when the model compilation job completed.
cjsCompilationEndTime :: Lens' CompilationJobSummary (Maybe UTCTime)
cjsCompilationEndTime = lens _cjsCompilationEndTime (\ s a -> s{_cjsCompilationEndTime = a}) . mapping _Time

-- | The name of the model compilation job that you want a summary for.
cjsCompilationJobName :: Lens' CompilationJobSummary Text
cjsCompilationJobName = lens _cjsCompilationJobName (\ s a -> s{_cjsCompilationJobName = a})

-- | The Amazon Resource Name (ARN) of the model compilation job.
cjsCompilationJobARN :: Lens' CompilationJobSummary Text
cjsCompilationJobARN = lens _cjsCompilationJobARN (\ s a -> s{_cjsCompilationJobARN = a})

-- | The time when the model compilation job was created.
cjsCreationTime :: Lens' CompilationJobSummary UTCTime
cjsCreationTime = lens _cjsCreationTime (\ s a -> s{_cjsCreationTime = a}) . _Time

-- | The type of device that the model will run on after compilation has completed.
cjsCompilationTargetDevice :: Lens' CompilationJobSummary TargetDevice
cjsCompilationTargetDevice = lens _cjsCompilationTargetDevice (\ s a -> s{_cjsCompilationTargetDevice = a})

-- | The status of the model compilation job.
cjsCompilationJobStatus :: Lens' CompilationJobSummary CompilationJobStatus
cjsCompilationJobStatus = lens _cjsCompilationJobStatus (\ s a -> s{_cjsCompilationJobStatus = a})

instance FromJSON CompilationJobSummary where
        parseJSON
          = withObject "CompilationJobSummary"
              (\ x ->
                 CompilationJobSummary' <$>
                   (x .:? "CompilationStartTime") <*>
                     (x .:? "LastModifiedTime")
                     <*> (x .:? "CompilationEndTime")
                     <*> (x .: "CompilationJobName")
                     <*> (x .: "CompilationJobArn")
                     <*> (x .: "CreationTime")
                     <*> (x .: "CompilationTargetDevice")
                     <*> (x .: "CompilationJobStatus"))

instance Hashable CompilationJobSummary where

instance NFData CompilationJobSummary where

-- | Describes the container, as part of model definition.
--
--
--
-- /See:/ 'containerDefinition' smart constructor.
data ContainerDefinition = ContainerDefinition'
  { _cdModelDataURL      :: !(Maybe Text)
  , _cdImage             :: !(Maybe Text)
  , _cdModelPackageName  :: !(Maybe Text)
  , _cdEnvironment       :: !(Maybe (Map Text Text))
  , _cdContainerHostname :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContainerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdModelDataURL' - The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix).  If you provide a value for this parameter, Amazon SageMaker uses AWS Security Token Service to download model artifacts from the S3 path you provide. AWS STS is activated in your IAM user account by default. If you previously deactivated AWS STS for a region, you need to reactivate AWS STS for that region. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
--
-- * 'cdImage' - The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
--
-- * 'cdModelPackageName' - The name of the model package to use to create the model.
--
-- * 'cdEnvironment' - The environment variables to set in the Docker container. Each key and value in the @Environment@ string to string map can have length of up to 1024. We support up to 16 entries in the map.
--
-- * 'cdContainerHostname' - This parameter is ignored.
containerDefinition
    :: ContainerDefinition
containerDefinition =
  ContainerDefinition'
    { _cdModelDataURL = Nothing
    , _cdImage = Nothing
    , _cdModelPackageName = Nothing
    , _cdEnvironment = Nothing
    , _cdContainerHostname = Nothing
    }


-- | The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix).  If you provide a value for this parameter, Amazon SageMaker uses AWS Security Token Service to download model artifacts from the S3 path you provide. AWS STS is activated in your IAM user account by default. If you previously deactivated AWS STS for a region, you need to reactivate AWS STS for that region. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
cdModelDataURL :: Lens' ContainerDefinition (Maybe Text)
cdModelDataURL = lens _cdModelDataURL (\ s a -> s{_cdModelDataURL = a})

-- | The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>
cdImage :: Lens' ContainerDefinition (Maybe Text)
cdImage = lens _cdImage (\ s a -> s{_cdImage = a})

-- | The name of the model package to use to create the model.
cdModelPackageName :: Lens' ContainerDefinition (Maybe Text)
cdModelPackageName = lens _cdModelPackageName (\ s a -> s{_cdModelPackageName = a})

-- | The environment variables to set in the Docker container. Each key and value in the @Environment@ string to string map can have length of up to 1024. We support up to 16 entries in the map.
cdEnvironment :: Lens' ContainerDefinition (HashMap Text Text)
cdEnvironment = lens _cdEnvironment (\ s a -> s{_cdEnvironment = a}) . _Default . _Map

-- | This parameter is ignored.
cdContainerHostname :: Lens' ContainerDefinition (Maybe Text)
cdContainerHostname = lens _cdContainerHostname (\ s a -> s{_cdContainerHostname = a})

instance FromJSON ContainerDefinition where
        parseJSON
          = withObject "ContainerDefinition"
              (\ x ->
                 ContainerDefinition' <$>
                   (x .:? "ModelDataUrl") <*> (x .:? "Image") <*>
                     (x .:? "ModelPackageName")
                     <*> (x .:? "Environment" .!= mempty)
                     <*> (x .:? "ContainerHostname"))

instance Hashable ContainerDefinition where

instance NFData ContainerDefinition where

instance ToJSON ContainerDefinition where
        toJSON ContainerDefinition'{..}
          = object
              (catMaybes
                 [("ModelDataUrl" .=) <$> _cdModelDataURL,
                  ("Image" .=) <$> _cdImage,
                  ("ModelPackageName" .=) <$> _cdModelPackageName,
                  ("Environment" .=) <$> _cdEnvironment,
                  ("ContainerHostname" .=) <$> _cdContainerHostname])

-- | A list of continuous hyperparameters to tune.
--
--
--
-- /See:/ 'continuousParameterRange' smart constructor.
data ContinuousParameterRange = ContinuousParameterRange'
  { _cScalingType :: !(Maybe HyperParameterScalingType)
  , _cName        :: !Text
  , _cMinValue    :: !Text
  , _cMaxValue    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContinuousParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cScalingType' - The scale that hyperparameter tuning uses to search the hyperparameter range. For information about choosing a hyperparameter scale, see <http://docs.aws.amazon.com//sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Range Scaling> . One of the following values:     * Auto    * Amazon SageMaker hyperparameter tuning chooses the best scale for the hyperparameter.     * Linear    * Hyperparameter tuning searches the values in the hyperparameter range by using a linear scale.     * Logarithmic    * Hyperparemeter tuning searches the values in the hyperparameter range by using a logarithmic scale. Logarithmic scaling works only for ranges that have only values greater than 0.     * ReverseLogarithmic    * Hyperparemeter tuning searches the values in the hyperparameter range by using a reverse logarithmic scale. Reverse logarithmic scaling works only for ranges that are entirely within the range 0<=x<1.0.
--
-- * 'cName' - The name of the continuous hyperparameter to tune.
--
-- * 'cMinValue' - The minimum value for the hyperparameter. The tuning job uses floating-point values between this value and @MaxValue@ for tuning.
--
-- * 'cMaxValue' - The maximum value for the hyperparameter. The tuning job uses floating-point values between @MinValue@ value and this value for tuning.
continuousParameterRange
    :: Text -- ^ 'cName'
    -> Text -- ^ 'cMinValue'
    -> Text -- ^ 'cMaxValue'
    -> ContinuousParameterRange
continuousParameterRange pName_ pMinValue_ pMaxValue_ =
  ContinuousParameterRange'
    { _cScalingType = Nothing
    , _cName = pName_
    , _cMinValue = pMinValue_
    , _cMaxValue = pMaxValue_
    }


-- | The scale that hyperparameter tuning uses to search the hyperparameter range. For information about choosing a hyperparameter scale, see <http://docs.aws.amazon.com//sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Range Scaling> . One of the following values:     * Auto    * Amazon SageMaker hyperparameter tuning chooses the best scale for the hyperparameter.     * Linear    * Hyperparameter tuning searches the values in the hyperparameter range by using a linear scale.     * Logarithmic    * Hyperparemeter tuning searches the values in the hyperparameter range by using a logarithmic scale. Logarithmic scaling works only for ranges that have only values greater than 0.     * ReverseLogarithmic    * Hyperparemeter tuning searches the values in the hyperparameter range by using a reverse logarithmic scale. Reverse logarithmic scaling works only for ranges that are entirely within the range 0<=x<1.0.
cScalingType :: Lens' ContinuousParameterRange (Maybe HyperParameterScalingType)
cScalingType = lens _cScalingType (\ s a -> s{_cScalingType = a})

-- | The name of the continuous hyperparameter to tune.
cName :: Lens' ContinuousParameterRange Text
cName = lens _cName (\ s a -> s{_cName = a})

-- | The minimum value for the hyperparameter. The tuning job uses floating-point values between this value and @MaxValue@ for tuning.
cMinValue :: Lens' ContinuousParameterRange Text
cMinValue = lens _cMinValue (\ s a -> s{_cMinValue = a})

-- | The maximum value for the hyperparameter. The tuning job uses floating-point values between @MinValue@ value and this value for tuning.
cMaxValue :: Lens' ContinuousParameterRange Text
cMaxValue = lens _cMaxValue (\ s a -> s{_cMaxValue = a})

instance FromJSON ContinuousParameterRange where
        parseJSON
          = withObject "ContinuousParameterRange"
              (\ x ->
                 ContinuousParameterRange' <$>
                   (x .:? "ScalingType") <*> (x .: "Name") <*>
                     (x .: "MinValue")
                     <*> (x .: "MaxValue"))

instance Hashable ContinuousParameterRange where

instance NFData ContinuousParameterRange where

instance ToJSON ContinuousParameterRange where
        toJSON ContinuousParameterRange'{..}
          = object
              (catMaybes
                 [("ScalingType" .=) <$> _cScalingType,
                  Just ("Name" .= _cName),
                  Just ("MinValue" .= _cMinValue),
                  Just ("MaxValue" .= _cMaxValue)])

-- | Defines the possible values for a continuous hyperparameter.
--
--
--
-- /See:/ 'continuousParameterRangeSpecification' smart constructor.
data ContinuousParameterRangeSpecification = ContinuousParameterRangeSpecification'
  { _cprsMinValue :: !Text
  , _cprsMaxValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContinuousParameterRangeSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsMinValue' - The minimum floating-point value allowed.
--
-- * 'cprsMaxValue' - The maximum floating-point value allowed.
continuousParameterRangeSpecification
    :: Text -- ^ 'cprsMinValue'
    -> Text -- ^ 'cprsMaxValue'
    -> ContinuousParameterRangeSpecification
continuousParameterRangeSpecification pMinValue_ pMaxValue_ =
  ContinuousParameterRangeSpecification'
    {_cprsMinValue = pMinValue_, _cprsMaxValue = pMaxValue_}


-- | The minimum floating-point value allowed.
cprsMinValue :: Lens' ContinuousParameterRangeSpecification Text
cprsMinValue = lens _cprsMinValue (\ s a -> s{_cprsMinValue = a})

-- | The maximum floating-point value allowed.
cprsMaxValue :: Lens' ContinuousParameterRangeSpecification Text
cprsMaxValue = lens _cprsMaxValue (\ s a -> s{_cprsMaxValue = a})

instance FromJSON
           ContinuousParameterRangeSpecification
         where
        parseJSON
          = withObject "ContinuousParameterRangeSpecification"
              (\ x ->
                 ContinuousParameterRangeSpecification' <$>
                   (x .: "MinValue") <*> (x .: "MaxValue"))

instance Hashable
           ContinuousParameterRangeSpecification
         where

instance NFData ContinuousParameterRangeSpecification
         where

instance ToJSON ContinuousParameterRangeSpecification
         where
        toJSON ContinuousParameterRangeSpecification'{..}
          = object
              (catMaybes
                 [Just ("MinValue" .= _cprsMinValue),
                  Just ("MaxValue" .= _cprsMaxValue)])

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

-- | Gets the Amazon EC2 Container Registry path of the docker image of the model that is hosted in this 'ProductionVariant' .
--
--
-- If you used the @registry/repository[:tag]@ form to specify the image path of the primary container when you created the model hosted in this @ProductionVariant@ , the path resolves to a path of the form @registry/repository[@digest]@ . A digest is a hash value that identifies a specific version of an image. For information about Amazon ECR paths, see <http://docs.aws.amazon.com//AmazonECR/latest/userguide/docker-pull-ecr-image.html Pulling an Image> in the /Amazon ECR User Guide/ .
--
--
-- /See:/ 'deployedImage' smart constructor.
data DeployedImage = DeployedImage'
  { _diResolvedImage  :: !(Maybe Text)
  , _diSpecifiedImage :: !(Maybe Text)
  , _diResolutionTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeployedImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diResolvedImage' - The specific digest path of the image hosted in this @ProductionVariant@ .
--
-- * 'diSpecifiedImage' - The image path you specified when you created the model.
--
-- * 'diResolutionTime' - The date and time when the image path for the model resolved to the @ResolvedImage@
deployedImage
    :: DeployedImage
deployedImage =
  DeployedImage'
    { _diResolvedImage = Nothing
    , _diSpecifiedImage = Nothing
    , _diResolutionTime = Nothing
    }


-- | The specific digest path of the image hosted in this @ProductionVariant@ .
diResolvedImage :: Lens' DeployedImage (Maybe Text)
diResolvedImage = lens _diResolvedImage (\ s a -> s{_diResolvedImage = a})

-- | The image path you specified when you created the model.
diSpecifiedImage :: Lens' DeployedImage (Maybe Text)
diSpecifiedImage = lens _diSpecifiedImage (\ s a -> s{_diSpecifiedImage = a})

-- | The date and time when the image path for the model resolved to the @ResolvedImage@
diResolutionTime :: Lens' DeployedImage (Maybe UTCTime)
diResolutionTime = lens _diResolutionTime (\ s a -> s{_diResolutionTime = a}) . mapping _Time

instance FromJSON DeployedImage where
        parseJSON
          = withObject "DeployedImage"
              (\ x ->
                 DeployedImage' <$>
                   (x .:? "ResolvedImage") <*> (x .:? "SpecifiedImage")
                     <*> (x .:? "ResolutionTime"))

instance Hashable DeployedImage where

instance NFData DeployedImage where

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
-- * 'esEndpointStatus' - The status of the endpoint.     * @OutOfService@ : Endpoint is not available to take incoming requests.     * @Creating@ : 'CreateEndpoint' is executing.     * @Updating@ : 'UpdateEndpoint' or 'UpdateEndpointWeightsAndCapacities' is executing.     * @SystemUpdating@ : Endpoint is undergoing maintenance and cannot be updated or deleted or re-scaled until it has completed. This maintenance operation does not change any customer-specified values such as VPC config, KMS encryption, model, instance type, or instance count.     * @RollingBack@ : Endpoint fails to scale up or down or change its variant weight and is in the process of rolling back to its previous configuration. Once the rollback completes, endpoint returns to an @InService@ status. This transitional status only applies to an endpoint that has autoscaling enabled and is undergoing variant weight or capacity changes as part of an 'UpdateEndpointWeightsAndCapacities' call or when the 'UpdateEndpointWeightsAndCapacities' operation is called explicitly.     * @InService@ : Endpoint is available to process incoming requests.     * @Deleting@ : 'DeleteEndpoint' is executing.     * @Failed@ : Endpoint could not be created, updated, or re-scaled. Use 'DescribeEndpointOutput$FailureReason' for information about the failure. 'DeleteEndpoint' is the only operation that can be performed on a failed endpoint. To get a list of endpoints with a specified status, use the 'ListEndpointsInput$StatusEquals' filter.
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

-- | The status of the endpoint.     * @OutOfService@ : Endpoint is not available to take incoming requests.     * @Creating@ : 'CreateEndpoint' is executing.     * @Updating@ : 'UpdateEndpoint' or 'UpdateEndpointWeightsAndCapacities' is executing.     * @SystemUpdating@ : Endpoint is undergoing maintenance and cannot be updated or deleted or re-scaled until it has completed. This maintenance operation does not change any customer-specified values such as VPC config, KMS encryption, model, instance type, or instance count.     * @RollingBack@ : Endpoint fails to scale up or down or change its variant weight and is in the process of rolling back to its previous configuration. Once the rollback completes, endpoint returns to an @InService@ status. This transitional status only applies to an endpoint that has autoscaling enabled and is undergoing variant weight or capacity changes as part of an 'UpdateEndpointWeightsAndCapacities' call or when the 'UpdateEndpointWeightsAndCapacities' operation is called explicitly.     * @InService@ : Endpoint is available to process incoming requests.     * @Deleting@ : 'DeleteEndpoint' is executing.     * @Failed@ : Endpoint could not be created, updated, or re-scaled. Use 'DescribeEndpointOutput$FailureReason' for information about the failure. 'DeleteEndpoint' is the only operation that can be performed on a failed endpoint. To get a list of endpoints with a specified status, use the 'ListEndpointsInput$StatusEquals' filter.
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

-- | A conditional statement for a search expression that includes a Boolean operator, a resource property, and a value.
--
--
-- If you don't specify an @Operator@ and a @Value@ , the filter searches for only the specified property. For example, defining a @Filter@ for the @FailureReason@ for the @TrainingJob@ @Resource@ searches for training job objects that have a value in the @FailureReason@ field.
--
-- If you specify a @Value@ , but not an @Operator@ , Amazon SageMaker uses the equals operator as the default.
--
-- In search, there are several property types:
--
--     * Metrics    * To define a metric filter, enter a value using the form @"Metrics.<name>"@ , where @<name>@ is a metric name. For example, the following filter searches for training jobs with an @"accuracy"@ metric greater than @"0.9"@ :
--
-- @{@
--
-- @"Name": "Metrics.accuracy",@
--
-- @"Operator": "GREATER_THAN",@
--
-- @"Value": "0.9"@
--
-- @}@
--
--     * HyperParameters    * To define a hyperparameter filter, enter a value with the form @"HyperParameters.<name>"@ . Decimal hyperparameter values are treated as a decimal in a comparison if the specified @Value@ is also a decimal value. If the specified @Value@ is an integer, the decimal hyperparameter values are treated as integers. For example, the following filter is satisfied by training jobs with a @"learning_rate"@ hyperparameter that is less than @"0.5"@ :
--
-- @{@
--
-- @"Name": "HyperParameters.learning_rate",@
--
-- @"Operator": "LESS_THAN",@
--
-- @"Value": "0.5"@
--
-- @}@
--
--     * Tags    * To define a tag filter, enter a value with the form @"Tags.<key>"@ .
--
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fOperator :: !(Maybe Operator)
  , _fValue    :: !(Maybe Text)
  , _fName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fOperator' - A Boolean binary operator that is used to evaluate the filter. The operator field contains one of the following values:     * Equals    * The specified resource in @Name@ equals the specified @Value@ .     * NotEquals    * The specified resource in @Name@ does not equal the specified @Value@ .     * GreaterThan    * The specified resource in @Name@ is greater than the specified @Value@ . Not supported for text-based properties.     * GreaterThanOrEqualTo    * The specified resource in @Name@ is greater than or equal to the specified @Value@ . Not supported for text-based properties.     * LessThan    * The specified resource in @Name@ is less than the specified @Value@ . Not supported for text-based properties.     * LessThanOrEqualTo    * The specified resource in @Name@ is less than or equal to the specified @Value@ . Not supported for text-based properties.     * Contains    * Only supported for text-based properties. The word-list of the property contains the specified @Value@ . If you have specified a filter @Value@ , the default is @Equals@ .
--
-- * 'fValue' - A value used with @Resource@ and @Operator@ to determine if objects satisfy the filter's condition. For numerical properties, @Value@ must be an integer or floating-point decimal. For timestamp properties, @Value@ must be an ISO 8601 date-time string of the following format: @YYYY-mm-dd'T'HH:MM:SS@ .
--
-- * 'fName' - A property name. For example, @TrainingJobName@ . For the list of valid property names returned in a search result for each supported resource, see 'TrainingJob' properties. You must specify a valid property name for the resource.
filter'
    :: Text -- ^ 'fName'
    -> Filter
filter' pName_ =
  Filter' {_fOperator = Nothing, _fValue = Nothing, _fName = pName_}


-- | A Boolean binary operator that is used to evaluate the filter. The operator field contains one of the following values:     * Equals    * The specified resource in @Name@ equals the specified @Value@ .     * NotEquals    * The specified resource in @Name@ does not equal the specified @Value@ .     * GreaterThan    * The specified resource in @Name@ is greater than the specified @Value@ . Not supported for text-based properties.     * GreaterThanOrEqualTo    * The specified resource in @Name@ is greater than or equal to the specified @Value@ . Not supported for text-based properties.     * LessThan    * The specified resource in @Name@ is less than the specified @Value@ . Not supported for text-based properties.     * LessThanOrEqualTo    * The specified resource in @Name@ is less than or equal to the specified @Value@ . Not supported for text-based properties.     * Contains    * Only supported for text-based properties. The word-list of the property contains the specified @Value@ . If you have specified a filter @Value@ , the default is @Equals@ .
fOperator :: Lens' Filter (Maybe Operator)
fOperator = lens _fOperator (\ s a -> s{_fOperator = a})

-- | A value used with @Resource@ and @Operator@ to determine if objects satisfy the filter's condition. For numerical properties, @Value@ must be an integer or floating-point decimal. For timestamp properties, @Value@ must be an ISO 8601 date-time string of the following format: @YYYY-mm-dd'T'HH:MM:SS@ .
fValue :: Lens' Filter (Maybe Text)
fValue = lens _fValue (\ s a -> s{_fValue = a})

-- | A property name. For example, @TrainingJobName@ . For the list of valid property names returned in a search result for each supported resource, see 'TrainingJob' properties. You must specify a valid property name for the resource.
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a})

instance Hashable Filter where

instance NFData Filter where

instance ToJSON Filter where
        toJSON Filter'{..}
          = object
              (catMaybes
                 [("Operator" .=) <$> _fOperator,
                  ("Value" .=) <$> _fValue, Just ("Name" .= _fName)])

-- | Shows the final value for the objective metric for a training job that was launched by a hyperparameter tuning job. You define the objective metric in the @HyperParameterTuningJobObjective@ parameter of 'HyperParameterTuningJobConfig' .
--
--
--
-- /See:/ 'finalHyperParameterTuningJobObjectiveMetric' smart constructor.
data FinalHyperParameterTuningJobObjectiveMetric = FinalHyperParameterTuningJobObjectiveMetric'
  { _fhptjomType       :: !(Maybe HyperParameterTuningJobObjectiveType)
  , _fhptjomMetricName :: !Text
  , _fhptjomValue      :: !Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FinalHyperParameterTuningJobObjectiveMetric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fhptjomType' - Whether to minimize or maximize the objective metric. Valid values are Minimize and Maximize.
--
-- * 'fhptjomMetricName' - The name of the objective metric.
--
-- * 'fhptjomValue' - The value of the objective metric.
finalHyperParameterTuningJobObjectiveMetric
    :: Text -- ^ 'fhptjomMetricName'
    -> Double -- ^ 'fhptjomValue'
    -> FinalHyperParameterTuningJobObjectiveMetric
finalHyperParameterTuningJobObjectiveMetric pMetricName_ pValue_ =
  FinalHyperParameterTuningJobObjectiveMetric'
    { _fhptjomType = Nothing
    , _fhptjomMetricName = pMetricName_
    , _fhptjomValue = pValue_
    }


-- | Whether to minimize or maximize the objective metric. Valid values are Minimize and Maximize.
fhptjomType :: Lens' FinalHyperParameterTuningJobObjectiveMetric (Maybe HyperParameterTuningJobObjectiveType)
fhptjomType = lens _fhptjomType (\ s a -> s{_fhptjomType = a})

-- | The name of the objective metric.
fhptjomMetricName :: Lens' FinalHyperParameterTuningJobObjectiveMetric Text
fhptjomMetricName = lens _fhptjomMetricName (\ s a -> s{_fhptjomMetricName = a})

-- | The value of the objective metric.
fhptjomValue :: Lens' FinalHyperParameterTuningJobObjectiveMetric Double
fhptjomValue = lens _fhptjomValue (\ s a -> s{_fhptjomValue = a})

instance FromJSON
           FinalHyperParameterTuningJobObjectiveMetric
         where
        parseJSON
          = withObject
              "FinalHyperParameterTuningJobObjectiveMetric"
              (\ x ->
                 FinalHyperParameterTuningJobObjectiveMetric' <$>
                   (x .:? "Type") <*> (x .: "MetricName") <*>
                     (x .: "Value"))

instance Hashable
           FinalHyperParameterTuningJobObjectiveMetric
         where

instance NFData
           FinalHyperParameterTuningJobObjectiveMetric
         where

-- | Specifies configuration details for a Git repository in your AWS account.
--
--
--
-- /See:/ 'gitConfig' smart constructor.
data GitConfig = GitConfig'
  { _gcBranch        :: !(Maybe Text)
  , _gcSecretARN     :: !(Maybe Text)
  , _gcRepositoryURL :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GitConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcBranch' - The default branch for the Git repository.
--
-- * 'gcSecretARN' - The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format: @{"username": /UserName/ , "password": /Password/ }@
--
-- * 'gcRepositoryURL' - The URL where the Git repository is located.
gitConfig
    :: Text -- ^ 'gcRepositoryURL'
    -> GitConfig
gitConfig pRepositoryURL_ =
  GitConfig'
    { _gcBranch = Nothing
    , _gcSecretARN = Nothing
    , _gcRepositoryURL = pRepositoryURL_
    }


-- | The default branch for the Git repository.
gcBranch :: Lens' GitConfig (Maybe Text)
gcBranch = lens _gcBranch (\ s a -> s{_gcBranch = a})

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format: @{"username": /UserName/ , "password": /Password/ }@
gcSecretARN :: Lens' GitConfig (Maybe Text)
gcSecretARN = lens _gcSecretARN (\ s a -> s{_gcSecretARN = a})

-- | The URL where the Git repository is located.
gcRepositoryURL :: Lens' GitConfig Text
gcRepositoryURL = lens _gcRepositoryURL (\ s a -> s{_gcRepositoryURL = a})

instance FromJSON GitConfig where
        parseJSON
          = withObject "GitConfig"
              (\ x ->
                 GitConfig' <$>
                   (x .:? "Branch") <*> (x .:? "SecretArn") <*>
                     (x .: "RepositoryUrl"))

instance Hashable GitConfig where

instance NFData GitConfig where

instance ToJSON GitConfig where
        toJSON GitConfig'{..}
          = object
              (catMaybes
                 [("Branch" .=) <$> _gcBranch,
                  ("SecretArn" .=) <$> _gcSecretARN,
                  Just ("RepositoryUrl" .= _gcRepositoryURL)])

-- | Specifies configuration details for a Git repository when the repository is updated.
--
--
--
-- /See:/ 'gitConfigForUpdate' smart constructor.
newtype GitConfigForUpdate = GitConfigForUpdate'
  { _gcfuSecretARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GitConfigForUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfuSecretARN' - The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format: @{"username": /UserName/ , "password": /Password/ }@
gitConfigForUpdate
    :: GitConfigForUpdate
gitConfigForUpdate = GitConfigForUpdate' {_gcfuSecretARN = Nothing}


-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format: @{"username": /UserName/ , "password": /Password/ }@
gcfuSecretARN :: Lens' GitConfigForUpdate (Maybe Text)
gcfuSecretARN = lens _gcfuSecretARN (\ s a -> s{_gcfuSecretARN = a})

instance Hashable GitConfigForUpdate where

instance NFData GitConfigForUpdate where

instance ToJSON GitConfigForUpdate where
        toJSON GitConfigForUpdate'{..}
          = object
              (catMaybes [("SecretArn" .=) <$> _gcfuSecretARN])

-- | Information required for human workers to complete a labeling task.
--
--
--
-- /See:/ 'humanTaskConfig' smart constructor.
data HumanTaskConfig = HumanTaskConfig'
  { _htcTaskKeywords                      :: !(Maybe (List1 Text))
  , _htcPublicWorkforceTaskPrice          :: !(Maybe PublicWorkforceTaskPrice)
  , _htcTaskAvailabilityLifetimeInSeconds :: !(Maybe Nat)
  , _htcMaxConcurrentTaskCount            :: !(Maybe Nat)
  , _htcWorkteamARN                       :: !Text
  , _htcUiConfig                          :: !UiConfig
  , _htcPreHumanTaskLambdaARN             :: !Text
  , _htcTaskTitle                         :: !Text
  , _htcTaskDescription                   :: !Text
  , _htcNumberOfHumanWorkersPerDataObject :: !Nat
  , _htcTaskTimeLimitInSeconds            :: !Nat
  , _htcAnnotationConsolidationConfig     :: !AnnotationConsolidationConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HumanTaskConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'htcTaskKeywords' - Keywords used to describe the task so that workers on Amazon Mechanical Turk can discover the task.
--
-- * 'htcPublicWorkforceTaskPrice' - The price that you pay for each task performed by a public worker.
--
-- * 'htcTaskAvailabilityLifetimeInSeconds' - The length of time that a task remains available for labelling by human workers.
--
-- * 'htcMaxConcurrentTaskCount' - Defines the maximum number of data objects that can be labeled by human workers at the same time. Each object may have more than one worker at one time.
--
-- * 'htcWorkteamARN' - The Amazon Resource Name (ARN) of the work team assigned to complete the tasks.
--
-- * 'htcUiConfig' - Information about the user interface that workers use to complete the labeling task.
--
-- * 'htcPreHumanTaskLambdaARN' - The Amazon Resource Name (ARN) of a Lambda function that is run before a data object is sent to a human worker. Use this function to provide input to a custom labeling job. For the built-in bounding box, image classification, semantic segmentation, and text classification task types, Amazon SageMaker Ground Truth provides the following Lambda functions: __US East (Northern Virginia) (us-east-1):__      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-BoundingBox@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-TextMultiClass@  __US East (Ohio) (us-east-2):__      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-BoundingBox@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-TextMultiClass@  __US West (Oregon) (us-west-2):__      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-BoundingBox@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-TextMultiClass@  __EU (Ireland) (eu-west-1):__      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-BoundingBox@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-ImageMultiClass@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-TextMultiClass@  __Asia Pacific (Tokyo (ap-northeast-1):__      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-TextMultiClass@
--
-- * 'htcTaskTitle' - A title for the task for your human workers.
--
-- * 'htcTaskDescription' - A description of the task for your human workers.
--
-- * 'htcNumberOfHumanWorkersPerDataObject' - The number of human workers that will label an object.
--
-- * 'htcTaskTimeLimitInSeconds' - The amount of time that a worker has to complete a task.
--
-- * 'htcAnnotationConsolidationConfig' - Configures how labels are consolidated across human workers.
humanTaskConfig
    :: Text -- ^ 'htcWorkteamARN'
    -> UiConfig -- ^ 'htcUiConfig'
    -> Text -- ^ 'htcPreHumanTaskLambdaARN'
    -> Text -- ^ 'htcTaskTitle'
    -> Text -- ^ 'htcTaskDescription'
    -> Natural -- ^ 'htcNumberOfHumanWorkersPerDataObject'
    -> Natural -- ^ 'htcTaskTimeLimitInSeconds'
    -> AnnotationConsolidationConfig -- ^ 'htcAnnotationConsolidationConfig'
    -> HumanTaskConfig
humanTaskConfig pWorkteamARN_ pUiConfig_ pPreHumanTaskLambdaARN_ pTaskTitle_ pTaskDescription_ pNumberOfHumanWorkersPerDataObject_ pTaskTimeLimitInSeconds_ pAnnotationConsolidationConfig_ =
  HumanTaskConfig'
    { _htcTaskKeywords = Nothing
    , _htcPublicWorkforceTaskPrice = Nothing
    , _htcTaskAvailabilityLifetimeInSeconds = Nothing
    , _htcMaxConcurrentTaskCount = Nothing
    , _htcWorkteamARN = pWorkteamARN_
    , _htcUiConfig = pUiConfig_
    , _htcPreHumanTaskLambdaARN = pPreHumanTaskLambdaARN_
    , _htcTaskTitle = pTaskTitle_
    , _htcTaskDescription = pTaskDescription_
    , _htcNumberOfHumanWorkersPerDataObject =
        _Nat # pNumberOfHumanWorkersPerDataObject_
    , _htcTaskTimeLimitInSeconds = _Nat # pTaskTimeLimitInSeconds_
    , _htcAnnotationConsolidationConfig = pAnnotationConsolidationConfig_
    }


-- | Keywords used to describe the task so that workers on Amazon Mechanical Turk can discover the task.
htcTaskKeywords :: Lens' HumanTaskConfig (Maybe (NonEmpty Text))
htcTaskKeywords = lens _htcTaskKeywords (\ s a -> s{_htcTaskKeywords = a}) . mapping _List1

-- | The price that you pay for each task performed by a public worker.
htcPublicWorkforceTaskPrice :: Lens' HumanTaskConfig (Maybe PublicWorkforceTaskPrice)
htcPublicWorkforceTaskPrice = lens _htcPublicWorkforceTaskPrice (\ s a -> s{_htcPublicWorkforceTaskPrice = a})

-- | The length of time that a task remains available for labelling by human workers.
htcTaskAvailabilityLifetimeInSeconds :: Lens' HumanTaskConfig (Maybe Natural)
htcTaskAvailabilityLifetimeInSeconds = lens _htcTaskAvailabilityLifetimeInSeconds (\ s a -> s{_htcTaskAvailabilityLifetimeInSeconds = a}) . mapping _Nat

-- | Defines the maximum number of data objects that can be labeled by human workers at the same time. Each object may have more than one worker at one time.
htcMaxConcurrentTaskCount :: Lens' HumanTaskConfig (Maybe Natural)
htcMaxConcurrentTaskCount = lens _htcMaxConcurrentTaskCount (\ s a -> s{_htcMaxConcurrentTaskCount = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the work team assigned to complete the tasks.
htcWorkteamARN :: Lens' HumanTaskConfig Text
htcWorkteamARN = lens _htcWorkteamARN (\ s a -> s{_htcWorkteamARN = a})

-- | Information about the user interface that workers use to complete the labeling task.
htcUiConfig :: Lens' HumanTaskConfig UiConfig
htcUiConfig = lens _htcUiConfig (\ s a -> s{_htcUiConfig = a})

-- | The Amazon Resource Name (ARN) of a Lambda function that is run before a data object is sent to a human worker. Use this function to provide input to a custom labeling job. For the built-in bounding box, image classification, semantic segmentation, and text classification task types, Amazon SageMaker Ground Truth provides the following Lambda functions: __US East (Northern Virginia) (us-east-1):__      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-BoundingBox@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-east-1:432418664414:function:PRE-TextMultiClass@  __US East (Ohio) (us-east-2):__      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-BoundingBox@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-east-2:266458841044:function:PRE-TextMultiClass@  __US West (Oregon) (us-west-2):__      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-BoundingBox@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-ImageMultiClass@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:us-west-2:081040173940:function:PRE-TextMultiClass@  __EU (Ireland) (eu-west-1):__      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-BoundingBox@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-ImageMultiClass@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:eu-west-1:568282634449:function:PRE-TextMultiClass@  __Asia Pacific (Tokyo (ap-northeast-1):__      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-BoundingBox@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-ImageMultiClass@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-SemanticSegmentation@      * @arn:aws:lambda:ap-northeast-1:477331159723:function:PRE-TextMultiClass@
htcPreHumanTaskLambdaARN :: Lens' HumanTaskConfig Text
htcPreHumanTaskLambdaARN = lens _htcPreHumanTaskLambdaARN (\ s a -> s{_htcPreHumanTaskLambdaARN = a})

-- | A title for the task for your human workers.
htcTaskTitle :: Lens' HumanTaskConfig Text
htcTaskTitle = lens _htcTaskTitle (\ s a -> s{_htcTaskTitle = a})

-- | A description of the task for your human workers.
htcTaskDescription :: Lens' HumanTaskConfig Text
htcTaskDescription = lens _htcTaskDescription (\ s a -> s{_htcTaskDescription = a})

-- | The number of human workers that will label an object.
htcNumberOfHumanWorkersPerDataObject :: Lens' HumanTaskConfig Natural
htcNumberOfHumanWorkersPerDataObject = lens _htcNumberOfHumanWorkersPerDataObject (\ s a -> s{_htcNumberOfHumanWorkersPerDataObject = a}) . _Nat

-- | The amount of time that a worker has to complete a task.
htcTaskTimeLimitInSeconds :: Lens' HumanTaskConfig Natural
htcTaskTimeLimitInSeconds = lens _htcTaskTimeLimitInSeconds (\ s a -> s{_htcTaskTimeLimitInSeconds = a}) . _Nat

-- | Configures how labels are consolidated across human workers.
htcAnnotationConsolidationConfig :: Lens' HumanTaskConfig AnnotationConsolidationConfig
htcAnnotationConsolidationConfig = lens _htcAnnotationConsolidationConfig (\ s a -> s{_htcAnnotationConsolidationConfig = a})

instance FromJSON HumanTaskConfig where
        parseJSON
          = withObject "HumanTaskConfig"
              (\ x ->
                 HumanTaskConfig' <$>
                   (x .:? "TaskKeywords") <*>
                     (x .:? "PublicWorkforceTaskPrice")
                     <*> (x .:? "TaskAvailabilityLifetimeInSeconds")
                     <*> (x .:? "MaxConcurrentTaskCount")
                     <*> (x .: "WorkteamArn")
                     <*> (x .: "UiConfig")
                     <*> (x .: "PreHumanTaskLambdaArn")
                     <*> (x .: "TaskTitle")
                     <*> (x .: "TaskDescription")
                     <*> (x .: "NumberOfHumanWorkersPerDataObject")
                     <*> (x .: "TaskTimeLimitInSeconds")
                     <*> (x .: "AnnotationConsolidationConfig"))

instance Hashable HumanTaskConfig where

instance NFData HumanTaskConfig where

instance ToJSON HumanTaskConfig where
        toJSON HumanTaskConfig'{..}
          = object
              (catMaybes
                 [("TaskKeywords" .=) <$> _htcTaskKeywords,
                  ("PublicWorkforceTaskPrice" .=) <$>
                    _htcPublicWorkforceTaskPrice,
                  ("TaskAvailabilityLifetimeInSeconds" .=) <$>
                    _htcTaskAvailabilityLifetimeInSeconds,
                  ("MaxConcurrentTaskCount" .=) <$>
                    _htcMaxConcurrentTaskCount,
                  Just ("WorkteamArn" .= _htcWorkteamARN),
                  Just ("UiConfig" .= _htcUiConfig),
                  Just
                    ("PreHumanTaskLambdaArn" .=
                       _htcPreHumanTaskLambdaARN),
                  Just ("TaskTitle" .= _htcTaskTitle),
                  Just ("TaskDescription" .= _htcTaskDescription),
                  Just
                    ("NumberOfHumanWorkersPerDataObject" .=
                       _htcNumberOfHumanWorkersPerDataObject),
                  Just
                    ("TaskTimeLimitInSeconds" .=
                       _htcTaskTimeLimitInSeconds),
                  Just
                    ("AnnotationConsolidationConfig" .=
                       _htcAnnotationConsolidationConfig)])

-- | Specifies which training algorithm to use for training jobs that a hyperparameter tuning job launches and the metrics to monitor.
--
--
--
-- /See:/ 'hyperParameterAlgorithmSpecification' smart constructor.
data HyperParameterAlgorithmSpecification = HyperParameterAlgorithmSpecification'
  { _hpasAlgorithmName     :: !(Maybe Text)
  , _hpasTrainingImage     :: !(Maybe Text)
  , _hpasMetricDefinitions :: !(Maybe [MetricDefinition])
  , _hpasTrainingInputMode :: !TrainingInputMode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HyperParameterAlgorithmSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpasAlgorithmName' - The name of the resource algorithm to use for the hyperparameter tuning job. If you specify a value for this parameter, do not specify a value for @TrainingImage@ .
--
-- * 'hpasTrainingImage' - The registry path of the Docker image that contains the training algorithm. For information about Docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- * 'hpasMetricDefinitions' - An array of 'MetricDefinition' objects that specify the metrics that the algorithm emits.
--
-- * 'hpasTrainingInputMode' - The input mode that the algorithm supports: File or Pipe. In File input mode, Amazon SageMaker downloads the training data from Amazon S3 to the storage volume that is attached to the training instance and mounts the directory to the Docker volume for the training container. In Pipe input mode, Amazon SageMaker streams data directly from Amazon S3 to the container.  If you specify File mode, make sure that you provision the storage volume that is attached to the training instance with enough capacity to accommodate the training data downloaded from Amazon S3, the model artifacts, and intermediate information. For more information about input modes, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
hyperParameterAlgorithmSpecification
    :: TrainingInputMode -- ^ 'hpasTrainingInputMode'
    -> HyperParameterAlgorithmSpecification
hyperParameterAlgorithmSpecification pTrainingInputMode_ =
  HyperParameterAlgorithmSpecification'
    { _hpasAlgorithmName = Nothing
    , _hpasTrainingImage = Nothing
    , _hpasMetricDefinitions = Nothing
    , _hpasTrainingInputMode = pTrainingInputMode_
    }


-- | The name of the resource algorithm to use for the hyperparameter tuning job. If you specify a value for this parameter, do not specify a value for @TrainingImage@ .
hpasAlgorithmName :: Lens' HyperParameterAlgorithmSpecification (Maybe Text)
hpasAlgorithmName = lens _hpasAlgorithmName (\ s a -> s{_hpasAlgorithmName = a})

-- | The registry path of the Docker image that contains the training algorithm. For information about Docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
hpasTrainingImage :: Lens' HyperParameterAlgorithmSpecification (Maybe Text)
hpasTrainingImage = lens _hpasTrainingImage (\ s a -> s{_hpasTrainingImage = a})

-- | An array of 'MetricDefinition' objects that specify the metrics that the algorithm emits.
hpasMetricDefinitions :: Lens' HyperParameterAlgorithmSpecification [MetricDefinition]
hpasMetricDefinitions = lens _hpasMetricDefinitions (\ s a -> s{_hpasMetricDefinitions = a}) . _Default . _Coerce

-- | The input mode that the algorithm supports: File or Pipe. In File input mode, Amazon SageMaker downloads the training data from Amazon S3 to the storage volume that is attached to the training instance and mounts the directory to the Docker volume for the training container. In Pipe input mode, Amazon SageMaker streams data directly from Amazon S3 to the container.  If you specify File mode, make sure that you provision the storage volume that is attached to the training instance with enough capacity to accommodate the training data downloaded from Amazon S3, the model artifacts, and intermediate information. For more information about input modes, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
hpasTrainingInputMode :: Lens' HyperParameterAlgorithmSpecification TrainingInputMode
hpasTrainingInputMode = lens _hpasTrainingInputMode (\ s a -> s{_hpasTrainingInputMode = a})

instance FromJSON
           HyperParameterAlgorithmSpecification
         where
        parseJSON
          = withObject "HyperParameterAlgorithmSpecification"
              (\ x ->
                 HyperParameterAlgorithmSpecification' <$>
                   (x .:? "AlgorithmName") <*> (x .:? "TrainingImage")
                     <*> (x .:? "MetricDefinitions" .!= mempty)
                     <*> (x .: "TrainingInputMode"))

instance Hashable
           HyperParameterAlgorithmSpecification
         where

instance NFData HyperParameterAlgorithmSpecification
         where

instance ToJSON HyperParameterAlgorithmSpecification
         where
        toJSON HyperParameterAlgorithmSpecification'{..}
          = object
              (catMaybes
                 [("AlgorithmName" .=) <$> _hpasAlgorithmName,
                  ("TrainingImage" .=) <$> _hpasTrainingImage,
                  ("MetricDefinitions" .=) <$> _hpasMetricDefinitions,
                  Just
                    ("TrainingInputMode" .= _hpasTrainingInputMode)])

-- | Defines a hyperparameter to be used by an algorithm.
--
--
--
-- /See:/ 'hyperParameterSpecification' smart constructor.
data HyperParameterSpecification = HyperParameterSpecification'
  { _hpsIsTunable    :: !(Maybe Bool)
  , _hpsRange        :: !(Maybe ParameterRange)
  , _hpsDefaultValue :: !(Maybe Text)
  , _hpsIsRequired   :: !(Maybe Bool)
  , _hpsDescription  :: !(Maybe Text)
  , _hpsName         :: !Text
  , _hpsType         :: !ParameterType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HyperParameterSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpsIsTunable' - Indicates whether this hyperparameter is tunable in a hyperparameter tuning job.
--
-- * 'hpsRange' - The allowed range for this hyperparameter.
--
-- * 'hpsDefaultValue' - The default value for this hyperparameter. If a default value is specified, a hyperparameter cannot be required.
--
-- * 'hpsIsRequired' - Indicates whether this hyperparameter is required.
--
-- * 'hpsDescription' - A brief description of the hyperparameter.
--
-- * 'hpsName' - The name of this hyperparameter. The name must be unique.
--
-- * 'hpsType' - The type of this hyperparameter. The valid types are @Integer@ , @Continuous@ , @Categorical@ , and @FreeText@ .
hyperParameterSpecification
    :: Text -- ^ 'hpsName'
    -> ParameterType -- ^ 'hpsType'
    -> HyperParameterSpecification
hyperParameterSpecification pName_ pType_ =
  HyperParameterSpecification'
    { _hpsIsTunable = Nothing
    , _hpsRange = Nothing
    , _hpsDefaultValue = Nothing
    , _hpsIsRequired = Nothing
    , _hpsDescription = Nothing
    , _hpsName = pName_
    , _hpsType = pType_
    }


-- | Indicates whether this hyperparameter is tunable in a hyperparameter tuning job.
hpsIsTunable :: Lens' HyperParameterSpecification (Maybe Bool)
hpsIsTunable = lens _hpsIsTunable (\ s a -> s{_hpsIsTunable = a})

-- | The allowed range for this hyperparameter.
hpsRange :: Lens' HyperParameterSpecification (Maybe ParameterRange)
hpsRange = lens _hpsRange (\ s a -> s{_hpsRange = a})

-- | The default value for this hyperparameter. If a default value is specified, a hyperparameter cannot be required.
hpsDefaultValue :: Lens' HyperParameterSpecification (Maybe Text)
hpsDefaultValue = lens _hpsDefaultValue (\ s a -> s{_hpsDefaultValue = a})

-- | Indicates whether this hyperparameter is required.
hpsIsRequired :: Lens' HyperParameterSpecification (Maybe Bool)
hpsIsRequired = lens _hpsIsRequired (\ s a -> s{_hpsIsRequired = a})

-- | A brief description of the hyperparameter.
hpsDescription :: Lens' HyperParameterSpecification (Maybe Text)
hpsDescription = lens _hpsDescription (\ s a -> s{_hpsDescription = a})

-- | The name of this hyperparameter. The name must be unique.
hpsName :: Lens' HyperParameterSpecification Text
hpsName = lens _hpsName (\ s a -> s{_hpsName = a})

-- | The type of this hyperparameter. The valid types are @Integer@ , @Continuous@ , @Categorical@ , and @FreeText@ .
hpsType :: Lens' HyperParameterSpecification ParameterType
hpsType = lens _hpsType (\ s a -> s{_hpsType = a})

instance FromJSON HyperParameterSpecification where
        parseJSON
          = withObject "HyperParameterSpecification"
              (\ x ->
                 HyperParameterSpecification' <$>
                   (x .:? "IsTunable") <*> (x .:? "Range") <*>
                     (x .:? "DefaultValue")
                     <*> (x .:? "IsRequired")
                     <*> (x .:? "Description")
                     <*> (x .: "Name")
                     <*> (x .: "Type"))

instance Hashable HyperParameterSpecification where

instance NFData HyperParameterSpecification where

instance ToJSON HyperParameterSpecification where
        toJSON HyperParameterSpecification'{..}
          = object
              (catMaybes
                 [("IsTunable" .=) <$> _hpsIsTunable,
                  ("Range" .=) <$> _hpsRange,
                  ("DefaultValue" .=) <$> _hpsDefaultValue,
                  ("IsRequired" .=) <$> _hpsIsRequired,
                  ("Description" .=) <$> _hpsDescription,
                  Just ("Name" .= _hpsName),
                  Just ("Type" .= _hpsType)])

-- | Defines the training jobs launched by a hyperparameter tuning job.
--
--
--
-- /See:/ 'hyperParameterTrainingJobDefinition' smart constructor.
data HyperParameterTrainingJobDefinition = HyperParameterTrainingJobDefinition'
  { _hptjdEnableNetworkIsolation :: !(Maybe Bool)
  , _hptjdStaticHyperParameters :: !(Maybe (Map Text Text))
  , _hptjdInputDataConfig :: !(Maybe (List1 Channel))
  , _hptjdVPCConfig :: !(Maybe VPCConfig)
  , _hptjdEnableInterContainerTrafficEncryption :: !(Maybe Bool)
  , _hptjdAlgorithmSpecification :: !HyperParameterAlgorithmSpecification
  , _hptjdRoleARN :: !Text
  , _hptjdOutputDataConfig :: !OutputDataConfig
  , _hptjdResourceConfig :: !ResourceConfig
  , _hptjdStoppingCondition :: !StoppingCondition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HyperParameterTrainingJobDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hptjdEnableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If network isolation is used for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- * 'hptjdStaticHyperParameters' - Specifies the values of hyperparameters that do not change for the tuning job.
--
-- * 'hptjdInputDataConfig' - An array of 'Channel' objects that specify the input for the training jobs that the tuning job launches.
--
-- * 'hptjdVPCConfig' - The 'VpcConfig' object that specifies the VPC that you want the training jobs that this hyperparameter tuning job launches to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- * 'hptjdEnableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
--
-- * 'hptjdAlgorithmSpecification' - The 'HyperParameterAlgorithmSpecification' object that specifies the resource algorithm to use for the training jobs that the tuning job launches.
--
-- * 'hptjdRoleARN' - The Amazon Resource Name (ARN) of the IAM role associated with the training jobs that the tuning job launches.
--
-- * 'hptjdOutputDataConfig' - Specifies the path to the Amazon S3 bucket where you store model artifacts from the training jobs that the tuning job launches.
--
-- * 'hptjdResourceConfig' - The resources, including the compute instances and storage volumes, to use for the training jobs that the tuning job launches. Storage volumes store model artifacts and incremental states. Training algorithms might also use storage volumes for scratch space. If you want Amazon SageMaker to use the storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
--
-- * 'hptjdStoppingCondition' - Sets a maximum duration for the training jobs that the tuning job launches. Use this parameter to limit model training costs.  To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal. This delays job termination for 120 seconds. Algorithms might use this 120-second window to save the model artifacts. When Amazon SageMaker terminates a job because the stopping condition has been met, training algorithms provided by Amazon SageMaker save the intermediate results of the job.
hyperParameterTrainingJobDefinition
    :: HyperParameterAlgorithmSpecification -- ^ 'hptjdAlgorithmSpecification'
    -> Text -- ^ 'hptjdRoleARN'
    -> OutputDataConfig -- ^ 'hptjdOutputDataConfig'
    -> ResourceConfig -- ^ 'hptjdResourceConfig'
    -> StoppingCondition -- ^ 'hptjdStoppingCondition'
    -> HyperParameterTrainingJobDefinition
hyperParameterTrainingJobDefinition pAlgorithmSpecification_ pRoleARN_ pOutputDataConfig_ pResourceConfig_ pStoppingCondition_ =
  HyperParameterTrainingJobDefinition'
    { _hptjdEnableNetworkIsolation = Nothing
    , _hptjdStaticHyperParameters = Nothing
    , _hptjdInputDataConfig = Nothing
    , _hptjdVPCConfig = Nothing
    , _hptjdEnableInterContainerTrafficEncryption = Nothing
    , _hptjdAlgorithmSpecification = pAlgorithmSpecification_
    , _hptjdRoleARN = pRoleARN_
    , _hptjdOutputDataConfig = pOutputDataConfig_
    , _hptjdResourceConfig = pResourceConfig_
    , _hptjdStoppingCondition = pStoppingCondition_
    }


-- | Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If network isolation is used for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
hptjdEnableNetworkIsolation :: Lens' HyperParameterTrainingJobDefinition (Maybe Bool)
hptjdEnableNetworkIsolation = lens _hptjdEnableNetworkIsolation (\ s a -> s{_hptjdEnableNetworkIsolation = a})

-- | Specifies the values of hyperparameters that do not change for the tuning job.
hptjdStaticHyperParameters :: Lens' HyperParameterTrainingJobDefinition (HashMap Text Text)
hptjdStaticHyperParameters = lens _hptjdStaticHyperParameters (\ s a -> s{_hptjdStaticHyperParameters = a}) . _Default . _Map

-- | An array of 'Channel' objects that specify the input for the training jobs that the tuning job launches.
hptjdInputDataConfig :: Lens' HyperParameterTrainingJobDefinition (Maybe (NonEmpty Channel))
hptjdInputDataConfig = lens _hptjdInputDataConfig (\ s a -> s{_hptjdInputDataConfig = a}) . mapping _List1

-- | The 'VpcConfig' object that specifies the VPC that you want the training jobs that this hyperparameter tuning job launches to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
hptjdVPCConfig :: Lens' HyperParameterTrainingJobDefinition (Maybe VPCConfig)
hptjdVPCConfig = lens _hptjdVPCConfig (\ s a -> s{_hptjdVPCConfig = a})

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
hptjdEnableInterContainerTrafficEncryption :: Lens' HyperParameterTrainingJobDefinition (Maybe Bool)
hptjdEnableInterContainerTrafficEncryption = lens _hptjdEnableInterContainerTrafficEncryption (\ s a -> s{_hptjdEnableInterContainerTrafficEncryption = a})

-- | The 'HyperParameterAlgorithmSpecification' object that specifies the resource algorithm to use for the training jobs that the tuning job launches.
hptjdAlgorithmSpecification :: Lens' HyperParameterTrainingJobDefinition HyperParameterAlgorithmSpecification
hptjdAlgorithmSpecification = lens _hptjdAlgorithmSpecification (\ s a -> s{_hptjdAlgorithmSpecification = a})

-- | The Amazon Resource Name (ARN) of the IAM role associated with the training jobs that the tuning job launches.
hptjdRoleARN :: Lens' HyperParameterTrainingJobDefinition Text
hptjdRoleARN = lens _hptjdRoleARN (\ s a -> s{_hptjdRoleARN = a})

-- | Specifies the path to the Amazon S3 bucket where you store model artifacts from the training jobs that the tuning job launches.
hptjdOutputDataConfig :: Lens' HyperParameterTrainingJobDefinition OutputDataConfig
hptjdOutputDataConfig = lens _hptjdOutputDataConfig (\ s a -> s{_hptjdOutputDataConfig = a})

-- | The resources, including the compute instances and storage volumes, to use for the training jobs that the tuning job launches. Storage volumes store model artifacts and incremental states. Training algorithms might also use storage volumes for scratch space. If you want Amazon SageMaker to use the storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
hptjdResourceConfig :: Lens' HyperParameterTrainingJobDefinition ResourceConfig
hptjdResourceConfig = lens _hptjdResourceConfig (\ s a -> s{_hptjdResourceConfig = a})

-- | Sets a maximum duration for the training jobs that the tuning job launches. Use this parameter to limit model training costs.  To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal. This delays job termination for 120 seconds. Algorithms might use this 120-second window to save the model artifacts. When Amazon SageMaker terminates a job because the stopping condition has been met, training algorithms provided by Amazon SageMaker save the intermediate results of the job.
hptjdStoppingCondition :: Lens' HyperParameterTrainingJobDefinition StoppingCondition
hptjdStoppingCondition = lens _hptjdStoppingCondition (\ s a -> s{_hptjdStoppingCondition = a})

instance FromJSON HyperParameterTrainingJobDefinition
         where
        parseJSON
          = withObject "HyperParameterTrainingJobDefinition"
              (\ x ->
                 HyperParameterTrainingJobDefinition' <$>
                   (x .:? "EnableNetworkIsolation") <*>
                     (x .:? "StaticHyperParameters" .!= mempty)
                     <*> (x .:? "InputDataConfig")
                     <*> (x .:? "VpcConfig")
                     <*> (x .:? "EnableInterContainerTrafficEncryption")
                     <*> (x .: "AlgorithmSpecification")
                     <*> (x .: "RoleArn")
                     <*> (x .: "OutputDataConfig")
                     <*> (x .: "ResourceConfig")
                     <*> (x .: "StoppingCondition"))

instance Hashable HyperParameterTrainingJobDefinition
         where

instance NFData HyperParameterTrainingJobDefinition
         where

instance ToJSON HyperParameterTrainingJobDefinition
         where
        toJSON HyperParameterTrainingJobDefinition'{..}
          = object
              (catMaybes
                 [("EnableNetworkIsolation" .=) <$>
                    _hptjdEnableNetworkIsolation,
                  ("StaticHyperParameters" .=) <$>
                    _hptjdStaticHyperParameters,
                  ("InputDataConfig" .=) <$> _hptjdInputDataConfig,
                  ("VpcConfig" .=) <$> _hptjdVPCConfig,
                  ("EnableInterContainerTrafficEncryption" .=) <$>
                    _hptjdEnableInterContainerTrafficEncryption,
                  Just
                    ("AlgorithmSpecification" .=
                       _hptjdAlgorithmSpecification),
                  Just ("RoleArn" .= _hptjdRoleARN),
                  Just ("OutputDataConfig" .= _hptjdOutputDataConfig),
                  Just ("ResourceConfig" .= _hptjdResourceConfig),
                  Just
                    ("StoppingCondition" .= _hptjdStoppingCondition)])

-- | Specifies summary information about a training job.
--
--
--
-- /See:/ 'hyperParameterTrainingJobSummary' smart constructor.
data HyperParameterTrainingJobSummary = HyperParameterTrainingJobSummary'
  { _hptjsFailureReason :: !(Maybe Text)
  , _hptjsTuningJobName :: !(Maybe Text)
  , _hptjsTrainingEndTime :: !(Maybe POSIX)
  , _hptjsObjectiveStatus :: !(Maybe ObjectiveStatus)
  , _hptjsTrainingStartTime :: !(Maybe POSIX)
  , _hptjsFinalHyperParameterTuningJobObjectiveMetric :: !(Maybe FinalHyperParameterTuningJobObjectiveMetric)
  , _hptjsTrainingJobName :: !Text
  , _hptjsTrainingJobARN :: !Text
  , _hptjsCreationTime :: !POSIX
  , _hptjsTrainingJobStatus :: !TrainingJobStatus
  , _hptjsTunedHyperParameters :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HyperParameterTrainingJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hptjsFailureReason' - The reason that the training job failed.
--
-- * 'hptjsTuningJobName' - The HyperParameter tuning job that launched the training job.
--
-- * 'hptjsTrainingEndTime' - Specifies the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
--
-- * 'hptjsObjectiveStatus' - The status of the objective metric for the training job:     * Succeeded: The final objective metric for the training job was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.     * Pending: The training job is in progress and evaluation of its final objective metric is pending.     * Failed: The final objective metric for the training job was not evaluated, and was not used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
--
-- * 'hptjsTrainingStartTime' - The date and time that the training job started.
--
-- * 'hptjsFinalHyperParameterTuningJobObjectiveMetric' - The 'FinalHyperParameterTuningJobObjectiveMetric' object that specifies the value of the objective metric of the tuning job that launched this training job.
--
-- * 'hptjsTrainingJobName' - The name of the training job.
--
-- * 'hptjsTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
--
-- * 'hptjsCreationTime' - The date and time that the training job was created.
--
-- * 'hptjsTrainingJobStatus' - The status of the training job.
--
-- * 'hptjsTunedHyperParameters' - A list of the hyperparameters for which you specified ranges to search.
hyperParameterTrainingJobSummary
    :: Text -- ^ 'hptjsTrainingJobName'
    -> Text -- ^ 'hptjsTrainingJobARN'
    -> UTCTime -- ^ 'hptjsCreationTime'
    -> TrainingJobStatus -- ^ 'hptjsTrainingJobStatus'
    -> HyperParameterTrainingJobSummary
hyperParameterTrainingJobSummary pTrainingJobName_ pTrainingJobARN_ pCreationTime_ pTrainingJobStatus_ =
  HyperParameterTrainingJobSummary'
    { _hptjsFailureReason = Nothing
    , _hptjsTuningJobName = Nothing
    , _hptjsTrainingEndTime = Nothing
    , _hptjsObjectiveStatus = Nothing
    , _hptjsTrainingStartTime = Nothing
    , _hptjsFinalHyperParameterTuningJobObjectiveMetric = Nothing
    , _hptjsTrainingJobName = pTrainingJobName_
    , _hptjsTrainingJobARN = pTrainingJobARN_
    , _hptjsCreationTime = _Time # pCreationTime_
    , _hptjsTrainingJobStatus = pTrainingJobStatus_
    , _hptjsTunedHyperParameters = mempty
    }


-- | The reason that the training job failed.
hptjsFailureReason :: Lens' HyperParameterTrainingJobSummary (Maybe Text)
hptjsFailureReason = lens _hptjsFailureReason (\ s a -> s{_hptjsFailureReason = a})

-- | The HyperParameter tuning job that launched the training job.
hptjsTuningJobName :: Lens' HyperParameterTrainingJobSummary (Maybe Text)
hptjsTuningJobName = lens _hptjsTuningJobName (\ s a -> s{_hptjsTuningJobName = a})

-- | Specifies the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
hptjsTrainingEndTime :: Lens' HyperParameterTrainingJobSummary (Maybe UTCTime)
hptjsTrainingEndTime = lens _hptjsTrainingEndTime (\ s a -> s{_hptjsTrainingEndTime = a}) . mapping _Time

-- | The status of the objective metric for the training job:     * Succeeded: The final objective metric for the training job was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.     * Pending: The training job is in progress and evaluation of its final objective metric is pending.     * Failed: The final objective metric for the training job was not evaluated, and was not used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
hptjsObjectiveStatus :: Lens' HyperParameterTrainingJobSummary (Maybe ObjectiveStatus)
hptjsObjectiveStatus = lens _hptjsObjectiveStatus (\ s a -> s{_hptjsObjectiveStatus = a})

-- | The date and time that the training job started.
hptjsTrainingStartTime :: Lens' HyperParameterTrainingJobSummary (Maybe UTCTime)
hptjsTrainingStartTime = lens _hptjsTrainingStartTime (\ s a -> s{_hptjsTrainingStartTime = a}) . mapping _Time

-- | The 'FinalHyperParameterTuningJobObjectiveMetric' object that specifies the value of the objective metric of the tuning job that launched this training job.
hptjsFinalHyperParameterTuningJobObjectiveMetric :: Lens' HyperParameterTrainingJobSummary (Maybe FinalHyperParameterTuningJobObjectiveMetric)
hptjsFinalHyperParameterTuningJobObjectiveMetric = lens _hptjsFinalHyperParameterTuningJobObjectiveMetric (\ s a -> s{_hptjsFinalHyperParameterTuningJobObjectiveMetric = a})

-- | The name of the training job.
hptjsTrainingJobName :: Lens' HyperParameterTrainingJobSummary Text
hptjsTrainingJobName = lens _hptjsTrainingJobName (\ s a -> s{_hptjsTrainingJobName = a})

-- | The Amazon Resource Name (ARN) of the training job.
hptjsTrainingJobARN :: Lens' HyperParameterTrainingJobSummary Text
hptjsTrainingJobARN = lens _hptjsTrainingJobARN (\ s a -> s{_hptjsTrainingJobARN = a})

-- | The date and time that the training job was created.
hptjsCreationTime :: Lens' HyperParameterTrainingJobSummary UTCTime
hptjsCreationTime = lens _hptjsCreationTime (\ s a -> s{_hptjsCreationTime = a}) . _Time

-- | The status of the training job.
hptjsTrainingJobStatus :: Lens' HyperParameterTrainingJobSummary TrainingJobStatus
hptjsTrainingJobStatus = lens _hptjsTrainingJobStatus (\ s a -> s{_hptjsTrainingJobStatus = a})

-- | A list of the hyperparameters for which you specified ranges to search.
hptjsTunedHyperParameters :: Lens' HyperParameterTrainingJobSummary (HashMap Text Text)
hptjsTunedHyperParameters = lens _hptjsTunedHyperParameters (\ s a -> s{_hptjsTunedHyperParameters = a}) . _Map

instance FromJSON HyperParameterTrainingJobSummary
         where
        parseJSON
          = withObject "HyperParameterTrainingJobSummary"
              (\ x ->
                 HyperParameterTrainingJobSummary' <$>
                   (x .:? "FailureReason") <*> (x .:? "TuningJobName")
                     <*> (x .:? "TrainingEndTime")
                     <*> (x .:? "ObjectiveStatus")
                     <*> (x .:? "TrainingStartTime")
                     <*>
                     (x .:? "FinalHyperParameterTuningJobObjectiveMetric")
                     <*> (x .: "TrainingJobName")
                     <*> (x .: "TrainingJobArn")
                     <*> (x .: "CreationTime")
                     <*> (x .: "TrainingJobStatus")
                     <*> (x .:? "TunedHyperParameters" .!= mempty))

instance Hashable HyperParameterTrainingJobSummary
         where

instance NFData HyperParameterTrainingJobSummary
         where

-- | Configures a hyperparameter tuning job.
--
--
--
-- /See:/ 'hyperParameterTuningJobConfig' smart constructor.
data HyperParameterTuningJobConfig = HyperParameterTuningJobConfig'
  { _hptjcTrainingJobEarlyStoppingType :: !(Maybe TrainingJobEarlyStoppingType)
  , _hptjcStrategy :: !HyperParameterTuningJobStrategyType
  , _hptjcHyperParameterTuningJobObjective :: !HyperParameterTuningJobObjective
  , _hptjcResourceLimits :: !ResourceLimits
  , _hptjcParameterRanges :: !ParameterRanges
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HyperParameterTuningJobConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hptjcTrainingJobEarlyStoppingType' - Specifies whether to use early stopping for training jobs launched by the hyperparameter tuning job. This can be one of the following values (the default value is @OFF@ ):     * OFF    * Training jobs launched by the hyperparameter tuning job do not use early stopping.     * AUTO    * Amazon SageMaker stops training jobs launched by the hyperparameter tuning job when they are unlikely to perform better than previously completed training jobs. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early> .
--
-- * 'hptjcStrategy' - Specifies how hyperparameter tuning chooses the combinations of hyperparameter values to use for the training job it launches. To use the Bayesian search stategy, set this to @Bayesian@ . To randomly search, set it to @Random@ . For information about search strategies, see <http://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works> .
--
-- * 'hptjcHyperParameterTuningJobObjective' - The 'HyperParameterTuningJobObjective' object that specifies the objective metric for this tuning job.
--
-- * 'hptjcResourceLimits' - The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs for this tuning job.
--
-- * 'hptjcParameterRanges' - The 'ParameterRanges' object that specifies the ranges of hyperparameters that this tuning job searches.
hyperParameterTuningJobConfig
    :: HyperParameterTuningJobStrategyType -- ^ 'hptjcStrategy'
    -> HyperParameterTuningJobObjective -- ^ 'hptjcHyperParameterTuningJobObjective'
    -> ResourceLimits -- ^ 'hptjcResourceLimits'
    -> ParameterRanges -- ^ 'hptjcParameterRanges'
    -> HyperParameterTuningJobConfig
hyperParameterTuningJobConfig pStrategy_ pHyperParameterTuningJobObjective_ pResourceLimits_ pParameterRanges_ =
  HyperParameterTuningJobConfig'
    { _hptjcTrainingJobEarlyStoppingType = Nothing
    , _hptjcStrategy = pStrategy_
    , _hptjcHyperParameterTuningJobObjective =
        pHyperParameterTuningJobObjective_
    , _hptjcResourceLimits = pResourceLimits_
    , _hptjcParameterRanges = pParameterRanges_
    }


-- | Specifies whether to use early stopping for training jobs launched by the hyperparameter tuning job. This can be one of the following values (the default value is @OFF@ ):     * OFF    * Training jobs launched by the hyperparameter tuning job do not use early stopping.     * AUTO    * Amazon SageMaker stops training jobs launched by the hyperparameter tuning job when they are unlikely to perform better than previously completed training jobs. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early> .
hptjcTrainingJobEarlyStoppingType :: Lens' HyperParameterTuningJobConfig (Maybe TrainingJobEarlyStoppingType)
hptjcTrainingJobEarlyStoppingType = lens _hptjcTrainingJobEarlyStoppingType (\ s a -> s{_hptjcTrainingJobEarlyStoppingType = a})

-- | Specifies how hyperparameter tuning chooses the combinations of hyperparameter values to use for the training job it launches. To use the Bayesian search stategy, set this to @Bayesian@ . To randomly search, set it to @Random@ . For information about search strategies, see <http://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works> .
hptjcStrategy :: Lens' HyperParameterTuningJobConfig HyperParameterTuningJobStrategyType
hptjcStrategy = lens _hptjcStrategy (\ s a -> s{_hptjcStrategy = a})

-- | The 'HyperParameterTuningJobObjective' object that specifies the objective metric for this tuning job.
hptjcHyperParameterTuningJobObjective :: Lens' HyperParameterTuningJobConfig HyperParameterTuningJobObjective
hptjcHyperParameterTuningJobObjective = lens _hptjcHyperParameterTuningJobObjective (\ s a -> s{_hptjcHyperParameterTuningJobObjective = a})

-- | The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs for this tuning job.
hptjcResourceLimits :: Lens' HyperParameterTuningJobConfig ResourceLimits
hptjcResourceLimits = lens _hptjcResourceLimits (\ s a -> s{_hptjcResourceLimits = a})

-- | The 'ParameterRanges' object that specifies the ranges of hyperparameters that this tuning job searches.
hptjcParameterRanges :: Lens' HyperParameterTuningJobConfig ParameterRanges
hptjcParameterRanges = lens _hptjcParameterRanges (\ s a -> s{_hptjcParameterRanges = a})

instance FromJSON HyperParameterTuningJobConfig where
        parseJSON
          = withObject "HyperParameterTuningJobConfig"
              (\ x ->
                 HyperParameterTuningJobConfig' <$>
                   (x .:? "TrainingJobEarlyStoppingType") <*>
                     (x .: "Strategy")
                     <*> (x .: "HyperParameterTuningJobObjective")
                     <*> (x .: "ResourceLimits")
                     <*> (x .: "ParameterRanges"))

instance Hashable HyperParameterTuningJobConfig where

instance NFData HyperParameterTuningJobConfig where

instance ToJSON HyperParameterTuningJobConfig where
        toJSON HyperParameterTuningJobConfig'{..}
          = object
              (catMaybes
                 [("TrainingJobEarlyStoppingType" .=) <$>
                    _hptjcTrainingJobEarlyStoppingType,
                  Just ("Strategy" .= _hptjcStrategy),
                  Just
                    ("HyperParameterTuningJobObjective" .=
                       _hptjcHyperParameterTuningJobObjective),
                  Just ("ResourceLimits" .= _hptjcResourceLimits),
                  Just ("ParameterRanges" .= _hptjcParameterRanges)])

-- | Defines the objective metric for a hyperparameter tuning job. Hyperparameter tuning uses the value of this metric to evaluate the training jobs it launches, and returns the training job that results in either the highest or lowest value for this metric, depending on the value you specify for the @Type@ parameter.
--
--
--
-- /See:/ 'hyperParameterTuningJobObjective' smart constructor.
data HyperParameterTuningJobObjective = HyperParameterTuningJobObjective'
  { _hptjoType       :: !HyperParameterTuningJobObjectiveType
  , _hptjoMetricName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HyperParameterTuningJobObjective' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hptjoType' - Whether to minimize or maximize the objective metric.
--
-- * 'hptjoMetricName' - The name of the metric to use for the objective metric.
hyperParameterTuningJobObjective
    :: HyperParameterTuningJobObjectiveType -- ^ 'hptjoType'
    -> Text -- ^ 'hptjoMetricName'
    -> HyperParameterTuningJobObjective
hyperParameterTuningJobObjective pType_ pMetricName_ =
  HyperParameterTuningJobObjective'
    {_hptjoType = pType_, _hptjoMetricName = pMetricName_}


-- | Whether to minimize or maximize the objective metric.
hptjoType :: Lens' HyperParameterTuningJobObjective HyperParameterTuningJobObjectiveType
hptjoType = lens _hptjoType (\ s a -> s{_hptjoType = a})

-- | The name of the metric to use for the objective metric.
hptjoMetricName :: Lens' HyperParameterTuningJobObjective Text
hptjoMetricName = lens _hptjoMetricName (\ s a -> s{_hptjoMetricName = a})

instance FromJSON HyperParameterTuningJobObjective
         where
        parseJSON
          = withObject "HyperParameterTuningJobObjective"
              (\ x ->
                 HyperParameterTuningJobObjective' <$>
                   (x .: "Type") <*> (x .: "MetricName"))

instance Hashable HyperParameterTuningJobObjective
         where

instance NFData HyperParameterTuningJobObjective
         where

instance ToJSON HyperParameterTuningJobObjective
         where
        toJSON HyperParameterTuningJobObjective'{..}
          = object
              (catMaybes
                 [Just ("Type" .= _hptjoType),
                  Just ("MetricName" .= _hptjoMetricName)])

-- | Provides summary information about a hyperparameter tuning job.
--
--
--
-- /See:/ 'hyperParameterTuningJobSummary' smart constructor.
data HyperParameterTuningJobSummary = HyperParameterTuningJobSummary'
  { _hResourceLimits                :: !(Maybe ResourceLimits)
  , _hLastModifiedTime              :: !(Maybe POSIX)
  , _hHyperParameterTuningEndTime   :: !(Maybe POSIX)
  , _hHyperParameterTuningJobName   :: !Text
  , _hHyperParameterTuningJobARN    :: !Text
  , _hHyperParameterTuningJobStatus :: !HyperParameterTuningJobStatus
  , _hStrategy                      :: !HyperParameterTuningJobStrategyType
  , _hCreationTime                  :: !POSIX
  , _hTrainingJobStatusCounters     :: !TrainingJobStatusCounters
  , _hObjectiveStatusCounters       :: !ObjectiveStatusCounters
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HyperParameterTuningJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hResourceLimits' - The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs allowed for this tuning job.
--
-- * 'hLastModifiedTime' - The date and time that the tuning job was modified.
--
-- * 'hHyperParameterTuningEndTime' - The date and time that the tuning job ended.
--
-- * 'hHyperParameterTuningJobName' - The name of the tuning job.
--
-- * 'hHyperParameterTuningJobARN' - The Amazon Resource Name (ARN) of the tuning job.
--
-- * 'hHyperParameterTuningJobStatus' - The status of the tuning job.
--
-- * 'hStrategy' - Specifies the search strategy hyperparameter tuning uses to choose which hyperparameters to use for each iteration. Currently, the only valid value is Bayesian.
--
-- * 'hCreationTime' - The date and time that the tuning job was created.
--
-- * 'hTrainingJobStatusCounters' - The 'TrainingJobStatusCounters' object that specifies the numbers of training jobs, categorized by status, that this tuning job launched.
--
-- * 'hObjectiveStatusCounters' - The 'ObjectiveStatusCounters' object that specifies the numbers of training jobs, categorized by objective metric status, that this tuning job launched.
hyperParameterTuningJobSummary
    :: Text -- ^ 'hHyperParameterTuningJobName'
    -> Text -- ^ 'hHyperParameterTuningJobARN'
    -> HyperParameterTuningJobStatus -- ^ 'hHyperParameterTuningJobStatus'
    -> HyperParameterTuningJobStrategyType -- ^ 'hStrategy'
    -> UTCTime -- ^ 'hCreationTime'
    -> TrainingJobStatusCounters -- ^ 'hTrainingJobStatusCounters'
    -> ObjectiveStatusCounters -- ^ 'hObjectiveStatusCounters'
    -> HyperParameterTuningJobSummary
hyperParameterTuningJobSummary pHyperParameterTuningJobName_ pHyperParameterTuningJobARN_ pHyperParameterTuningJobStatus_ pStrategy_ pCreationTime_ pTrainingJobStatusCounters_ pObjectiveStatusCounters_ =
  HyperParameterTuningJobSummary'
    { _hResourceLimits = Nothing
    , _hLastModifiedTime = Nothing
    , _hHyperParameterTuningEndTime = Nothing
    , _hHyperParameterTuningJobName = pHyperParameterTuningJobName_
    , _hHyperParameterTuningJobARN = pHyperParameterTuningJobARN_
    , _hHyperParameterTuningJobStatus = pHyperParameterTuningJobStatus_
    , _hStrategy = pStrategy_
    , _hCreationTime = _Time # pCreationTime_
    , _hTrainingJobStatusCounters = pTrainingJobStatusCounters_
    , _hObjectiveStatusCounters = pObjectiveStatusCounters_
    }


-- | The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs allowed for this tuning job.
hResourceLimits :: Lens' HyperParameterTuningJobSummary (Maybe ResourceLimits)
hResourceLimits = lens _hResourceLimits (\ s a -> s{_hResourceLimits = a})

-- | The date and time that the tuning job was modified.
hLastModifiedTime :: Lens' HyperParameterTuningJobSummary (Maybe UTCTime)
hLastModifiedTime = lens _hLastModifiedTime (\ s a -> s{_hLastModifiedTime = a}) . mapping _Time

-- | The date and time that the tuning job ended.
hHyperParameterTuningEndTime :: Lens' HyperParameterTuningJobSummary (Maybe UTCTime)
hHyperParameterTuningEndTime = lens _hHyperParameterTuningEndTime (\ s a -> s{_hHyperParameterTuningEndTime = a}) . mapping _Time

-- | The name of the tuning job.
hHyperParameterTuningJobName :: Lens' HyperParameterTuningJobSummary Text
hHyperParameterTuningJobName = lens _hHyperParameterTuningJobName (\ s a -> s{_hHyperParameterTuningJobName = a})

-- | The Amazon Resource Name (ARN) of the tuning job.
hHyperParameterTuningJobARN :: Lens' HyperParameterTuningJobSummary Text
hHyperParameterTuningJobARN = lens _hHyperParameterTuningJobARN (\ s a -> s{_hHyperParameterTuningJobARN = a})

-- | The status of the tuning job.
hHyperParameterTuningJobStatus :: Lens' HyperParameterTuningJobSummary HyperParameterTuningJobStatus
hHyperParameterTuningJobStatus = lens _hHyperParameterTuningJobStatus (\ s a -> s{_hHyperParameterTuningJobStatus = a})

-- | Specifies the search strategy hyperparameter tuning uses to choose which hyperparameters to use for each iteration. Currently, the only valid value is Bayesian.
hStrategy :: Lens' HyperParameterTuningJobSummary HyperParameterTuningJobStrategyType
hStrategy = lens _hStrategy (\ s a -> s{_hStrategy = a})

-- | The date and time that the tuning job was created.
hCreationTime :: Lens' HyperParameterTuningJobSummary UTCTime
hCreationTime = lens _hCreationTime (\ s a -> s{_hCreationTime = a}) . _Time

-- | The 'TrainingJobStatusCounters' object that specifies the numbers of training jobs, categorized by status, that this tuning job launched.
hTrainingJobStatusCounters :: Lens' HyperParameterTuningJobSummary TrainingJobStatusCounters
hTrainingJobStatusCounters = lens _hTrainingJobStatusCounters (\ s a -> s{_hTrainingJobStatusCounters = a})

-- | The 'ObjectiveStatusCounters' object that specifies the numbers of training jobs, categorized by objective metric status, that this tuning job launched.
hObjectiveStatusCounters :: Lens' HyperParameterTuningJobSummary ObjectiveStatusCounters
hObjectiveStatusCounters = lens _hObjectiveStatusCounters (\ s a -> s{_hObjectiveStatusCounters = a})

instance FromJSON HyperParameterTuningJobSummary
         where
        parseJSON
          = withObject "HyperParameterTuningJobSummary"
              (\ x ->
                 HyperParameterTuningJobSummary' <$>
                   (x .:? "ResourceLimits") <*>
                     (x .:? "LastModifiedTime")
                     <*> (x .:? "HyperParameterTuningEndTime")
                     <*> (x .: "HyperParameterTuningJobName")
                     <*> (x .: "HyperParameterTuningJobArn")
                     <*> (x .: "HyperParameterTuningJobStatus")
                     <*> (x .: "Strategy")
                     <*> (x .: "CreationTime")
                     <*> (x .: "TrainingJobStatusCounters")
                     <*> (x .: "ObjectiveStatusCounters"))

instance Hashable HyperParameterTuningJobSummary
         where

instance NFData HyperParameterTuningJobSummary where

-- | Specifies the configuration for a hyperparameter tuning job that uses one or more previous hyperparameter tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
--
--
-- All training jobs launched by the new hyperparameter tuning job are evaluated by using the objective metric, and the training job that performs the best is compared to the best training jobs from the parent tuning jobs. From these, the training job that performs the best as measured by the objective metric is returned as the overall best training job.
--
--
-- /See:/ 'hyperParameterTuningJobWarmStartConfig' smart constructor.
data HyperParameterTuningJobWarmStartConfig = HyperParameterTuningJobWarmStartConfig'
  { _hptjwscParentHyperParameterTuningJobs :: !(List1 ParentHyperParameterTuningJob)
  , _hptjwscWarmStartType :: !HyperParameterTuningJobWarmStartType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HyperParameterTuningJobWarmStartConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hptjwscParentHyperParameterTuningJobs' - An array of hyperparameter tuning jobs that are used as the starting point for the new hyperparameter tuning job. For more information about warm starting a hyperparameter tuning job, see <http://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point> . Hyperparameter tuning jobs created before October 1, 2018 cannot be used as parent jobs for warm start tuning jobs.
--
-- * 'hptjwscWarmStartType' - Specifies one of the following:     * IDENTICAL_DATA_AND_ALGORITHM    * The new hyperparameter tuning job uses the same input data and training image as the parent tuning jobs. You can change the hyperparameter ranges to search and the maximum number of training jobs that the hyperparameter tuning job launches. You cannot use a new version of the training algorithm, unless the changes in the new version do not affect the algorithm itself. For example, changes that improve logging or adding support for a different data format are allowed. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.     * TRANSFER_LEARNING    * The new hyperparameter tuning job can include input data, hyperparameter ranges, maximum number of concurrent training jobs, and maximum number of training jobs that are different than those of its parent hyperparameter tuning jobs. The training image can also be a different version from the version used in the parent hyperparameter tuning job. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.
hyperParameterTuningJobWarmStartConfig
    :: NonEmpty ParentHyperParameterTuningJob -- ^ 'hptjwscParentHyperParameterTuningJobs'
    -> HyperParameterTuningJobWarmStartType -- ^ 'hptjwscWarmStartType'
    -> HyperParameterTuningJobWarmStartConfig
hyperParameterTuningJobWarmStartConfig pParentHyperParameterTuningJobs_ pWarmStartType_ =
  HyperParameterTuningJobWarmStartConfig'
    { _hptjwscParentHyperParameterTuningJobs =
        _List1 # pParentHyperParameterTuningJobs_
    , _hptjwscWarmStartType = pWarmStartType_
    }


-- | An array of hyperparameter tuning jobs that are used as the starting point for the new hyperparameter tuning job. For more information about warm starting a hyperparameter tuning job, see <http://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point> . Hyperparameter tuning jobs created before October 1, 2018 cannot be used as parent jobs for warm start tuning jobs.
hptjwscParentHyperParameterTuningJobs :: Lens' HyperParameterTuningJobWarmStartConfig (NonEmpty ParentHyperParameterTuningJob)
hptjwscParentHyperParameterTuningJobs = lens _hptjwscParentHyperParameterTuningJobs (\ s a -> s{_hptjwscParentHyperParameterTuningJobs = a}) . _List1

-- | Specifies one of the following:     * IDENTICAL_DATA_AND_ALGORITHM    * The new hyperparameter tuning job uses the same input data and training image as the parent tuning jobs. You can change the hyperparameter ranges to search and the maximum number of training jobs that the hyperparameter tuning job launches. You cannot use a new version of the training algorithm, unless the changes in the new version do not affect the algorithm itself. For example, changes that improve logging or adding support for a different data format are allowed. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.     * TRANSFER_LEARNING    * The new hyperparameter tuning job can include input data, hyperparameter ranges, maximum number of concurrent training jobs, and maximum number of training jobs that are different than those of its parent hyperparameter tuning jobs. The training image can also be a different version from the version used in the parent hyperparameter tuning job. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.
hptjwscWarmStartType :: Lens' HyperParameterTuningJobWarmStartConfig HyperParameterTuningJobWarmStartType
hptjwscWarmStartType = lens _hptjwscWarmStartType (\ s a -> s{_hptjwscWarmStartType = a})

instance FromJSON
           HyperParameterTuningJobWarmStartConfig
         where
        parseJSON
          = withObject "HyperParameterTuningJobWarmStartConfig"
              (\ x ->
                 HyperParameterTuningJobWarmStartConfig' <$>
                   (x .: "ParentHyperParameterTuningJobs") <*>
                     (x .: "WarmStartType"))

instance Hashable
           HyperParameterTuningJobWarmStartConfig
         where

instance NFData
           HyperParameterTuningJobWarmStartConfig
         where

instance ToJSON
           HyperParameterTuningJobWarmStartConfig
         where
        toJSON HyperParameterTuningJobWarmStartConfig'{..}
          = object
              (catMaybes
                 [Just
                    ("ParentHyperParameterTuningJobs" .=
                       _hptjwscParentHyperParameterTuningJobs),
                  Just ("WarmStartType" .= _hptjwscWarmStartType)])

-- | Defines how to perform inference generation after a training job is run.
--
--
--
-- /See:/ 'inferenceSpecification' smart constructor.
data InferenceSpecification = InferenceSpecification'
  { _isContainers :: !(List1 ModelPackageContainerDefinition)
  , _isSupportedTransformInstanceTypes :: !(List1 TransformInstanceType)
  , _isSupportedRealtimeInferenceInstanceTypes :: ![ProductionVariantInstanceType]
  , _isSupportedContentTypes :: ![Text]
  , _isSupportedResponseMIMETypes :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InferenceSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isContainers' - The Amazon ECR registry path of the Docker image that contains the inference code.
--
-- * 'isSupportedTransformInstanceTypes' - A list of the instance types on which a transformation job can be run or on which an endpoint can be deployed.
--
-- * 'isSupportedRealtimeInferenceInstanceTypes' - A list of the instance types that are used to generate inferences in real-time.
--
-- * 'isSupportedContentTypes' - The supported MIME types for the input data.
--
-- * 'isSupportedResponseMIMETypes' - The supported MIME types for the output data.
inferenceSpecification
    :: NonEmpty ModelPackageContainerDefinition -- ^ 'isContainers'
    -> NonEmpty TransformInstanceType -- ^ 'isSupportedTransformInstanceTypes'
    -> InferenceSpecification
inferenceSpecification pContainers_ pSupportedTransformInstanceTypes_ =
  InferenceSpecification'
    { _isContainers = _List1 # pContainers_
    , _isSupportedTransformInstanceTypes =
        _List1 # pSupportedTransformInstanceTypes_
    , _isSupportedRealtimeInferenceInstanceTypes = mempty
    , _isSupportedContentTypes = mempty
    , _isSupportedResponseMIMETypes = mempty
    }


-- | The Amazon ECR registry path of the Docker image that contains the inference code.
isContainers :: Lens' InferenceSpecification (NonEmpty ModelPackageContainerDefinition)
isContainers = lens _isContainers (\ s a -> s{_isContainers = a}) . _List1

-- | A list of the instance types on which a transformation job can be run or on which an endpoint can be deployed.
isSupportedTransformInstanceTypes :: Lens' InferenceSpecification (NonEmpty TransformInstanceType)
isSupportedTransformInstanceTypes = lens _isSupportedTransformInstanceTypes (\ s a -> s{_isSupportedTransformInstanceTypes = a}) . _List1

-- | A list of the instance types that are used to generate inferences in real-time.
isSupportedRealtimeInferenceInstanceTypes :: Lens' InferenceSpecification [ProductionVariantInstanceType]
isSupportedRealtimeInferenceInstanceTypes = lens _isSupportedRealtimeInferenceInstanceTypes (\ s a -> s{_isSupportedRealtimeInferenceInstanceTypes = a}) . _Coerce

-- | The supported MIME types for the input data.
isSupportedContentTypes :: Lens' InferenceSpecification [Text]
isSupportedContentTypes = lens _isSupportedContentTypes (\ s a -> s{_isSupportedContentTypes = a}) . _Coerce

-- | The supported MIME types for the output data.
isSupportedResponseMIMETypes :: Lens' InferenceSpecification [Text]
isSupportedResponseMIMETypes = lens _isSupportedResponseMIMETypes (\ s a -> s{_isSupportedResponseMIMETypes = a}) . _Coerce

instance FromJSON InferenceSpecification where
        parseJSON
          = withObject "InferenceSpecification"
              (\ x ->
                 InferenceSpecification' <$>
                   (x .: "Containers") <*>
                     (x .: "SupportedTransformInstanceTypes")
                     <*>
                     (x .:? "SupportedRealtimeInferenceInstanceTypes" .!=
                        mempty)
                     <*> (x .:? "SupportedContentTypes" .!= mempty)
                     <*> (x .:? "SupportedResponseMIMETypes" .!= mempty))

instance Hashable InferenceSpecification where

instance NFData InferenceSpecification where

instance ToJSON InferenceSpecification where
        toJSON InferenceSpecification'{..}
          = object
              (catMaybes
                 [Just ("Containers" .= _isContainers),
                  Just
                    ("SupportedTransformInstanceTypes" .=
                       _isSupportedTransformInstanceTypes),
                  Just
                    ("SupportedRealtimeInferenceInstanceTypes" .=
                       _isSupportedRealtimeInferenceInstanceTypes),
                  Just
                    ("SupportedContentTypes" .=
                       _isSupportedContentTypes),
                  Just
                    ("SupportedResponseMIMETypes" .=
                       _isSupportedResponseMIMETypes)])

-- | Contains information about the location of input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
--
--
--
-- /See:/ 'inputConfig' smart constructor.
data InputConfig = InputConfig'
  { _icS3URI           :: !Text
  , _icDataInputConfig :: !Text
  , _icFramework       :: !Framework
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icS3URI' - The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix).
--
-- * 'icDataInputConfig' - Specifies the name and shape of the expected data inputs for your trained model with a JSON dictionary form. The data inputs are 'InputConfig$Framework' specific.      * @TensorFlow@ : You must specify the name and shape (NHWC format) of the expected data inputs using a dictionary format for your trained model. The dictionary formats required for the console and CLI are different.     * Examples for one input:     * If using the console, @{"input":[1,1024,1024,3]}@      * If using the CLI, @{\"input\":[1,1024,1024,3]}@      * Examples for two inputs:     * If using the console, @{"data1": [1,28,28,1], "data2":[1,28,28,1]}@      * If using the CLI, @{\"data1\": [1,28,28,1], \"data2\":[1,28,28,1]}@      * @MXNET/ONNX@ : You must specify the name and shape (NCHW format) of the expected data inputs in order using a dictionary format for your trained model. The dictionary formats required for the console and CLI are different.     * Examples for one input:     * If using the console, @{"data":[1,3,1024,1024]}@      * If using the CLI, @{\"data\":[1,3,1024,1024]}@      * Examples for two inputs:     * If using the console, @{"var1": [1,1,28,28], "var2":[1,1,28,28]} @      * If using the CLI, @{\"var1\": [1,1,28,28], \"var2\":[1,1,28,28]}@      * @PyTorch@ : You can either specify the name and shape (NCHW format) of expected data inputs in order using a dictionary format for your trained model or you can specify the shape only using a list format. The dictionary formats required for the console and CLI are different. The list formats for the console and CLI are the same.     * Examples for one input in dictionary format:     * If using the console, @{"input0":[1,3,224,224]}@      * If using the CLI, @{\"input0\":[1,3,224,224]}@      * Example for one input in list format: @[[1,3,224,224]]@      * Examples for two inputs in dictionary format:     * If using the console, @{"input0":[1,3,224,224], "input1":[1,3,224,224]}@      * If using the CLI, @{\"input0\":[1,3,224,224], \"input1\":[1,3,224,224]} @      * Example for two inputs in list format: @[[1,3,224,224], [1,3,224,224]]@      * @XGBOOST@ : input data name and shape are not needed.
--
-- * 'icFramework' - Identifies the framework in which the model was trained. For example: TENSORFLOW.
inputConfig
    :: Text -- ^ 'icS3URI'
    -> Text -- ^ 'icDataInputConfig'
    -> Framework -- ^ 'icFramework'
    -> InputConfig
inputConfig pS3URI_ pDataInputConfig_ pFramework_ =
  InputConfig'
    { _icS3URI = pS3URI_
    , _icDataInputConfig = pDataInputConfig_
    , _icFramework = pFramework_
    }


-- | The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix).
icS3URI :: Lens' InputConfig Text
icS3URI = lens _icS3URI (\ s a -> s{_icS3URI = a})

-- | Specifies the name and shape of the expected data inputs for your trained model with a JSON dictionary form. The data inputs are 'InputConfig$Framework' specific.      * @TensorFlow@ : You must specify the name and shape (NHWC format) of the expected data inputs using a dictionary format for your trained model. The dictionary formats required for the console and CLI are different.     * Examples for one input:     * If using the console, @{"input":[1,1024,1024,3]}@      * If using the CLI, @{\"input\":[1,1024,1024,3]}@      * Examples for two inputs:     * If using the console, @{"data1": [1,28,28,1], "data2":[1,28,28,1]}@      * If using the CLI, @{\"data1\": [1,28,28,1], \"data2\":[1,28,28,1]}@      * @MXNET/ONNX@ : You must specify the name and shape (NCHW format) of the expected data inputs in order using a dictionary format for your trained model. The dictionary formats required for the console and CLI are different.     * Examples for one input:     * If using the console, @{"data":[1,3,1024,1024]}@      * If using the CLI, @{\"data\":[1,3,1024,1024]}@      * Examples for two inputs:     * If using the console, @{"var1": [1,1,28,28], "var2":[1,1,28,28]} @      * If using the CLI, @{\"var1\": [1,1,28,28], \"var2\":[1,1,28,28]}@      * @PyTorch@ : You can either specify the name and shape (NCHW format) of expected data inputs in order using a dictionary format for your trained model or you can specify the shape only using a list format. The dictionary formats required for the console and CLI are different. The list formats for the console and CLI are the same.     * Examples for one input in dictionary format:     * If using the console, @{"input0":[1,3,224,224]}@      * If using the CLI, @{\"input0\":[1,3,224,224]}@      * Example for one input in list format: @[[1,3,224,224]]@      * Examples for two inputs in dictionary format:     * If using the console, @{"input0":[1,3,224,224], "input1":[1,3,224,224]}@      * If using the CLI, @{\"input0\":[1,3,224,224], \"input1\":[1,3,224,224]} @      * Example for two inputs in list format: @[[1,3,224,224], [1,3,224,224]]@      * @XGBOOST@ : input data name and shape are not needed.
icDataInputConfig :: Lens' InputConfig Text
icDataInputConfig = lens _icDataInputConfig (\ s a -> s{_icDataInputConfig = a})

-- | Identifies the framework in which the model was trained. For example: TENSORFLOW.
icFramework :: Lens' InputConfig Framework
icFramework = lens _icFramework (\ s a -> s{_icFramework = a})

instance FromJSON InputConfig where
        parseJSON
          = withObject "InputConfig"
              (\ x ->
                 InputConfig' <$>
                   (x .: "S3Uri") <*> (x .: "DataInputConfig") <*>
                     (x .: "Framework"))

instance Hashable InputConfig where

instance NFData InputConfig where

instance ToJSON InputConfig where
        toJSON InputConfig'{..}
          = object
              (catMaybes
                 [Just ("S3Uri" .= _icS3URI),
                  Just ("DataInputConfig" .= _icDataInputConfig),
                  Just ("Framework" .= _icFramework)])

-- | For a hyperparameter of the integer type, specifies the range that a hyperparameter tuning job searches.
--
--
--
-- /See:/ 'integerParameterRange' smart constructor.
data IntegerParameterRange = IntegerParameterRange'
  { _iprScalingType :: !(Maybe HyperParameterScalingType)
  , _iprName        :: !Text
  , _iprMinValue    :: !Text
  , _iprMaxValue    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IntegerParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iprScalingType' - The scale that hyperparameter tuning uses to search the hyperparameter range. For information about choosing a hyperparameter scale, see <http://docs.aws.amazon.com//sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Range Scaling> . One of the following values:     * Auto    * Amazon SageMaker hyperparameter tuning chooses the best scale for the hyperparameter.     * Linear    * Hyperparameter tuning searches the values in the hyperparameter range by using a linear scale.     * Logarithmic    * Hyperparemeter tuning searches the values in the hyperparameter range by using a logarithmic scale. Logarithmic scaling works only for ranges that have only values greater than 0.
--
-- * 'iprName' - The name of the hyperparameter to search.
--
-- * 'iprMinValue' - The minimum value of the hyperparameter to search.
--
-- * 'iprMaxValue' - The maximum value of the hyperparameter to search.
integerParameterRange
    :: Text -- ^ 'iprName'
    -> Text -- ^ 'iprMinValue'
    -> Text -- ^ 'iprMaxValue'
    -> IntegerParameterRange
integerParameterRange pName_ pMinValue_ pMaxValue_ =
  IntegerParameterRange'
    { _iprScalingType = Nothing
    , _iprName = pName_
    , _iprMinValue = pMinValue_
    , _iprMaxValue = pMaxValue_
    }


-- | The scale that hyperparameter tuning uses to search the hyperparameter range. For information about choosing a hyperparameter scale, see <http://docs.aws.amazon.com//sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Range Scaling> . One of the following values:     * Auto    * Amazon SageMaker hyperparameter tuning chooses the best scale for the hyperparameter.     * Linear    * Hyperparameter tuning searches the values in the hyperparameter range by using a linear scale.     * Logarithmic    * Hyperparemeter tuning searches the values in the hyperparameter range by using a logarithmic scale. Logarithmic scaling works only for ranges that have only values greater than 0.
iprScalingType :: Lens' IntegerParameterRange (Maybe HyperParameterScalingType)
iprScalingType = lens _iprScalingType (\ s a -> s{_iprScalingType = a})

-- | The name of the hyperparameter to search.
iprName :: Lens' IntegerParameterRange Text
iprName = lens _iprName (\ s a -> s{_iprName = a})

-- | The minimum value of the hyperparameter to search.
iprMinValue :: Lens' IntegerParameterRange Text
iprMinValue = lens _iprMinValue (\ s a -> s{_iprMinValue = a})

-- | The maximum value of the hyperparameter to search.
iprMaxValue :: Lens' IntegerParameterRange Text
iprMaxValue = lens _iprMaxValue (\ s a -> s{_iprMaxValue = a})

instance FromJSON IntegerParameterRange where
        parseJSON
          = withObject "IntegerParameterRange"
              (\ x ->
                 IntegerParameterRange' <$>
                   (x .:? "ScalingType") <*> (x .: "Name") <*>
                     (x .: "MinValue")
                     <*> (x .: "MaxValue"))

instance Hashable IntegerParameterRange where

instance NFData IntegerParameterRange where

instance ToJSON IntegerParameterRange where
        toJSON IntegerParameterRange'{..}
          = object
              (catMaybes
                 [("ScalingType" .=) <$> _iprScalingType,
                  Just ("Name" .= _iprName),
                  Just ("MinValue" .= _iprMinValue),
                  Just ("MaxValue" .= _iprMaxValue)])

-- | Defines the possible values for an integer hyperparameter.
--
--
--
-- /See:/ 'integerParameterRangeSpecification' smart constructor.
data IntegerParameterRangeSpecification = IntegerParameterRangeSpecification'
  { _iprsMinValue :: !Text
  , _iprsMaxValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IntegerParameterRangeSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iprsMinValue' - The minimum integer value allowed.
--
-- * 'iprsMaxValue' - The maximum integer value allowed.
integerParameterRangeSpecification
    :: Text -- ^ 'iprsMinValue'
    -> Text -- ^ 'iprsMaxValue'
    -> IntegerParameterRangeSpecification
integerParameterRangeSpecification pMinValue_ pMaxValue_ =
  IntegerParameterRangeSpecification'
    {_iprsMinValue = pMinValue_, _iprsMaxValue = pMaxValue_}


-- | The minimum integer value allowed.
iprsMinValue :: Lens' IntegerParameterRangeSpecification Text
iprsMinValue = lens _iprsMinValue (\ s a -> s{_iprsMinValue = a})

-- | The maximum integer value allowed.
iprsMaxValue :: Lens' IntegerParameterRangeSpecification Text
iprsMaxValue = lens _iprsMaxValue (\ s a -> s{_iprsMaxValue = a})

instance FromJSON IntegerParameterRangeSpecification
         where
        parseJSON
          = withObject "IntegerParameterRangeSpecification"
              (\ x ->
                 IntegerParameterRangeSpecification' <$>
                   (x .: "MinValue") <*> (x .: "MaxValue"))

instance Hashable IntegerParameterRangeSpecification
         where

instance NFData IntegerParameterRangeSpecification
         where

instance ToJSON IntegerParameterRangeSpecification
         where
        toJSON IntegerParameterRangeSpecification'{..}
          = object
              (catMaybes
                 [Just ("MinValue" .= _iprsMinValue),
                  Just ("MaxValue" .= _iprsMaxValue)])

-- | Provides a breakdown of the number of objects labeled.
--
--
--
-- /See:/ 'labelCounters' smart constructor.
data LabelCounters = LabelCounters'
  { _lcMachineLabeled          :: !(Maybe Nat)
  , _lcTotalLabeled            :: !(Maybe Nat)
  , _lcFailedNonRetryableError :: !(Maybe Nat)
  , _lcUnlabeled               :: !(Maybe Nat)
  , _lcHumanLabeled            :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelCounters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcMachineLabeled' - The total number of objects labeled by automated data labeling.
--
-- * 'lcTotalLabeled' - The total number of objects labeled.
--
-- * 'lcFailedNonRetryableError' - The total number of objects that could not be labeled due to an error.
--
-- * 'lcUnlabeled' - The total number of objects not yet labeled.
--
-- * 'lcHumanLabeled' - The total number of objects labeled by a human worker.
labelCounters
    :: LabelCounters
labelCounters =
  LabelCounters'
    { _lcMachineLabeled = Nothing
    , _lcTotalLabeled = Nothing
    , _lcFailedNonRetryableError = Nothing
    , _lcUnlabeled = Nothing
    , _lcHumanLabeled = Nothing
    }


-- | The total number of objects labeled by automated data labeling.
lcMachineLabeled :: Lens' LabelCounters (Maybe Natural)
lcMachineLabeled = lens _lcMachineLabeled (\ s a -> s{_lcMachineLabeled = a}) . mapping _Nat

-- | The total number of objects labeled.
lcTotalLabeled :: Lens' LabelCounters (Maybe Natural)
lcTotalLabeled = lens _lcTotalLabeled (\ s a -> s{_lcTotalLabeled = a}) . mapping _Nat

-- | The total number of objects that could not be labeled due to an error.
lcFailedNonRetryableError :: Lens' LabelCounters (Maybe Natural)
lcFailedNonRetryableError = lens _lcFailedNonRetryableError (\ s a -> s{_lcFailedNonRetryableError = a}) . mapping _Nat

-- | The total number of objects not yet labeled.
lcUnlabeled :: Lens' LabelCounters (Maybe Natural)
lcUnlabeled = lens _lcUnlabeled (\ s a -> s{_lcUnlabeled = a}) . mapping _Nat

-- | The total number of objects labeled by a human worker.
lcHumanLabeled :: Lens' LabelCounters (Maybe Natural)
lcHumanLabeled = lens _lcHumanLabeled (\ s a -> s{_lcHumanLabeled = a}) . mapping _Nat

instance FromJSON LabelCounters where
        parseJSON
          = withObject "LabelCounters"
              (\ x ->
                 LabelCounters' <$>
                   (x .:? "MachineLabeled") <*> (x .:? "TotalLabeled")
                     <*> (x .:? "FailedNonRetryableError")
                     <*> (x .:? "Unlabeled")
                     <*> (x .:? "HumanLabeled"))

instance Hashable LabelCounters where

instance NFData LabelCounters where

-- | Provides counts for human-labeled tasks in the labeling job.
--
--
--
-- /See:/ 'labelCountersForWorkteam' smart constructor.
data LabelCountersForWorkteam = LabelCountersForWorkteam'
  { _lcfwPendingHuman :: !(Maybe Nat)
  , _lcfwTotal        :: !(Maybe Nat)
  , _lcfwHumanLabeled :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelCountersForWorkteam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcfwPendingHuman' - The total number of data objects that need to be labeled by a human worker.
--
-- * 'lcfwTotal' - The total number of tasks in the labeling job.
--
-- * 'lcfwHumanLabeled' - The total number of data objects labeled by a human worker.
labelCountersForWorkteam
    :: LabelCountersForWorkteam
labelCountersForWorkteam =
  LabelCountersForWorkteam'
    { _lcfwPendingHuman = Nothing
    , _lcfwTotal = Nothing
    , _lcfwHumanLabeled = Nothing
    }


-- | The total number of data objects that need to be labeled by a human worker.
lcfwPendingHuman :: Lens' LabelCountersForWorkteam (Maybe Natural)
lcfwPendingHuman = lens _lcfwPendingHuman (\ s a -> s{_lcfwPendingHuman = a}) . mapping _Nat

-- | The total number of tasks in the labeling job.
lcfwTotal :: Lens' LabelCountersForWorkteam (Maybe Natural)
lcfwTotal = lens _lcfwTotal (\ s a -> s{_lcfwTotal = a}) . mapping _Nat

-- | The total number of data objects labeled by a human worker.
lcfwHumanLabeled :: Lens' LabelCountersForWorkteam (Maybe Natural)
lcfwHumanLabeled = lens _lcfwHumanLabeled (\ s a -> s{_lcfwHumanLabeled = a}) . mapping _Nat

instance FromJSON LabelCountersForWorkteam where
        parseJSON
          = withObject "LabelCountersForWorkteam"
              (\ x ->
                 LabelCountersForWorkteam' <$>
                   (x .:? "PendingHuman") <*> (x .:? "Total") <*>
                     (x .:? "HumanLabeled"))

instance Hashable LabelCountersForWorkteam where

instance NFData LabelCountersForWorkteam where

-- | Provides configuration information for auto-labeling of your data objects. A @LabelingJobAlgorithmsConfig@ object must be supplied in order to use auto-labeling.
--
--
--
-- /See:/ 'labelingJobAlgorithmsConfig' smart constructor.
data LabelingJobAlgorithmsConfig = LabelingJobAlgorithmsConfig'
  { _ljacLabelingJobResourceConfig :: !(Maybe LabelingJobResourceConfig)
  , _ljacInitialActiveLearningModelARN :: !(Maybe Text)
  , _ljacLabelingJobAlgorithmSpecificationARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelingJobAlgorithmsConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljacLabelingJobResourceConfig' - Provides configuration information for a labeling job.
--
-- * 'ljacInitialActiveLearningModelARN' - At the end of an auto-label job Amazon SageMaker Ground Truth sends the Amazon Resource Nam (ARN) of the final model used for auto-labeling. You can use this model as the starting point for subsequent similar jobs by providing the ARN of the model here.
--
-- * 'ljacLabelingJobAlgorithmSpecificationARN' - Specifies the Amazon Resource Name (ARN) of the algorithm used for auto-labeling. You must select one of the following ARNs:     * /Image classification/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/image-classification@      * /Text classification/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/text-classification@      * /Object detection/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/object-detection@
labelingJobAlgorithmsConfig
    :: Text -- ^ 'ljacLabelingJobAlgorithmSpecificationARN'
    -> LabelingJobAlgorithmsConfig
labelingJobAlgorithmsConfig pLabelingJobAlgorithmSpecificationARN_ =
  LabelingJobAlgorithmsConfig'
    { _ljacLabelingJobResourceConfig = Nothing
    , _ljacInitialActiveLearningModelARN = Nothing
    , _ljacLabelingJobAlgorithmSpecificationARN =
        pLabelingJobAlgorithmSpecificationARN_
    }


-- | Provides configuration information for a labeling job.
ljacLabelingJobResourceConfig :: Lens' LabelingJobAlgorithmsConfig (Maybe LabelingJobResourceConfig)
ljacLabelingJobResourceConfig = lens _ljacLabelingJobResourceConfig (\ s a -> s{_ljacLabelingJobResourceConfig = a})

-- | At the end of an auto-label job Amazon SageMaker Ground Truth sends the Amazon Resource Nam (ARN) of the final model used for auto-labeling. You can use this model as the starting point for subsequent similar jobs by providing the ARN of the model here.
ljacInitialActiveLearningModelARN :: Lens' LabelingJobAlgorithmsConfig (Maybe Text)
ljacInitialActiveLearningModelARN = lens _ljacInitialActiveLearningModelARN (\ s a -> s{_ljacInitialActiveLearningModelARN = a})

-- | Specifies the Amazon Resource Name (ARN) of the algorithm used for auto-labeling. You must select one of the following ARNs:     * /Image classification/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/image-classification@      * /Text classification/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/text-classification@      * /Object detection/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/object-detection@
ljacLabelingJobAlgorithmSpecificationARN :: Lens' LabelingJobAlgorithmsConfig Text
ljacLabelingJobAlgorithmSpecificationARN = lens _ljacLabelingJobAlgorithmSpecificationARN (\ s a -> s{_ljacLabelingJobAlgorithmSpecificationARN = a})

instance FromJSON LabelingJobAlgorithmsConfig where
        parseJSON
          = withObject "LabelingJobAlgorithmsConfig"
              (\ x ->
                 LabelingJobAlgorithmsConfig' <$>
                   (x .:? "LabelingJobResourceConfig") <*>
                     (x .:? "InitialActiveLearningModelArn")
                     <*> (x .: "LabelingJobAlgorithmSpecificationArn"))

instance Hashable LabelingJobAlgorithmsConfig where

instance NFData LabelingJobAlgorithmsConfig where

instance ToJSON LabelingJobAlgorithmsConfig where
        toJSON LabelingJobAlgorithmsConfig'{..}
          = object
              (catMaybes
                 [("LabelingJobResourceConfig" .=) <$>
                    _ljacLabelingJobResourceConfig,
                  ("InitialActiveLearningModelArn" .=) <$>
                    _ljacInitialActiveLearningModelARN,
                  Just
                    ("LabelingJobAlgorithmSpecificationArn" .=
                       _ljacLabelingJobAlgorithmSpecificationARN)])

-- | Attributes of the data specified by the customer. Use these to describe the data to be labeled.
--
--
--
-- /See:/ 'labelingJobDataAttributes' smart constructor.
newtype LabelingJobDataAttributes = LabelingJobDataAttributes'
  { _ljdaContentClassifiers :: Maybe [ContentClassifier]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelingJobDataAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljdaContentClassifiers' - Declares that your content is free of personally identifiable information or adult content. Amazon SageMaker may restrict the Amazon Mechanical Turk workers that can view your task based on this information.
labelingJobDataAttributes
    :: LabelingJobDataAttributes
labelingJobDataAttributes =
  LabelingJobDataAttributes' {_ljdaContentClassifiers = Nothing}


-- | Declares that your content is free of personally identifiable information or adult content. Amazon SageMaker may restrict the Amazon Mechanical Turk workers that can view your task based on this information.
ljdaContentClassifiers :: Lens' LabelingJobDataAttributes [ContentClassifier]
ljdaContentClassifiers = lens _ljdaContentClassifiers (\ s a -> s{_ljdaContentClassifiers = a}) . _Default . _Coerce

instance FromJSON LabelingJobDataAttributes where
        parseJSON
          = withObject "LabelingJobDataAttributes"
              (\ x ->
                 LabelingJobDataAttributes' <$>
                   (x .:? "ContentClassifiers" .!= mempty))

instance Hashable LabelingJobDataAttributes where

instance NFData LabelingJobDataAttributes where

instance ToJSON LabelingJobDataAttributes where
        toJSON LabelingJobDataAttributes'{..}
          = object
              (catMaybes
                 [("ContentClassifiers" .=) <$>
                    _ljdaContentClassifiers])

-- | Provides information about the location of input data.
--
--
--
-- /See:/ 'labelingJobDataSource' smart constructor.
newtype LabelingJobDataSource = LabelingJobDataSource'
  { _ljdsS3DataSource :: LabelingJobS3DataSource
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelingJobDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljdsS3DataSource' - The Amazon S3 location of the input data objects.
labelingJobDataSource
    :: LabelingJobS3DataSource -- ^ 'ljdsS3DataSource'
    -> LabelingJobDataSource
labelingJobDataSource pS3DataSource_ =
  LabelingJobDataSource' {_ljdsS3DataSource = pS3DataSource_}


-- | The Amazon S3 location of the input data objects.
ljdsS3DataSource :: Lens' LabelingJobDataSource LabelingJobS3DataSource
ljdsS3DataSource = lens _ljdsS3DataSource (\ s a -> s{_ljdsS3DataSource = a})

instance FromJSON LabelingJobDataSource where
        parseJSON
          = withObject "LabelingJobDataSource"
              (\ x ->
                 LabelingJobDataSource' <$> (x .: "S3DataSource"))

instance Hashable LabelingJobDataSource where

instance NFData LabelingJobDataSource where

instance ToJSON LabelingJobDataSource where
        toJSON LabelingJobDataSource'{..}
          = object
              (catMaybes
                 [Just ("S3DataSource" .= _ljdsS3DataSource)])

-- | Provides summary information for a work team.
--
--
--
-- /See:/ 'labelingJobForWorkteamSummary' smart constructor.
data LabelingJobForWorkteamSummary = LabelingJobForWorkteamSummary'
  { _ljfwsLabelCounters          :: !(Maybe LabelCountersForWorkteam)
  , _ljfwsLabelingJobName        :: !(Maybe Text)
  , _ljfwsJobReferenceCode       :: !Text
  , _ljfwsWorkRequesterAccountId :: !Text
  , _ljfwsCreationTime           :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelingJobForWorkteamSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljfwsLabelCounters' - Provides information about the progress of a labeling job.
--
-- * 'ljfwsLabelingJobName' - The name of the labeling job that the work team is assigned to.
--
-- * 'ljfwsJobReferenceCode' - A unique identifier for a labeling job. You can use this to refer to a specific labeling job.
--
-- * 'ljfwsWorkRequesterAccountId' -
--
-- * 'ljfwsCreationTime' - The date and time that the labeling job was created.
labelingJobForWorkteamSummary
    :: Text -- ^ 'ljfwsJobReferenceCode'
    -> Text -- ^ 'ljfwsWorkRequesterAccountId'
    -> UTCTime -- ^ 'ljfwsCreationTime'
    -> LabelingJobForWorkteamSummary
labelingJobForWorkteamSummary pJobReferenceCode_ pWorkRequesterAccountId_ pCreationTime_ =
  LabelingJobForWorkteamSummary'
    { _ljfwsLabelCounters = Nothing
    , _ljfwsLabelingJobName = Nothing
    , _ljfwsJobReferenceCode = pJobReferenceCode_
    , _ljfwsWorkRequesterAccountId = pWorkRequesterAccountId_
    , _ljfwsCreationTime = _Time # pCreationTime_
    }


-- | Provides information about the progress of a labeling job.
ljfwsLabelCounters :: Lens' LabelingJobForWorkteamSummary (Maybe LabelCountersForWorkteam)
ljfwsLabelCounters = lens _ljfwsLabelCounters (\ s a -> s{_ljfwsLabelCounters = a})

-- | The name of the labeling job that the work team is assigned to.
ljfwsLabelingJobName :: Lens' LabelingJobForWorkteamSummary (Maybe Text)
ljfwsLabelingJobName = lens _ljfwsLabelingJobName (\ s a -> s{_ljfwsLabelingJobName = a})

-- | A unique identifier for a labeling job. You can use this to refer to a specific labeling job.
ljfwsJobReferenceCode :: Lens' LabelingJobForWorkteamSummary Text
ljfwsJobReferenceCode = lens _ljfwsJobReferenceCode (\ s a -> s{_ljfwsJobReferenceCode = a})

-- |
ljfwsWorkRequesterAccountId :: Lens' LabelingJobForWorkteamSummary Text
ljfwsWorkRequesterAccountId = lens _ljfwsWorkRequesterAccountId (\ s a -> s{_ljfwsWorkRequesterAccountId = a})

-- | The date and time that the labeling job was created.
ljfwsCreationTime :: Lens' LabelingJobForWorkteamSummary UTCTime
ljfwsCreationTime = lens _ljfwsCreationTime (\ s a -> s{_ljfwsCreationTime = a}) . _Time

instance FromJSON LabelingJobForWorkteamSummary where
        parseJSON
          = withObject "LabelingJobForWorkteamSummary"
              (\ x ->
                 LabelingJobForWorkteamSummary' <$>
                   (x .:? "LabelCounters") <*> (x .:? "LabelingJobName")
                     <*> (x .: "JobReferenceCode")
                     <*> (x .: "WorkRequesterAccountId")
                     <*> (x .: "CreationTime"))

instance Hashable LabelingJobForWorkteamSummary where

instance NFData LabelingJobForWorkteamSummary where

-- | Input configuration information for a labeling job.
--
--
--
-- /See:/ 'labelingJobInputConfig' smart constructor.
data LabelingJobInputConfig = LabelingJobInputConfig'
  { _ljicDataAttributes :: !(Maybe LabelingJobDataAttributes)
  , _ljicDataSource     :: !LabelingJobDataSource
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelingJobInputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljicDataAttributes' - Attributes of the data specified by the customer.
--
-- * 'ljicDataSource' - The location of the input data.
labelingJobInputConfig
    :: LabelingJobDataSource -- ^ 'ljicDataSource'
    -> LabelingJobInputConfig
labelingJobInputConfig pDataSource_ =
  LabelingJobInputConfig'
    {_ljicDataAttributes = Nothing, _ljicDataSource = pDataSource_}


-- | Attributes of the data specified by the customer.
ljicDataAttributes :: Lens' LabelingJobInputConfig (Maybe LabelingJobDataAttributes)
ljicDataAttributes = lens _ljicDataAttributes (\ s a -> s{_ljicDataAttributes = a})

-- | The location of the input data.
ljicDataSource :: Lens' LabelingJobInputConfig LabelingJobDataSource
ljicDataSource = lens _ljicDataSource (\ s a -> s{_ljicDataSource = a})

instance FromJSON LabelingJobInputConfig where
        parseJSON
          = withObject "LabelingJobInputConfig"
              (\ x ->
                 LabelingJobInputConfig' <$>
                   (x .:? "DataAttributes") <*> (x .: "DataSource"))

instance Hashable LabelingJobInputConfig where

instance NFData LabelingJobInputConfig where

instance ToJSON LabelingJobInputConfig where
        toJSON LabelingJobInputConfig'{..}
          = object
              (catMaybes
                 [("DataAttributes" .=) <$> _ljicDataAttributes,
                  Just ("DataSource" .= _ljicDataSource)])

-- | Specifies the location of the output produced by the labeling job.
--
--
--
-- /See:/ 'labelingJobOutput' smart constructor.
data LabelingJobOutput = LabelingJobOutput'
  { _ljoFinalActiveLearningModelARN :: !(Maybe Text)
  , _ljoOutputDatasetS3URI          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelingJobOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljoFinalActiveLearningModelARN' - The Amazon Resource Name (ARN) for the most recent Amazon SageMaker model trained as part of automated data labeling.
--
-- * 'ljoOutputDatasetS3URI' - The Amazon S3 bucket location of the manifest file for labeled data.
labelingJobOutput
    :: Text -- ^ 'ljoOutputDatasetS3URI'
    -> LabelingJobOutput
labelingJobOutput pOutputDatasetS3URI_ =
  LabelingJobOutput'
    { _ljoFinalActiveLearningModelARN = Nothing
    , _ljoOutputDatasetS3URI = pOutputDatasetS3URI_
    }


-- | The Amazon Resource Name (ARN) for the most recent Amazon SageMaker model trained as part of automated data labeling.
ljoFinalActiveLearningModelARN :: Lens' LabelingJobOutput (Maybe Text)
ljoFinalActiveLearningModelARN = lens _ljoFinalActiveLearningModelARN (\ s a -> s{_ljoFinalActiveLearningModelARN = a})

-- | The Amazon S3 bucket location of the manifest file for labeled data.
ljoOutputDatasetS3URI :: Lens' LabelingJobOutput Text
ljoOutputDatasetS3URI = lens _ljoOutputDatasetS3URI (\ s a -> s{_ljoOutputDatasetS3URI = a})

instance FromJSON LabelingJobOutput where
        parseJSON
          = withObject "LabelingJobOutput"
              (\ x ->
                 LabelingJobOutput' <$>
                   (x .:? "FinalActiveLearningModelArn") <*>
                     (x .: "OutputDatasetS3Uri"))

instance Hashable LabelingJobOutput where

instance NFData LabelingJobOutput where

-- | Output configuration information for a labeling job.
--
--
--
-- /See:/ 'labelingJobOutputConfig' smart constructor.
data LabelingJobOutputConfig = LabelingJobOutputConfig'
  { _ljocKMSKeyId     :: !(Maybe Text)
  , _ljocS3OutputPath :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelingJobOutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljocKMSKeyId' - The AWS Key Management Service ID of the key used to encrypt the output data, if any.
--
-- * 'ljocS3OutputPath' - The Amazon S3 location to write output data.
labelingJobOutputConfig
    :: Text -- ^ 'ljocS3OutputPath'
    -> LabelingJobOutputConfig
labelingJobOutputConfig pS3OutputPath_ =
  LabelingJobOutputConfig'
    {_ljocKMSKeyId = Nothing, _ljocS3OutputPath = pS3OutputPath_}


-- | The AWS Key Management Service ID of the key used to encrypt the output data, if any.
ljocKMSKeyId :: Lens' LabelingJobOutputConfig (Maybe Text)
ljocKMSKeyId = lens _ljocKMSKeyId (\ s a -> s{_ljocKMSKeyId = a})

-- | The Amazon S3 location to write output data.
ljocS3OutputPath :: Lens' LabelingJobOutputConfig Text
ljocS3OutputPath = lens _ljocS3OutputPath (\ s a -> s{_ljocS3OutputPath = a})

instance FromJSON LabelingJobOutputConfig where
        parseJSON
          = withObject "LabelingJobOutputConfig"
              (\ x ->
                 LabelingJobOutputConfig' <$>
                   (x .:? "KmsKeyId") <*> (x .: "S3OutputPath"))

instance Hashable LabelingJobOutputConfig where

instance NFData LabelingJobOutputConfig where

instance ToJSON LabelingJobOutputConfig where
        toJSON LabelingJobOutputConfig'{..}
          = object
              (catMaybes
                 [("KmsKeyId" .=) <$> _ljocKMSKeyId,
                  Just ("S3OutputPath" .= _ljocS3OutputPath)])

-- | Provides configuration information for labeling jobs.
--
--
--
-- /See:/ 'labelingJobResourceConfig' smart constructor.
newtype LabelingJobResourceConfig = LabelingJobResourceConfig'
  { _ljrcVolumeKMSKeyId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelingJobResourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljrcVolumeKMSKeyId' - The AWS Key Management Service key ID for the key used to encrypt the output data, if any.
labelingJobResourceConfig
    :: LabelingJobResourceConfig
labelingJobResourceConfig =
  LabelingJobResourceConfig' {_ljrcVolumeKMSKeyId = Nothing}


-- | The AWS Key Management Service key ID for the key used to encrypt the output data, if any.
ljrcVolumeKMSKeyId :: Lens' LabelingJobResourceConfig (Maybe Text)
ljrcVolumeKMSKeyId = lens _ljrcVolumeKMSKeyId (\ s a -> s{_ljrcVolumeKMSKeyId = a})

instance FromJSON LabelingJobResourceConfig where
        parseJSON
          = withObject "LabelingJobResourceConfig"
              (\ x ->
                 LabelingJobResourceConfig' <$>
                   (x .:? "VolumeKmsKeyId"))

instance Hashable LabelingJobResourceConfig where

instance NFData LabelingJobResourceConfig where

instance ToJSON LabelingJobResourceConfig where
        toJSON LabelingJobResourceConfig'{..}
          = object
              (catMaybes
                 [("VolumeKmsKeyId" .=) <$> _ljrcVolumeKMSKeyId])

-- | The Amazon S3 location of the input data objects.
--
--
--
-- /See:/ 'labelingJobS3DataSource' smart constructor.
newtype LabelingJobS3DataSource = LabelingJobS3DataSource'
  { _ljsdsManifestS3URI :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelingJobS3DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljsdsManifestS3URI' - The Amazon S3 location of the manifest file that describes the input data objects.
labelingJobS3DataSource
    :: Text -- ^ 'ljsdsManifestS3URI'
    -> LabelingJobS3DataSource
labelingJobS3DataSource pManifestS3URI_ =
  LabelingJobS3DataSource' {_ljsdsManifestS3URI = pManifestS3URI_}


-- | The Amazon S3 location of the manifest file that describes the input data objects.
ljsdsManifestS3URI :: Lens' LabelingJobS3DataSource Text
ljsdsManifestS3URI = lens _ljsdsManifestS3URI (\ s a -> s{_ljsdsManifestS3URI = a})

instance FromJSON LabelingJobS3DataSource where
        parseJSON
          = withObject "LabelingJobS3DataSource"
              (\ x ->
                 LabelingJobS3DataSource' <$> (x .: "ManifestS3Uri"))

instance Hashable LabelingJobS3DataSource where

instance NFData LabelingJobS3DataSource where

instance ToJSON LabelingJobS3DataSource where
        toJSON LabelingJobS3DataSource'{..}
          = object
              (catMaybes
                 [Just ("ManifestS3Uri" .= _ljsdsManifestS3URI)])

-- | A set of conditions for stopping a labeling job. If any of the conditions are met, the job is automatically stopped. You can use these conditions to control the cost of data labeling.
--
--
--
-- /See:/ 'labelingJobStoppingConditions' smart constructor.
data LabelingJobStoppingConditions = LabelingJobStoppingConditions'
  { _ljscMaxHumanLabeledObjectCount         :: !(Maybe Nat)
  , _ljscMaxPercentageOfInputDatasetLabeled :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelingJobStoppingConditions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljscMaxHumanLabeledObjectCount' - The maximum number of objects that can be labeled by human workers.
--
-- * 'ljscMaxPercentageOfInputDatasetLabeled' - The maximum number of input data objects that should be labeled.
labelingJobStoppingConditions
    :: LabelingJobStoppingConditions
labelingJobStoppingConditions =
  LabelingJobStoppingConditions'
    { _ljscMaxHumanLabeledObjectCount = Nothing
    , _ljscMaxPercentageOfInputDatasetLabeled = Nothing
    }


-- | The maximum number of objects that can be labeled by human workers.
ljscMaxHumanLabeledObjectCount :: Lens' LabelingJobStoppingConditions (Maybe Natural)
ljscMaxHumanLabeledObjectCount = lens _ljscMaxHumanLabeledObjectCount (\ s a -> s{_ljscMaxHumanLabeledObjectCount = a}) . mapping _Nat

-- | The maximum number of input data objects that should be labeled.
ljscMaxPercentageOfInputDatasetLabeled :: Lens' LabelingJobStoppingConditions (Maybe Natural)
ljscMaxPercentageOfInputDatasetLabeled = lens _ljscMaxPercentageOfInputDatasetLabeled (\ s a -> s{_ljscMaxPercentageOfInputDatasetLabeled = a}) . mapping _Nat

instance FromJSON LabelingJobStoppingConditions where
        parseJSON
          = withObject "LabelingJobStoppingConditions"
              (\ x ->
                 LabelingJobStoppingConditions' <$>
                   (x .:? "MaxHumanLabeledObjectCount") <*>
                     (x .:? "MaxPercentageOfInputDatasetLabeled"))

instance Hashable LabelingJobStoppingConditions where

instance NFData LabelingJobStoppingConditions where

instance ToJSON LabelingJobStoppingConditions where
        toJSON LabelingJobStoppingConditions'{..}
          = object
              (catMaybes
                 [("MaxHumanLabeledObjectCount" .=) <$>
                    _ljscMaxHumanLabeledObjectCount,
                  ("MaxPercentageOfInputDatasetLabeled" .=) <$>
                    _ljscMaxPercentageOfInputDatasetLabeled])

-- | Provides summary information about a labeling job.
--
--
--
-- /See:/ 'labelingJobSummary' smart constructor.
data LabelingJobSummary = LabelingJobSummary'
  { _ljsFailureReason                    :: !(Maybe Text)
  , _ljsAnnotationConsolidationLambdaARN :: !(Maybe Text)
  , _ljsInputConfig                      :: !(Maybe LabelingJobInputConfig)
  , _ljsLabelingJobOutput                :: !(Maybe LabelingJobOutput)
  , _ljsLabelingJobName                  :: !Text
  , _ljsLabelingJobARN                   :: !Text
  , _ljsCreationTime                     :: !POSIX
  , _ljsLastModifiedTime                 :: !POSIX
  , _ljsLabelingJobStatus                :: !LabelingJobStatus
  , _ljsLabelCounters                    :: !LabelCounters
  , _ljsWorkteamARN                      :: !Text
  , _ljsPreHumanTaskLambdaARN            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelingJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljsFailureReason' - If the @LabelingJobStatus@ field is @Failed@ , this field contains a description of the error.
--
-- * 'ljsAnnotationConsolidationLambdaARN' - The Amazon Resource Name (ARN) of the Lambda function used to consolidate the annotations from individual workers into a label for a data object. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
--
-- * 'ljsInputConfig' - Input configuration for the labeling job.
--
-- * 'ljsLabelingJobOutput' - The location of the output produced by the labeling job.
--
-- * 'ljsLabelingJobName' - The name of the labeling job.
--
-- * 'ljsLabelingJobARN' - The Amazon Resource Name (ARN) assigned to the labeling job when it was created.
--
-- * 'ljsCreationTime' - The date and time that the job was created (timestamp).
--
-- * 'ljsLastModifiedTime' - The date and time that the job was last modified (timestamp).
--
-- * 'ljsLabelingJobStatus' - The current status of the labeling job.
--
-- * 'ljsLabelCounters' - Counts showing the progress of the labeling job.
--
-- * 'ljsWorkteamARN' - The Amazon Resource Name (ARN) of the work team assigned to the job.
--
-- * 'ljsPreHumanTaskLambdaARN' - The Amazon Resource Name (ARN) of a Lambda function. The function is run before each data object is sent to a worker.
labelingJobSummary
    :: Text -- ^ 'ljsLabelingJobName'
    -> Text -- ^ 'ljsLabelingJobARN'
    -> UTCTime -- ^ 'ljsCreationTime'
    -> UTCTime -- ^ 'ljsLastModifiedTime'
    -> LabelingJobStatus -- ^ 'ljsLabelingJobStatus'
    -> LabelCounters -- ^ 'ljsLabelCounters'
    -> Text -- ^ 'ljsWorkteamARN'
    -> Text -- ^ 'ljsPreHumanTaskLambdaARN'
    -> LabelingJobSummary
labelingJobSummary pLabelingJobName_ pLabelingJobARN_ pCreationTime_ pLastModifiedTime_ pLabelingJobStatus_ pLabelCounters_ pWorkteamARN_ pPreHumanTaskLambdaARN_ =
  LabelingJobSummary'
    { _ljsFailureReason = Nothing
    , _ljsAnnotationConsolidationLambdaARN = Nothing
    , _ljsInputConfig = Nothing
    , _ljsLabelingJobOutput = Nothing
    , _ljsLabelingJobName = pLabelingJobName_
    , _ljsLabelingJobARN = pLabelingJobARN_
    , _ljsCreationTime = _Time # pCreationTime_
    , _ljsLastModifiedTime = _Time # pLastModifiedTime_
    , _ljsLabelingJobStatus = pLabelingJobStatus_
    , _ljsLabelCounters = pLabelCounters_
    , _ljsWorkteamARN = pWorkteamARN_
    , _ljsPreHumanTaskLambdaARN = pPreHumanTaskLambdaARN_
    }


-- | If the @LabelingJobStatus@ field is @Failed@ , this field contains a description of the error.
ljsFailureReason :: Lens' LabelingJobSummary (Maybe Text)
ljsFailureReason = lens _ljsFailureReason (\ s a -> s{_ljsFailureReason = a})

-- | The Amazon Resource Name (ARN) of the Lambda function used to consolidate the annotations from individual workers into a label for a data object. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
ljsAnnotationConsolidationLambdaARN :: Lens' LabelingJobSummary (Maybe Text)
ljsAnnotationConsolidationLambdaARN = lens _ljsAnnotationConsolidationLambdaARN (\ s a -> s{_ljsAnnotationConsolidationLambdaARN = a})

-- | Input configuration for the labeling job.
ljsInputConfig :: Lens' LabelingJobSummary (Maybe LabelingJobInputConfig)
ljsInputConfig = lens _ljsInputConfig (\ s a -> s{_ljsInputConfig = a})

-- | The location of the output produced by the labeling job.
ljsLabelingJobOutput :: Lens' LabelingJobSummary (Maybe LabelingJobOutput)
ljsLabelingJobOutput = lens _ljsLabelingJobOutput (\ s a -> s{_ljsLabelingJobOutput = a})

-- | The name of the labeling job.
ljsLabelingJobName :: Lens' LabelingJobSummary Text
ljsLabelingJobName = lens _ljsLabelingJobName (\ s a -> s{_ljsLabelingJobName = a})

-- | The Amazon Resource Name (ARN) assigned to the labeling job when it was created.
ljsLabelingJobARN :: Lens' LabelingJobSummary Text
ljsLabelingJobARN = lens _ljsLabelingJobARN (\ s a -> s{_ljsLabelingJobARN = a})

-- | The date and time that the job was created (timestamp).
ljsCreationTime :: Lens' LabelingJobSummary UTCTime
ljsCreationTime = lens _ljsCreationTime (\ s a -> s{_ljsCreationTime = a}) . _Time

-- | The date and time that the job was last modified (timestamp).
ljsLastModifiedTime :: Lens' LabelingJobSummary UTCTime
ljsLastModifiedTime = lens _ljsLastModifiedTime (\ s a -> s{_ljsLastModifiedTime = a}) . _Time

-- | The current status of the labeling job.
ljsLabelingJobStatus :: Lens' LabelingJobSummary LabelingJobStatus
ljsLabelingJobStatus = lens _ljsLabelingJobStatus (\ s a -> s{_ljsLabelingJobStatus = a})

-- | Counts showing the progress of the labeling job.
ljsLabelCounters :: Lens' LabelingJobSummary LabelCounters
ljsLabelCounters = lens _ljsLabelCounters (\ s a -> s{_ljsLabelCounters = a})

-- | The Amazon Resource Name (ARN) of the work team assigned to the job.
ljsWorkteamARN :: Lens' LabelingJobSummary Text
ljsWorkteamARN = lens _ljsWorkteamARN (\ s a -> s{_ljsWorkteamARN = a})

-- | The Amazon Resource Name (ARN) of a Lambda function. The function is run before each data object is sent to a worker.
ljsPreHumanTaskLambdaARN :: Lens' LabelingJobSummary Text
ljsPreHumanTaskLambdaARN = lens _ljsPreHumanTaskLambdaARN (\ s a -> s{_ljsPreHumanTaskLambdaARN = a})

instance FromJSON LabelingJobSummary where
        parseJSON
          = withObject "LabelingJobSummary"
              (\ x ->
                 LabelingJobSummary' <$>
                   (x .:? "FailureReason") <*>
                     (x .:? "AnnotationConsolidationLambdaArn")
                     <*> (x .:? "InputConfig")
                     <*> (x .:? "LabelingJobOutput")
                     <*> (x .: "LabelingJobName")
                     <*> (x .: "LabelingJobArn")
                     <*> (x .: "CreationTime")
                     <*> (x .: "LastModifiedTime")
                     <*> (x .: "LabelingJobStatus")
                     <*> (x .: "LabelCounters")
                     <*> (x .: "WorkteamArn")
                     <*> (x .: "PreHumanTaskLambdaArn"))

instance Hashable LabelingJobSummary where

instance NFData LabelingJobSummary where

-- | Defines the Amazon Cognito user group that is part of a work team.
--
--
--
-- /See:/ 'memberDefinition' smart constructor.
newtype MemberDefinition = MemberDefinition'
  { _mdCognitoMemberDefinition :: Maybe CognitoMemberDefinition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MemberDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdCognitoMemberDefinition' - The Amazon Cognito user group that is part of the work team.
memberDefinition
    :: MemberDefinition
memberDefinition = MemberDefinition' {_mdCognitoMemberDefinition = Nothing}


-- | The Amazon Cognito user group that is part of the work team.
mdCognitoMemberDefinition :: Lens' MemberDefinition (Maybe CognitoMemberDefinition)
mdCognitoMemberDefinition = lens _mdCognitoMemberDefinition (\ s a -> s{_mdCognitoMemberDefinition = a})

instance FromJSON MemberDefinition where
        parseJSON
          = withObject "MemberDefinition"
              (\ x ->
                 MemberDefinition' <$>
                   (x .:? "CognitoMemberDefinition"))

instance Hashable MemberDefinition where

instance NFData MemberDefinition where

instance ToJSON MemberDefinition where
        toJSON MemberDefinition'{..}
          = object
              (catMaybes
                 [("CognitoMemberDefinition" .=) <$>
                    _mdCognitoMemberDefinition])

-- | The name, value, and date and time of a metric that was emitted to Amazon CloudWatch.
--
--
--
-- /See:/ 'metricData' smart constructor.
data MetricData = MetricData'
  { _mdMetricName :: !(Maybe Text)
  , _mdValue      :: !(Maybe Double)
  , _mdTimestamp  :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdMetricName' - The name of the metric.
--
-- * 'mdValue' - The value of the metric.
--
-- * 'mdTimestamp' - The date and time that the algorithm emitted the metric.
metricData
    :: MetricData
metricData =
  MetricData'
    {_mdMetricName = Nothing, _mdValue = Nothing, _mdTimestamp = Nothing}


-- | The name of the metric.
mdMetricName :: Lens' MetricData (Maybe Text)
mdMetricName = lens _mdMetricName (\ s a -> s{_mdMetricName = a})

-- | The value of the metric.
mdValue :: Lens' MetricData (Maybe Double)
mdValue = lens _mdValue (\ s a -> s{_mdValue = a})

-- | The date and time that the algorithm emitted the metric.
mdTimestamp :: Lens' MetricData (Maybe UTCTime)
mdTimestamp = lens _mdTimestamp (\ s a -> s{_mdTimestamp = a}) . mapping _Time

instance FromJSON MetricData where
        parseJSON
          = withObject "MetricData"
              (\ x ->
                 MetricData' <$>
                   (x .:? "MetricName") <*> (x .:? "Value") <*>
                     (x .:? "Timestamp"))

instance Hashable MetricData where

instance NFData MetricData where

-- | Specifies a metric that the training algorithm writes to @stderr@ or @stdout@ . Amazon SageMakerhyperparameter tuning captures all defined metrics. You specify one metric that a hyperparameter tuning job uses as its objective metric to choose the best training job.
--
--
--
-- /See:/ 'metricDefinition' smart constructor.
data MetricDefinition = MetricDefinition'
  { _mdName  :: !Text
  , _mdRegex :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdName' - The name of the metric.
--
-- * 'mdRegex' - A regular expression that searches the output of a training job and gets the value of the metric. For more information about using regular expressions to define metrics, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics.html Defining Objective Metrics> .
metricDefinition
    :: Text -- ^ 'mdName'
    -> Text -- ^ 'mdRegex'
    -> MetricDefinition
metricDefinition pName_ pRegex_ =
  MetricDefinition' {_mdName = pName_, _mdRegex = pRegex_}


-- | The name of the metric.
mdName :: Lens' MetricDefinition Text
mdName = lens _mdName (\ s a -> s{_mdName = a})

-- | A regular expression that searches the output of a training job and gets the value of the metric. For more information about using regular expressions to define metrics, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-metrics.html Defining Objective Metrics> .
mdRegex :: Lens' MetricDefinition Text
mdRegex = lens _mdRegex (\ s a -> s{_mdRegex = a})

instance FromJSON MetricDefinition where
        parseJSON
          = withObject "MetricDefinition"
              (\ x ->
                 MetricDefinition' <$>
                   (x .: "Name") <*> (x .: "Regex"))

instance Hashable MetricDefinition where

instance NFData MetricDefinition where

instance ToJSON MetricDefinition where
        toJSON MetricDefinition'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _mdName),
                  Just ("Regex" .= _mdRegex)])

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

-- | Describes the Docker container for the model package.
--
--
--
-- /See:/ 'modelPackageContainerDefinition' smart constructor.
data ModelPackageContainerDefinition = ModelPackageContainerDefinition'
  { _mpcdModelDataURL      :: !(Maybe Text)
  , _mpcdImageDigest       :: !(Maybe Text)
  , _mpcdContainerHostname :: !(Maybe Text)
  , _mpcdProductId         :: !(Maybe Text)
  , _mpcdImage             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModelPackageContainerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpcdModelDataURL' - The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
--
-- * 'mpcdImageDigest' - An MD5 hash of the training algorithm that identifies the Docker image used for training.
--
-- * 'mpcdContainerHostname' - The DNS host name for the Docker container.
--
-- * 'mpcdProductId' - The AWS Marketplace product ID of the model package.
--
-- * 'mpcdImage' - The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
modelPackageContainerDefinition
    :: Text -- ^ 'mpcdImage'
    -> ModelPackageContainerDefinition
modelPackageContainerDefinition pImage_ =
  ModelPackageContainerDefinition'
    { _mpcdModelDataURL = Nothing
    , _mpcdImageDigest = Nothing
    , _mpcdContainerHostname = Nothing
    , _mpcdProductId = Nothing
    , _mpcdImage = pImage_
    }


-- | The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
mpcdModelDataURL :: Lens' ModelPackageContainerDefinition (Maybe Text)
mpcdModelDataURL = lens _mpcdModelDataURL (\ s a -> s{_mpcdModelDataURL = a})

-- | An MD5 hash of the training algorithm that identifies the Docker image used for training.
mpcdImageDigest :: Lens' ModelPackageContainerDefinition (Maybe Text)
mpcdImageDigest = lens _mpcdImageDigest (\ s a -> s{_mpcdImageDigest = a})

-- | The DNS host name for the Docker container.
mpcdContainerHostname :: Lens' ModelPackageContainerDefinition (Maybe Text)
mpcdContainerHostname = lens _mpcdContainerHostname (\ s a -> s{_mpcdContainerHostname = a})

-- | The AWS Marketplace product ID of the model package.
mpcdProductId :: Lens' ModelPackageContainerDefinition (Maybe Text)
mpcdProductId = lens _mpcdProductId (\ s a -> s{_mpcdProductId = a})

-- | The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
mpcdImage :: Lens' ModelPackageContainerDefinition Text
mpcdImage = lens _mpcdImage (\ s a -> s{_mpcdImage = a})

instance FromJSON ModelPackageContainerDefinition
         where
        parseJSON
          = withObject "ModelPackageContainerDefinition"
              (\ x ->
                 ModelPackageContainerDefinition' <$>
                   (x .:? "ModelDataUrl") <*> (x .:? "ImageDigest") <*>
                     (x .:? "ContainerHostname")
                     <*> (x .:? "ProductId")
                     <*> (x .: "Image"))

instance Hashable ModelPackageContainerDefinition
         where

instance NFData ModelPackageContainerDefinition where

instance ToJSON ModelPackageContainerDefinition where
        toJSON ModelPackageContainerDefinition'{..}
          = object
              (catMaybes
                 [("ModelDataUrl" .=) <$> _mpcdModelDataURL,
                  ("ImageDigest" .=) <$> _mpcdImageDigest,
                  ("ContainerHostname" .=) <$> _mpcdContainerHostname,
                  ("ProductId" .=) <$> _mpcdProductId,
                  Just ("Image" .= _mpcdImage)])

-- | Specifies the validation and image scan statuses of the model package.
--
--
--
-- /See:/ 'modelPackageStatusDetails' smart constructor.
data ModelPackageStatusDetails = ModelPackageStatusDetails'
  { _mpsdImageScanStatuses  :: !(Maybe [ModelPackageStatusItem])
  , _mpsdValidationStatuses :: ![ModelPackageStatusItem]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModelPackageStatusDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsdImageScanStatuses' - The status of the scan of the Docker image container for the model package.
--
-- * 'mpsdValidationStatuses' - The validation status of the model package.
modelPackageStatusDetails
    :: ModelPackageStatusDetails
modelPackageStatusDetails =
  ModelPackageStatusDetails'
    {_mpsdImageScanStatuses = Nothing, _mpsdValidationStatuses = mempty}


-- | The status of the scan of the Docker image container for the model package.
mpsdImageScanStatuses :: Lens' ModelPackageStatusDetails [ModelPackageStatusItem]
mpsdImageScanStatuses = lens _mpsdImageScanStatuses (\ s a -> s{_mpsdImageScanStatuses = a}) . _Default . _Coerce

-- | The validation status of the model package.
mpsdValidationStatuses :: Lens' ModelPackageStatusDetails [ModelPackageStatusItem]
mpsdValidationStatuses = lens _mpsdValidationStatuses (\ s a -> s{_mpsdValidationStatuses = a}) . _Coerce

instance FromJSON ModelPackageStatusDetails where
        parseJSON
          = withObject "ModelPackageStatusDetails"
              (\ x ->
                 ModelPackageStatusDetails' <$>
                   (x .:? "ImageScanStatuses" .!= mempty) <*>
                     (x .:? "ValidationStatuses" .!= mempty))

instance Hashable ModelPackageStatusDetails where

instance NFData ModelPackageStatusDetails where

-- | Represents the overall status of a model package.
--
--
--
-- /See:/ 'modelPackageStatusItem' smart constructor.
data ModelPackageStatusItem = ModelPackageStatusItem'
  { _mpsiFailureReason :: !(Maybe Text)
  , _mpsiName          :: !Text
  , _mpsiStatus        :: !DetailedModelPackageStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModelPackageStatusItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsiFailureReason' - if the overall status is @Failed@ , the reason for the failure.
--
-- * 'mpsiName' - The name of the model package for which the overall status is being reported.
--
-- * 'mpsiStatus' - The current status.
modelPackageStatusItem
    :: Text -- ^ 'mpsiName'
    -> DetailedModelPackageStatus -- ^ 'mpsiStatus'
    -> ModelPackageStatusItem
modelPackageStatusItem pName_ pStatus_ =
  ModelPackageStatusItem'
    {_mpsiFailureReason = Nothing, _mpsiName = pName_, _mpsiStatus = pStatus_}


-- | if the overall status is @Failed@ , the reason for the failure.
mpsiFailureReason :: Lens' ModelPackageStatusItem (Maybe Text)
mpsiFailureReason = lens _mpsiFailureReason (\ s a -> s{_mpsiFailureReason = a})

-- | The name of the model package for which the overall status is being reported.
mpsiName :: Lens' ModelPackageStatusItem Text
mpsiName = lens _mpsiName (\ s a -> s{_mpsiName = a})

-- | The current status.
mpsiStatus :: Lens' ModelPackageStatusItem DetailedModelPackageStatus
mpsiStatus = lens _mpsiStatus (\ s a -> s{_mpsiStatus = a})

instance FromJSON ModelPackageStatusItem where
        parseJSON
          = withObject "ModelPackageStatusItem"
              (\ x ->
                 ModelPackageStatusItem' <$>
                   (x .:? "FailureReason") <*> (x .: "Name") <*>
                     (x .: "Status"))

instance Hashable ModelPackageStatusItem where

instance NFData ModelPackageStatusItem where

-- | Provides summary information about a model package.
--
--
--
-- /See:/ 'modelPackageSummary' smart constructor.
data ModelPackageSummary = ModelPackageSummary'
  { _mpsModelPackageDescription :: !(Maybe Text)
  , _mpsModelPackageName        :: !Text
  , _mpsModelPackageARN         :: !Text
  , _mpsCreationTime            :: !POSIX
  , _mpsModelPackageStatus      :: !ModelPackageStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModelPackageSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsModelPackageDescription' - A brief description of the model package.
--
-- * 'mpsModelPackageName' - The name of the model package.
--
-- * 'mpsModelPackageARN' - The Amazon Resource Name (ARN) of the model package.
--
-- * 'mpsCreationTime' - A timestamp that shows when the model package was created.
--
-- * 'mpsModelPackageStatus' - The overall status of the model package.
modelPackageSummary
    :: Text -- ^ 'mpsModelPackageName'
    -> Text -- ^ 'mpsModelPackageARN'
    -> UTCTime -- ^ 'mpsCreationTime'
    -> ModelPackageStatus -- ^ 'mpsModelPackageStatus'
    -> ModelPackageSummary
modelPackageSummary pModelPackageName_ pModelPackageARN_ pCreationTime_ pModelPackageStatus_ =
  ModelPackageSummary'
    { _mpsModelPackageDescription = Nothing
    , _mpsModelPackageName = pModelPackageName_
    , _mpsModelPackageARN = pModelPackageARN_
    , _mpsCreationTime = _Time # pCreationTime_
    , _mpsModelPackageStatus = pModelPackageStatus_
    }


-- | A brief description of the model package.
mpsModelPackageDescription :: Lens' ModelPackageSummary (Maybe Text)
mpsModelPackageDescription = lens _mpsModelPackageDescription (\ s a -> s{_mpsModelPackageDescription = a})

-- | The name of the model package.
mpsModelPackageName :: Lens' ModelPackageSummary Text
mpsModelPackageName = lens _mpsModelPackageName (\ s a -> s{_mpsModelPackageName = a})

-- | The Amazon Resource Name (ARN) of the model package.
mpsModelPackageARN :: Lens' ModelPackageSummary Text
mpsModelPackageARN = lens _mpsModelPackageARN (\ s a -> s{_mpsModelPackageARN = a})

-- | A timestamp that shows when the model package was created.
mpsCreationTime :: Lens' ModelPackageSummary UTCTime
mpsCreationTime = lens _mpsCreationTime (\ s a -> s{_mpsCreationTime = a}) . _Time

-- | The overall status of the model package.
mpsModelPackageStatus :: Lens' ModelPackageSummary ModelPackageStatus
mpsModelPackageStatus = lens _mpsModelPackageStatus (\ s a -> s{_mpsModelPackageStatus = a})

instance FromJSON ModelPackageSummary where
        parseJSON
          = withObject "ModelPackageSummary"
              (\ x ->
                 ModelPackageSummary' <$>
                   (x .:? "ModelPackageDescription") <*>
                     (x .: "ModelPackageName")
                     <*> (x .: "ModelPackageArn")
                     <*> (x .: "CreationTime")
                     <*> (x .: "ModelPackageStatus"))

instance Hashable ModelPackageSummary where

instance NFData ModelPackageSummary where

-- | Contains data, such as the inputs and targeted instance types that are used in the process of validating the model package.
--
--
-- The data provided in the validation profile is made available to your buyers on AWS Marketplace.
--
--
-- /See:/ 'modelPackageValidationProfile' smart constructor.
data ModelPackageValidationProfile = ModelPackageValidationProfile'
  { _mpvpProfileName            :: !Text
  , _mpvpTransformJobDefinition :: !TransformJobDefinition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModelPackageValidationProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpvpProfileName' - The name of the profile for the model package.
--
-- * 'mpvpTransformJobDefinition' - The @TransformJobDefinition@ object that describes the transform job used for the validation of the model package.
modelPackageValidationProfile
    :: Text -- ^ 'mpvpProfileName'
    -> TransformJobDefinition -- ^ 'mpvpTransformJobDefinition'
    -> ModelPackageValidationProfile
modelPackageValidationProfile pProfileName_ pTransformJobDefinition_ =
  ModelPackageValidationProfile'
    { _mpvpProfileName = pProfileName_
    , _mpvpTransformJobDefinition = pTransformJobDefinition_
    }


-- | The name of the profile for the model package.
mpvpProfileName :: Lens' ModelPackageValidationProfile Text
mpvpProfileName = lens _mpvpProfileName (\ s a -> s{_mpvpProfileName = a})

-- | The @TransformJobDefinition@ object that describes the transform job used for the validation of the model package.
mpvpTransformJobDefinition :: Lens' ModelPackageValidationProfile TransformJobDefinition
mpvpTransformJobDefinition = lens _mpvpTransformJobDefinition (\ s a -> s{_mpvpTransformJobDefinition = a})

instance FromJSON ModelPackageValidationProfile where
        parseJSON
          = withObject "ModelPackageValidationProfile"
              (\ x ->
                 ModelPackageValidationProfile' <$>
                   (x .: "ProfileName") <*>
                     (x .: "TransformJobDefinition"))

instance Hashable ModelPackageValidationProfile where

instance NFData ModelPackageValidationProfile where

instance ToJSON ModelPackageValidationProfile where
        toJSON ModelPackageValidationProfile'{..}
          = object
              (catMaybes
                 [Just ("ProfileName" .= _mpvpProfileName),
                  Just
                    ("TransformJobDefinition" .=
                       _mpvpTransformJobDefinition)])

-- | Specifies batch transform jobs that Amazon SageMaker runs to validate your model package.
--
--
--
-- /See:/ 'modelPackageValidationSpecification' smart constructor.
data ModelPackageValidationSpecification = ModelPackageValidationSpecification'
  { _mpvsValidationRole     :: !Text
  , _mpvsValidationProfiles :: !(List1 ModelPackageValidationProfile)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModelPackageValidationSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpvsValidationRole' - The IAM roles to be used for the validation of the model package.
--
-- * 'mpvsValidationProfiles' - An array of @ModelPackageValidationProfile@ objects, each of which specifies a batch transform job that Amazon SageMaker runs to validate your model package.
modelPackageValidationSpecification
    :: Text -- ^ 'mpvsValidationRole'
    -> NonEmpty ModelPackageValidationProfile -- ^ 'mpvsValidationProfiles'
    -> ModelPackageValidationSpecification
modelPackageValidationSpecification pValidationRole_ pValidationProfiles_ =
  ModelPackageValidationSpecification'
    { _mpvsValidationRole = pValidationRole_
    , _mpvsValidationProfiles = _List1 # pValidationProfiles_
    }


-- | The IAM roles to be used for the validation of the model package.
mpvsValidationRole :: Lens' ModelPackageValidationSpecification Text
mpvsValidationRole = lens _mpvsValidationRole (\ s a -> s{_mpvsValidationRole = a})

-- | An array of @ModelPackageValidationProfile@ objects, each of which specifies a batch transform job that Amazon SageMaker runs to validate your model package.
mpvsValidationProfiles :: Lens' ModelPackageValidationSpecification (NonEmpty ModelPackageValidationProfile)
mpvsValidationProfiles = lens _mpvsValidationProfiles (\ s a -> s{_mpvsValidationProfiles = a}) . _List1

instance FromJSON ModelPackageValidationSpecification
         where
        parseJSON
          = withObject "ModelPackageValidationSpecification"
              (\ x ->
                 ModelPackageValidationSpecification' <$>
                   (x .: "ValidationRole") <*>
                     (x .: "ValidationProfiles"))

instance Hashable ModelPackageValidationSpecification
         where

instance NFData ModelPackageValidationSpecification
         where

instance ToJSON ModelPackageValidationSpecification
         where
        toJSON ModelPackageValidationSpecification'{..}
          = object
              (catMaybes
                 [Just ("ValidationRole" .= _mpvsValidationRole),
                  Just
                    ("ValidationProfiles" .= _mpvsValidationProfiles)])

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

-- | Defines a list of @NestedFilters@ objects. To satisfy the conditions specified in the @NestedFilters@ call, a resource must satisfy the conditions of all of the filters.
--
--
-- For example, you could define a @NestedFilters@ using the training job's @InputDataConfig@ property to filter on @Channel@ objects.
--
-- A @NestedFilters@ object contains multiple filters. For example, to find all training jobs whose name contains @train@ and that have @cat/data@ in their @S3Uri@ (specified in @InputDataConfig@ ), you need to create a @NestedFilters@ object that specifies the @InputDataConfig@ property with the following @Filter@ objects:
--
--     * @'{Name:"InputDataConfig.ChannelName", "Operator":"EQUALS", "Value":"train"}',@
--
--     * @'{Name:"InputDataConfig.DataSource.S3DataSource.S3Uri", "Operator":"CONTAINS", "Value":"cat/data"}'@
--
--
--
--
-- /See:/ 'nestedFilters' smart constructor.
data NestedFilters = NestedFilters'
  { _nfNestedPropertyName :: !Text
  , _nfFilters            :: !(List1 Filter)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NestedFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nfNestedPropertyName' - The name of the property to use in the nested filters. The value must match a listed property name, such as @InputDataConfig@ .
--
-- * 'nfFilters' - A list of filters. Each filter acts on a property. Filters must contain at least one @Filters@ value. For example, a @NestedFilters@ call might include a filter on the @PropertyName@ parameter of the @InputDataConfig@ property: @InputDataConfig.DataSource.S3DataSource.S3Uri@ .
nestedFilters
    :: Text -- ^ 'nfNestedPropertyName'
    -> NonEmpty Filter -- ^ 'nfFilters'
    -> NestedFilters
nestedFilters pNestedPropertyName_ pFilters_ =
  NestedFilters'
    { _nfNestedPropertyName = pNestedPropertyName_
    , _nfFilters = _List1 # pFilters_
    }


-- | The name of the property to use in the nested filters. The value must match a listed property name, such as @InputDataConfig@ .
nfNestedPropertyName :: Lens' NestedFilters Text
nfNestedPropertyName = lens _nfNestedPropertyName (\ s a -> s{_nfNestedPropertyName = a})

-- | A list of filters. Each filter acts on a property. Filters must contain at least one @Filters@ value. For example, a @NestedFilters@ call might include a filter on the @PropertyName@ parameter of the @InputDataConfig@ property: @InputDataConfig.DataSource.S3DataSource.S3Uri@ .
nfFilters :: Lens' NestedFilters (NonEmpty Filter)
nfFilters = lens _nfFilters (\ s a -> s{_nfFilters = a}) . _List1

instance Hashable NestedFilters where

instance NFData NestedFilters where

instance ToJSON NestedFilters where
        toJSON NestedFilters'{..}
          = object
              (catMaybes
                 [Just
                    ("NestedPropertyName" .= _nfNestedPropertyName),
                  Just ("Filters" .= _nfFilters)])

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
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
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
  , _nisAdditionalCodeRepositories          :: !(Maybe [Text])
  , _nisURL                                 :: !(Maybe Text)
  , _nisLastModifiedTime                    :: !(Maybe POSIX)
  , _nisInstanceType                        :: !(Maybe InstanceType)
  , _nisNotebookInstanceStatus              :: !(Maybe NotebookInstanceStatus)
  , _nisDefaultCodeRepository               :: !(Maybe Text)
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
-- * 'nisAdditionalCodeRepositories' - An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- * 'nisURL' - The URL that you use to connect to the Jupyter instance running in your notebook instance.
--
-- * 'nisLastModifiedTime' - A timestamp that shows when the notebook instance was last modified.
--
-- * 'nisInstanceType' - The type of ML compute instance that the notebook instance is running on.
--
-- * 'nisNotebookInstanceStatus' - The status of the notebook instance.
--
-- * 'nisDefaultCodeRepository' - The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- * 'nisNotebookInstanceLifecycleConfigName' - The name of a notebook instance lifecycle configuration associated with this notebook instance. For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
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
    , _nisAdditionalCodeRepositories = Nothing
    , _nisURL = Nothing
    , _nisLastModifiedTime = Nothing
    , _nisInstanceType = Nothing
    , _nisNotebookInstanceStatus = Nothing
    , _nisDefaultCodeRepository = Nothing
    , _nisNotebookInstanceLifecycleConfigName = Nothing
    , _nisNotebookInstanceName = pNotebookInstanceName_
    , _nisNotebookInstanceARN = pNotebookInstanceARN_
    }


-- | A timestamp that shows when the notebook instance was created.
nisCreationTime :: Lens' NotebookInstanceSummary (Maybe UTCTime)
nisCreationTime = lens _nisCreationTime (\ s a -> s{_nisCreationTime = a}) . mapping _Time

-- | An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
nisAdditionalCodeRepositories :: Lens' NotebookInstanceSummary [Text]
nisAdditionalCodeRepositories = lens _nisAdditionalCodeRepositories (\ s a -> s{_nisAdditionalCodeRepositories = a}) . _Default . _Coerce

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

-- | The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
nisDefaultCodeRepository :: Lens' NotebookInstanceSummary (Maybe Text)
nisDefaultCodeRepository = lens _nisDefaultCodeRepository (\ s a -> s{_nisDefaultCodeRepository = a})

-- | The name of a notebook instance lifecycle configuration associated with this notebook instance. For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
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
                   (x .:? "CreationTime") <*>
                     (x .:? "AdditionalCodeRepositories" .!= mempty)
                     <*> (x .:? "Url")
                     <*> (x .:? "LastModifiedTime")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "NotebookInstanceStatus")
                     <*> (x .:? "DefaultCodeRepository")
                     <*> (x .:? "NotebookInstanceLifecycleConfigName")
                     <*> (x .: "NotebookInstanceName")
                     <*> (x .: "NotebookInstanceArn"))

instance Hashable NotebookInstanceSummary where

instance NFData NotebookInstanceSummary where

-- | Specifies the number of training jobs that this hyperparameter tuning job launched, categorized by the status of their objective metric. The objective metric status shows whether the final objective metric for the training job has been evaluated by the tuning job and used in the hyperparameter tuning process.
--
--
--
-- /See:/ 'objectiveStatusCounters' smart constructor.
data ObjectiveStatusCounters = ObjectiveStatusCounters'
  { _oscPending   :: !(Maybe Nat)
  , _oscSucceeded :: !(Maybe Nat)
  , _oscFailed    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectiveStatusCounters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oscPending' - The number of training jobs that are in progress and pending evaluation of their final objective metric.
--
-- * 'oscSucceeded' - The number of training jobs whose final objective metric was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.
--
-- * 'oscFailed' - The number of training jobs whose final objective metric was not evaluated and used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
objectiveStatusCounters
    :: ObjectiveStatusCounters
objectiveStatusCounters =
  ObjectiveStatusCounters'
    {_oscPending = Nothing, _oscSucceeded = Nothing, _oscFailed = Nothing}


-- | The number of training jobs that are in progress and pending evaluation of their final objective metric.
oscPending :: Lens' ObjectiveStatusCounters (Maybe Natural)
oscPending = lens _oscPending (\ s a -> s{_oscPending = a}) . mapping _Nat

-- | The number of training jobs whose final objective metric was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.
oscSucceeded :: Lens' ObjectiveStatusCounters (Maybe Natural)
oscSucceeded = lens _oscSucceeded (\ s a -> s{_oscSucceeded = a}) . mapping _Nat

-- | The number of training jobs whose final objective metric was not evaluated and used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
oscFailed :: Lens' ObjectiveStatusCounters (Maybe Natural)
oscFailed = lens _oscFailed (\ s a -> s{_oscFailed = a}) . mapping _Nat

instance FromJSON ObjectiveStatusCounters where
        parseJSON
          = withObject "ObjectiveStatusCounters"
              (\ x ->
                 ObjectiveStatusCounters' <$>
                   (x .:? "Pending") <*> (x .:? "Succeeded") <*>
                     (x .:? "Failed"))

instance Hashable ObjectiveStatusCounters where

instance NFData ObjectiveStatusCounters where

-- | Contains information about the output location for the compiled model and the device (target) that the model runs on.
--
--
--
-- /See:/ 'outputConfig' smart constructor.
data OutputConfig = OutputConfig'
  { _ocS3OutputLocation :: !Text
  , _ocTargetDevice     :: !TargetDevice
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocS3OutputLocation' - Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, s3://bucket-name/key-name-prefix.
--
-- * 'ocTargetDevice' - Identifies the device that you want to run your model on after it has been compiled. For example: ml_c5.
outputConfig
    :: Text -- ^ 'ocS3OutputLocation'
    -> TargetDevice -- ^ 'ocTargetDevice'
    -> OutputConfig
outputConfig pS3OutputLocation_ pTargetDevice_ =
  OutputConfig'
    {_ocS3OutputLocation = pS3OutputLocation_, _ocTargetDevice = pTargetDevice_}


-- | Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, s3://bucket-name/key-name-prefix.
ocS3OutputLocation :: Lens' OutputConfig Text
ocS3OutputLocation = lens _ocS3OutputLocation (\ s a -> s{_ocS3OutputLocation = a})

-- | Identifies the device that you want to run your model on after it has been compiled. For example: ml_c5.
ocTargetDevice :: Lens' OutputConfig TargetDevice
ocTargetDevice = lens _ocTargetDevice (\ s a -> s{_ocTargetDevice = a})

instance FromJSON OutputConfig where
        parseJSON
          = withObject "OutputConfig"
              (\ x ->
                 OutputConfig' <$>
                   (x .: "S3OutputLocation") <*> (x .: "TargetDevice"))

instance Hashable OutputConfig where

instance NFData OutputConfig where

instance ToJSON OutputConfig where
        toJSON OutputConfig'{..}
          = object
              (catMaybes
                 [Just ("S3OutputLocation" .= _ocS3OutputLocation),
                  Just ("TargetDevice" .= _ocTargetDevice)])

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
-- * 'odcKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats:      * // KMS Key ID @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * // Amazon Resource Name (ARN) of a KMS Key @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@      * // KMS Key Alias @"alias/ExampleAlias"@      * // Amazon Resource Name (ARN) of a KMS Key Alias @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@  If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./  The KMS key policy must grant permission to the IAM role that you specify in your @CreateTramsformJob@ request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'odcS3OutputPath' - Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
outputDataConfig
    :: Text -- ^ 'odcS3OutputPath'
    -> OutputDataConfig
outputDataConfig pS3OutputPath_ =
  OutputDataConfig' {_odcKMSKeyId = Nothing, _odcS3OutputPath = pS3OutputPath_}


-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats:      * // KMS Key ID @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * // Amazon Resource Name (ARN) of a KMS Key @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@      * // KMS Key Alias @"alias/ExampleAlias"@      * // Amazon Resource Name (ARN) of a KMS Key Alias @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@  If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./  The KMS key policy must grant permission to the IAM role that you specify in your @CreateTramsformJob@ request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
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

-- | Defines the possible values for categorical, continuous, and integer hyperparameters to be used by an algorithm.
--
--
--
-- /See:/ 'parameterRange' smart constructor.
data ParameterRange = ParameterRange'
  { _prCategoricalParameterRangeSpecification :: !(Maybe CategoricalParameterRangeSpecification)
  , _prIntegerParameterRangeSpecification :: !(Maybe IntegerParameterRangeSpecification)
  , _prContinuousParameterRangeSpecification :: !(Maybe ContinuousParameterRangeSpecification)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prCategoricalParameterRangeSpecification' - A @CategoricalParameterRangeSpecification@ object that defines the possible values for a categorical hyperparameter.
--
-- * 'prIntegerParameterRangeSpecification' - A @IntegerParameterRangeSpecification@ object that defines the possible values for an integer hyperparameter.
--
-- * 'prContinuousParameterRangeSpecification' - A @ContinuousParameterRangeSpecification@ object that defines the possible values for a continuous hyperparameter.
parameterRange
    :: ParameterRange
parameterRange =
  ParameterRange'
    { _prCategoricalParameterRangeSpecification = Nothing
    , _prIntegerParameterRangeSpecification = Nothing
    , _prContinuousParameterRangeSpecification = Nothing
    }


-- | A @CategoricalParameterRangeSpecification@ object that defines the possible values for a categorical hyperparameter.
prCategoricalParameterRangeSpecification :: Lens' ParameterRange (Maybe CategoricalParameterRangeSpecification)
prCategoricalParameterRangeSpecification = lens _prCategoricalParameterRangeSpecification (\ s a -> s{_prCategoricalParameterRangeSpecification = a})

-- | A @IntegerParameterRangeSpecification@ object that defines the possible values for an integer hyperparameter.
prIntegerParameterRangeSpecification :: Lens' ParameterRange (Maybe IntegerParameterRangeSpecification)
prIntegerParameterRangeSpecification = lens _prIntegerParameterRangeSpecification (\ s a -> s{_prIntegerParameterRangeSpecification = a})

-- | A @ContinuousParameterRangeSpecification@ object that defines the possible values for a continuous hyperparameter.
prContinuousParameterRangeSpecification :: Lens' ParameterRange (Maybe ContinuousParameterRangeSpecification)
prContinuousParameterRangeSpecification = lens _prContinuousParameterRangeSpecification (\ s a -> s{_prContinuousParameterRangeSpecification = a})

instance FromJSON ParameterRange where
        parseJSON
          = withObject "ParameterRange"
              (\ x ->
                 ParameterRange' <$>
                   (x .:? "CategoricalParameterRangeSpecification") <*>
                     (x .:? "IntegerParameterRangeSpecification")
                     <*> (x .:? "ContinuousParameterRangeSpecification"))

instance Hashable ParameterRange where

instance NFData ParameterRange where

instance ToJSON ParameterRange where
        toJSON ParameterRange'{..}
          = object
              (catMaybes
                 [("CategoricalParameterRangeSpecification" .=) <$>
                    _prCategoricalParameterRangeSpecification,
                  ("IntegerParameterRangeSpecification" .=) <$>
                    _prIntegerParameterRangeSpecification,
                  ("ContinuousParameterRangeSpecification" .=) <$>
                    _prContinuousParameterRangeSpecification])

-- | Specifies ranges of integer, continuous, and categorical hyperparameters that a hyperparameter tuning job searches. The hyperparameter tuning job launches training jobs with hyperparameter values within these ranges to find the combination of values that result in the training job with the best performance as measured by the objective metric of the hyperparameter tuning job.
--
--
--
-- /See:/ 'parameterRanges' smart constructor.
data ParameterRanges = ParameterRanges'
  { _prCategoricalParameterRanges :: !(Maybe [CategoricalParameterRange])
  , _prIntegerParameterRanges     :: !(Maybe [IntegerParameterRange])
  , _prContinuousParameterRanges  :: !(Maybe [ContinuousParameterRange])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterRanges' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prCategoricalParameterRanges' - The array of 'CategoricalParameterRange' objects that specify ranges of categorical hyperparameters that a hyperparameter tuning job searches.
--
-- * 'prIntegerParameterRanges' - The array of 'IntegerParameterRange' objects that specify ranges of integer hyperparameters that a hyperparameter tuning job searches.
--
-- * 'prContinuousParameterRanges' - The array of 'ContinuousParameterRange' objects that specify ranges of continuous hyperparameters that a hyperparameter tuning job searches.
parameterRanges
    :: ParameterRanges
parameterRanges =
  ParameterRanges'
    { _prCategoricalParameterRanges = Nothing
    , _prIntegerParameterRanges = Nothing
    , _prContinuousParameterRanges = Nothing
    }


-- | The array of 'CategoricalParameterRange' objects that specify ranges of categorical hyperparameters that a hyperparameter tuning job searches.
prCategoricalParameterRanges :: Lens' ParameterRanges [CategoricalParameterRange]
prCategoricalParameterRanges = lens _prCategoricalParameterRanges (\ s a -> s{_prCategoricalParameterRanges = a}) . _Default . _Coerce

-- | The array of 'IntegerParameterRange' objects that specify ranges of integer hyperparameters that a hyperparameter tuning job searches.
prIntegerParameterRanges :: Lens' ParameterRanges [IntegerParameterRange]
prIntegerParameterRanges = lens _prIntegerParameterRanges (\ s a -> s{_prIntegerParameterRanges = a}) . _Default . _Coerce

-- | The array of 'ContinuousParameterRange' objects that specify ranges of continuous hyperparameters that a hyperparameter tuning job searches.
prContinuousParameterRanges :: Lens' ParameterRanges [ContinuousParameterRange]
prContinuousParameterRanges = lens _prContinuousParameterRanges (\ s a -> s{_prContinuousParameterRanges = a}) . _Default . _Coerce

instance FromJSON ParameterRanges where
        parseJSON
          = withObject "ParameterRanges"
              (\ x ->
                 ParameterRanges' <$>
                   (x .:? "CategoricalParameterRanges" .!= mempty) <*>
                     (x .:? "IntegerParameterRanges" .!= mempty)
                     <*> (x .:? "ContinuousParameterRanges" .!= mempty))

instance Hashable ParameterRanges where

instance NFData ParameterRanges where

instance ToJSON ParameterRanges where
        toJSON ParameterRanges'{..}
          = object
              (catMaybes
                 [("CategoricalParameterRanges" .=) <$>
                    _prCategoricalParameterRanges,
                  ("IntegerParameterRanges" .=) <$>
                    _prIntegerParameterRanges,
                  ("ContinuousParameterRanges" .=) <$>
                    _prContinuousParameterRanges])

-- | A previously completed or stopped hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
--
--
--
-- /See:/ 'parentHyperParameterTuningJob' smart constructor.
newtype ParentHyperParameterTuningJob = ParentHyperParameterTuningJob'
  { _phptjHyperParameterTuningJobName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParentHyperParameterTuningJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phptjHyperParameterTuningJobName' - The name of the hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
parentHyperParameterTuningJob
    :: ParentHyperParameterTuningJob
parentHyperParameterTuningJob =
  ParentHyperParameterTuningJob' {_phptjHyperParameterTuningJobName = Nothing}


-- | The name of the hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
phptjHyperParameterTuningJobName :: Lens' ParentHyperParameterTuningJob (Maybe Text)
phptjHyperParameterTuningJobName = lens _phptjHyperParameterTuningJobName (\ s a -> s{_phptjHyperParameterTuningJobName = a})

instance FromJSON ParentHyperParameterTuningJob where
        parseJSON
          = withObject "ParentHyperParameterTuningJob"
              (\ x ->
                 ParentHyperParameterTuningJob' <$>
                   (x .:? "HyperParameterTuningJobName"))

instance Hashable ParentHyperParameterTuningJob where

instance NFData ParentHyperParameterTuningJob where

instance ToJSON ParentHyperParameterTuningJob where
        toJSON ParentHyperParameterTuningJob'{..}
          = object
              (catMaybes
                 [("HyperParameterTuningJobName" .=) <$>
                    _phptjHyperParameterTuningJobName])

-- | Identifies a model that you want to host and the resources to deploy for hosting it. If you are deploying multiple models, tell Amazon SageMaker how to distribute traffic among the models by specifying variant weights.
--
--
--
-- /See:/ 'productionVariant' smart constructor.
data ProductionVariant = ProductionVariant'
  { _pvAcceleratorType      :: !(Maybe ProductionVariantAcceleratorType)
  , _pvInitialVariantWeight :: !(Maybe Double)
  , _pvVariantName          :: !Text
  , _pvModelName            :: !Text
  , _pvInitialInstanceCount :: !Nat
  , _pvInstanceType         :: !ProductionVariantInstanceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProductionVariant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvAcceleratorType' - The size of the Elastic Inference (EI) instance to use for the production variant. EI instances provide on-demand GPU computing for inference. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> . For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
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
    { _pvAcceleratorType = Nothing
    , _pvInitialVariantWeight = Nothing
    , _pvVariantName = pVariantName_
    , _pvModelName = pModelName_
    , _pvInitialInstanceCount = _Nat # pInitialInstanceCount_
    , _pvInstanceType = pInstanceType_
    }


-- | The size of the Elastic Inference (EI) instance to use for the production variant. EI instances provide on-demand GPU computing for inference. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> . For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
pvAcceleratorType :: Lens' ProductionVariant (Maybe ProductionVariantAcceleratorType)
pvAcceleratorType = lens _pvAcceleratorType (\ s a -> s{_pvAcceleratorType = a})

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
                   (x .:? "AcceleratorType") <*>
                     (x .:? "InitialVariantWeight")
                     <*> (x .: "VariantName")
                     <*> (x .: "ModelName")
                     <*> (x .: "InitialInstanceCount")
                     <*> (x .: "InstanceType"))

instance Hashable ProductionVariant where

instance NFData ProductionVariant where

instance ToJSON ProductionVariant where
        toJSON ProductionVariant'{..}
          = object
              (catMaybes
                 [("AcceleratorType" .=) <$> _pvAcceleratorType,
                  ("InitialVariantWeight" .=) <$>
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
  , _pvsDeployedImages       :: !(Maybe [DeployedImage])
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
-- * 'pvsDeployedImages' - An array of @DeployedImage@ objects that specify the Amazon EC2 Container Registry paths of the inference images deployed on instances of this @ProductionVariant@ .
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
    , _pvsDeployedImages = Nothing
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

-- | An array of @DeployedImage@ objects that specify the Amazon EC2 Container Registry paths of the inference images deployed on instances of this @ProductionVariant@ .
pvsDeployedImages :: Lens' ProductionVariantSummary [DeployedImage]
pvsDeployedImages = lens _pvsDeployedImages (\ s a -> s{_pvsDeployedImages = a}) . _Default . _Coerce

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
                     <*> (x .:? "DeployedImages" .!= mempty)
                     <*> (x .: "VariantName"))

instance Hashable ProductionVariantSummary where

instance NFData ProductionVariantSummary where

-- | A type of @SuggestionQuery@ . A suggestion query for retrieving property names that match the specified hint.
--
--
--
-- /See:/ 'propertyNameQuery' smart constructor.
newtype PropertyNameQuery = PropertyNameQuery'
  { _pnqPropertyNameHint :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertyNameQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pnqPropertyNameHint' - Text that is part of a property's name. The property names of hyperparameter, metric, and tag key names that begin with the specified text in the @PropertyNameHint@ .
propertyNameQuery
    :: Text -- ^ 'pnqPropertyNameHint'
    -> PropertyNameQuery
propertyNameQuery pPropertyNameHint_ =
  PropertyNameQuery' {_pnqPropertyNameHint = pPropertyNameHint_}


-- | Text that is part of a property's name. The property names of hyperparameter, metric, and tag key names that begin with the specified text in the @PropertyNameHint@ .
pnqPropertyNameHint :: Lens' PropertyNameQuery Text
pnqPropertyNameHint = lens _pnqPropertyNameHint (\ s a -> s{_pnqPropertyNameHint = a})

instance Hashable PropertyNameQuery where

instance NFData PropertyNameQuery where

instance ToJSON PropertyNameQuery where
        toJSON PropertyNameQuery'{..}
          = object
              (catMaybes
                 [Just ("PropertyNameHint" .= _pnqPropertyNameHint)])

-- | A property name returned from a @GetSearchSuggestions@ call that specifies a value in the @PropertyNameQuery@ field.
--
--
--
-- /See:/ 'propertyNameSuggestion' smart constructor.
newtype PropertyNameSuggestion = PropertyNameSuggestion'
  { _pnsPropertyName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropertyNameSuggestion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pnsPropertyName' - A suggested property name based on what you entered in the search textbox in the Amazon SageMaker console.
propertyNameSuggestion
    :: PropertyNameSuggestion
propertyNameSuggestion = PropertyNameSuggestion' {_pnsPropertyName = Nothing}


-- | A suggested property name based on what you entered in the search textbox in the Amazon SageMaker console.
pnsPropertyName :: Lens' PropertyNameSuggestion (Maybe Text)
pnsPropertyName = lens _pnsPropertyName (\ s a -> s{_pnsPropertyName = a})

instance FromJSON PropertyNameSuggestion where
        parseJSON
          = withObject "PropertyNameSuggestion"
              (\ x ->
                 PropertyNameSuggestion' <$> (x .:? "PropertyName"))

instance Hashable PropertyNameSuggestion where

instance NFData PropertyNameSuggestion where

-- | Defines the amount of money paid to an Amazon Mechanical Turk worker for each task performed.
--
--
-- Use one of the following prices for bounding box tasks. Prices are in US dollars.
--
--     * 0.036
--
--     * 0.048
--
--     * 0.060
--
--     * 0.072
--
--     * 0.120
--
--     * 0.240
--
--     * 0.360
--
--     * 0.480
--
--     * 0.600
--
--     * 0.720
--
--     * 0.840
--
--     * 0.960
--
--     * 1.080
--
--     * 1.200
--
--
--
-- Use one of the following prices for image classification, text classification, and custom tasks. Prices are in US dollars.
--
--     * 0.012
--
--     * 0.024
--
--     * 0.036
--
--     * 0.048
--
--     * 0.060
--
--     * 0.072
--
--     * 0.120
--
--     * 0.240
--
--     * 0.360
--
--     * 0.480
--
--     * 0.600
--
--     * 0.720
--
--     * 0.840
--
--     * 0.960
--
--     * 1.080
--
--     * 1.200
--
--
--
-- Use one of the following prices for semantic segmentation tasks. Prices are in US dollars.
--
--     * 0.840
--
--     * 0.960
--
--     * 1.080
--
--     * 1.200
--
--
--
--
-- /See:/ 'publicWorkforceTaskPrice' smart constructor.
newtype PublicWorkforceTaskPrice = PublicWorkforceTaskPrice'
  { _pwtpAmountInUsd :: Maybe USD
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublicWorkforceTaskPrice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwtpAmountInUsd' - Defines the amount of money paid to a worker in United States dollars.
publicWorkforceTaskPrice
    :: PublicWorkforceTaskPrice
publicWorkforceTaskPrice =
  PublicWorkforceTaskPrice' {_pwtpAmountInUsd = Nothing}


-- | Defines the amount of money paid to a worker in United States dollars.
pwtpAmountInUsd :: Lens' PublicWorkforceTaskPrice (Maybe USD)
pwtpAmountInUsd = lens _pwtpAmountInUsd (\ s a -> s{_pwtpAmountInUsd = a})

instance FromJSON PublicWorkforceTaskPrice where
        parseJSON
          = withObject "PublicWorkforceTaskPrice"
              (\ x ->
                 PublicWorkforceTaskPrice' <$> (x .:? "AmountInUsd"))

instance Hashable PublicWorkforceTaskPrice where

instance NFData PublicWorkforceTaskPrice where

instance ToJSON PublicWorkforceTaskPrice where
        toJSON PublicWorkforceTaskPrice'{..}
          = object
              (catMaybes [("AmountInUsd" .=) <$> _pwtpAmountInUsd])

-- | Contains input values for a task.
--
--
--
-- /See:/ 'renderableTask' smart constructor.
newtype RenderableTask = RenderableTask'
  { _rtInput :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RenderableTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtInput' - A JSON object that contains values for the variables defined in the template. It is made available to the template under the substitution variable @task.input@ . For example, if you define a variable @task.input.text@ in your template, you can supply the variable in the JSON object as @"text": "sample text"@ .
renderableTask
    :: Text -- ^ 'rtInput'
    -> RenderableTask
renderableTask pInput_ = RenderableTask' {_rtInput = pInput_}


-- | A JSON object that contains values for the variables defined in the template. It is made available to the template under the substitution variable @task.input@ . For example, if you define a variable @task.input.text@ in your template, you can supply the variable in the JSON object as @"text": "sample text"@ .
rtInput :: Lens' RenderableTask Text
rtInput = lens _rtInput (\ s a -> s{_rtInput = a})

instance Hashable RenderableTask where

instance NFData RenderableTask where

instance ToJSON RenderableTask where
        toJSON RenderableTask'{..}
          = object (catMaybes [Just ("Input" .= _rtInput)])

-- | A description of an error that occurred while rendering the template.
--
--
--
-- /See:/ 'renderingError' smart constructor.
data RenderingError = RenderingError'
  { _reCode    :: !Text
  , _reMessage :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RenderingError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reCode' - A unique identifier for a specific class of errors.
--
-- * 'reMessage' - A human-readable message describing the error.
renderingError
    :: Text -- ^ 'reCode'
    -> Text -- ^ 'reMessage'
    -> RenderingError
renderingError pCode_ pMessage_ =
  RenderingError' {_reCode = pCode_, _reMessage = pMessage_}


-- | A unique identifier for a specific class of errors.
reCode :: Lens' RenderingError Text
reCode = lens _reCode (\ s a -> s{_reCode = a})

-- | A human-readable message describing the error.
reMessage :: Lens' RenderingError Text
reMessage = lens _reMessage (\ s a -> s{_reMessage = a})

instance FromJSON RenderingError where
        parseJSON
          = withObject "RenderingError"
              (\ x ->
                 RenderingError' <$>
                   (x .: "Code") <*> (x .: "Message"))

instance Hashable RenderingError where

instance NFData RenderingError where

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
-- * 'rcVolumeKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job. The @VolumeKmsKeyId@ can be any of the following formats:     * // KMS Key ID @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * // Amazon Resource Name (ARN) of a KMS Key @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
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


-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job. The @VolumeKmsKeyId@ can be any of the following formats:     * // KMS Key ID @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * // Amazon Resource Name (ARN) of a KMS Key @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
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

-- | Specifies the maximum number of training jobs and parallel training jobs that a hyperparameter tuning job can launch.
--
--
--
-- /See:/ 'resourceLimits' smart constructor.
data ResourceLimits = ResourceLimits'
  { _rlMaxNumberOfTrainingJobs :: !Nat
  , _rlMaxParallelTrainingJobs :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlMaxNumberOfTrainingJobs' - The maximum number of training jobs that a hyperparameter tuning job can launch.
--
-- * 'rlMaxParallelTrainingJobs' - The maximum number of concurrent training jobs that a hyperparameter tuning job can launch.
resourceLimits
    :: Natural -- ^ 'rlMaxNumberOfTrainingJobs'
    -> Natural -- ^ 'rlMaxParallelTrainingJobs'
    -> ResourceLimits
resourceLimits pMaxNumberOfTrainingJobs_ pMaxParallelTrainingJobs_ =
  ResourceLimits'
    { _rlMaxNumberOfTrainingJobs = _Nat # pMaxNumberOfTrainingJobs_
    , _rlMaxParallelTrainingJobs = _Nat # pMaxParallelTrainingJobs_
    }


-- | The maximum number of training jobs that a hyperparameter tuning job can launch.
rlMaxNumberOfTrainingJobs :: Lens' ResourceLimits Natural
rlMaxNumberOfTrainingJobs = lens _rlMaxNumberOfTrainingJobs (\ s a -> s{_rlMaxNumberOfTrainingJobs = a}) . _Nat

-- | The maximum number of concurrent training jobs that a hyperparameter tuning job can launch.
rlMaxParallelTrainingJobs :: Lens' ResourceLimits Natural
rlMaxParallelTrainingJobs = lens _rlMaxParallelTrainingJobs (\ s a -> s{_rlMaxParallelTrainingJobs = a}) . _Nat

instance FromJSON ResourceLimits where
        parseJSON
          = withObject "ResourceLimits"
              (\ x ->
                 ResourceLimits' <$>
                   (x .: "MaxNumberOfTrainingJobs") <*>
                     (x .: "MaxParallelTrainingJobs"))

instance Hashable ResourceLimits where

instance NFData ResourceLimits where

instance ToJSON ResourceLimits where
        toJSON ResourceLimits'{..}
          = object
              (catMaybes
                 [Just
                    ("MaxNumberOfTrainingJobs" .=
                       _rlMaxNumberOfTrainingJobs),
                  Just
                    ("MaxParallelTrainingJobs" .=
                       _rlMaxParallelTrainingJobs)])

-- | Describes the S3 data source.
--
--
--
-- /See:/ 's3DataSource' smart constructor.
data S3DataSource = S3DataSource'
  { _sdsS3DataDistributionType :: !(Maybe S3DataDistribution)
  , _sdsAttributeNames         :: !(Maybe [Text])
  , _sdsS3DataType             :: !S3DataType
  , _sdsS3URI                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsS3DataDistributionType' - If you want Amazon SageMaker to replicate the entire dataset on each ML compute instance that is launched for model training, specify @FullyReplicated@ .  If you want Amazon SageMaker to replicate a subset of data on each ML compute instance that is launched for model training, specify @ShardedByS3Key@ . If there are /n/ ML compute instances launched for a training job, each instance gets approximately 1//n/ of the number of S3 objects. In this case, model training on each machine uses only the subset of training data.  Don't choose more ML compute instances for training than available S3 objects. If you do, some nodes won't get any data and you will pay for nodes that aren't getting any training data. This applies in both File and Pipe modes. Keep this in mind when developing algorithms.  In distributed training, where you use multiple ML compute EC2 instances, you might choose @ShardedByS3Key@ . If the algorithm requires copying training data to the ML storage volume (when @TrainingInputMode@ is set to @File@ ), this copies 1//n/ of the number of objects.
--
-- * 'sdsAttributeNames' - A list of one or more attribute names to use that are found in a specified augmented manifest file.
--
-- * 'sdsS3DataType' - If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects that match the specified key name prefix for model training.  If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for model training.  If you choose @AugmentedManifestFile@ , S3Uri identifies an object that is an augmented manifest file in JSON lines format. This file contains the data you want to use for model training. @AugmentedManifestFile@ can only be used if the Channel's input mode is @Pipe@ .
--
-- * 'sdsS3URI' - Depending on the value specified for the @S3DataType@ , identifies either a key name prefix or a manifest. For example:      * A key name prefix might look like this: @s3://bucketname/exampleprefix@ .      * A manifest might look like this: @s3://bucketname/example.manifest@  The manifest is an S3 object which is a JSON file with the following format:  @[@  @{"prefix": "s3://customer_bucket/some/prefix/"},@  @"relative/path/to/custdata-1",@  @"relative/path/custdata-2",@  @...@  @]@  The preceding JSON matches the following @s3Uris@ :  @s3://customer_bucket/some/prefix/relative/path/to/custdata-1@  @s3://customer_bucket/some/prefix/relative/path/custdata-2@  @...@  The complete set of @s3uris@ in this manifest is the input data for the channel for this datasource. The object that each @s3uris@ points to must be readable by the IAM role that Amazon SageMaker uses to perform tasks on your behalf.
s3DataSource
    :: S3DataType -- ^ 'sdsS3DataType'
    -> Text -- ^ 'sdsS3URI'
    -> S3DataSource
s3DataSource pS3DataType_ pS3URI_ =
  S3DataSource'
    { _sdsS3DataDistributionType = Nothing
    , _sdsAttributeNames = Nothing
    , _sdsS3DataType = pS3DataType_
    , _sdsS3URI = pS3URI_
    }


-- | If you want Amazon SageMaker to replicate the entire dataset on each ML compute instance that is launched for model training, specify @FullyReplicated@ .  If you want Amazon SageMaker to replicate a subset of data on each ML compute instance that is launched for model training, specify @ShardedByS3Key@ . If there are /n/ ML compute instances launched for a training job, each instance gets approximately 1//n/ of the number of S3 objects. In this case, model training on each machine uses only the subset of training data.  Don't choose more ML compute instances for training than available S3 objects. If you do, some nodes won't get any data and you will pay for nodes that aren't getting any training data. This applies in both File and Pipe modes. Keep this in mind when developing algorithms.  In distributed training, where you use multiple ML compute EC2 instances, you might choose @ShardedByS3Key@ . If the algorithm requires copying training data to the ML storage volume (when @TrainingInputMode@ is set to @File@ ), this copies 1//n/ of the number of objects.
sdsS3DataDistributionType :: Lens' S3DataSource (Maybe S3DataDistribution)
sdsS3DataDistributionType = lens _sdsS3DataDistributionType (\ s a -> s{_sdsS3DataDistributionType = a})

-- | A list of one or more attribute names to use that are found in a specified augmented manifest file.
sdsAttributeNames :: Lens' S3DataSource [Text]
sdsAttributeNames = lens _sdsAttributeNames (\ s a -> s{_sdsAttributeNames = a}) . _Default . _Coerce

-- | If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects that match the specified key name prefix for model training.  If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for model training.  If you choose @AugmentedManifestFile@ , S3Uri identifies an object that is an augmented manifest file in JSON lines format. This file contains the data you want to use for model training. @AugmentedManifestFile@ can only be used if the Channel's input mode is @Pipe@ .
sdsS3DataType :: Lens' S3DataSource S3DataType
sdsS3DataType = lens _sdsS3DataType (\ s a -> s{_sdsS3DataType = a})

-- | Depending on the value specified for the @S3DataType@ , identifies either a key name prefix or a manifest. For example:      * A key name prefix might look like this: @s3://bucketname/exampleprefix@ .      * A manifest might look like this: @s3://bucketname/example.manifest@  The manifest is an S3 object which is a JSON file with the following format:  @[@  @{"prefix": "s3://customer_bucket/some/prefix/"},@  @"relative/path/to/custdata-1",@  @"relative/path/custdata-2",@  @...@  @]@  The preceding JSON matches the following @s3Uris@ :  @s3://customer_bucket/some/prefix/relative/path/to/custdata-1@  @s3://customer_bucket/some/prefix/relative/path/custdata-2@  @...@  The complete set of @s3uris@ in this manifest is the input data for the channel for this datasource. The object that each @s3uris@ points to must be readable by the IAM role that Amazon SageMaker uses to perform tasks on your behalf.
sdsS3URI :: Lens' S3DataSource Text
sdsS3URI = lens _sdsS3URI (\ s a -> s{_sdsS3URI = a})

instance FromJSON S3DataSource where
        parseJSON
          = withObject "S3DataSource"
              (\ x ->
                 S3DataSource' <$>
                   (x .:? "S3DataDistributionType") <*>
                     (x .:? "AttributeNames" .!= mempty)
                     <*> (x .: "S3DataType")
                     <*> (x .: "S3Uri"))

instance Hashable S3DataSource where

instance NFData S3DataSource where

instance ToJSON S3DataSource where
        toJSON S3DataSource'{..}
          = object
              (catMaybes
                 [("S3DataDistributionType" .=) <$>
                    _sdsS3DataDistributionType,
                  ("AttributeNames" .=) <$> _sdsAttributeNames,
                  Just ("S3DataType" .= _sdsS3DataType),
                  Just ("S3Uri" .= _sdsS3URI)])

-- | A multi-expression that searches for the specified resource or resources in a search. All resource objects that satisfy the expression's condition are included in the search results. You must specify at least one subexpression, filter, or nested filter. A @SearchExpression@ can contain up to twenty elements.
--
--
-- A @SearchExpression@ contains the following components:
--
--     * A list of @Filter@ objects. Each filter defines a simple Boolean expression comprised of a resource property name, Boolean operator, and value.
--
--     * A list of @NestedFilter@ objects. Each nested filter defines a list of Boolean expressions using a list of resource properties. A nested filter is satisfied if a single object in the list satisfies all Boolean expressions.
--
--     * A list of @SearchExpression@ objects. A search expression object can be nested in a list of search expression objects.
--
--     * A Boolean operator: @And@ or @Or@ .
--
--
--
--
-- /See:/ 'searchExpression' smart constructor.
data SearchExpression = SearchExpression'
  { _seSubExpressions :: !(Maybe (List1 SearchExpression))
  , _seOperator       :: !(Maybe BooleanOperator)
  , _seFilters        :: !(Maybe (List1 Filter))
  , _seNestedFilters  :: !(Maybe (List1 NestedFilters))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchExpression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seSubExpressions' - A list of search expression objects.
--
-- * 'seOperator' - A Boolean operator used to evaluate the search expression. If you want every conditional statement in all lists to be satisfied for the entire search expression to be true, specify @And@ . If only a single conditional statement needs to be true for the entire search expression to be true, specify @Or@ . The default value is @And@ .
--
-- * 'seFilters' - A list of filter objects.
--
-- * 'seNestedFilters' - A list of nested filter objects.
searchExpression
    :: SearchExpression
searchExpression =
  SearchExpression'
    { _seSubExpressions = Nothing
    , _seOperator = Nothing
    , _seFilters = Nothing
    , _seNestedFilters = Nothing
    }


-- | A list of search expression objects.
seSubExpressions :: Lens' SearchExpression (Maybe (NonEmpty SearchExpression))
seSubExpressions = lens _seSubExpressions (\ s a -> s{_seSubExpressions = a}) . mapping _List1

-- | A Boolean operator used to evaluate the search expression. If you want every conditional statement in all lists to be satisfied for the entire search expression to be true, specify @And@ . If only a single conditional statement needs to be true for the entire search expression to be true, specify @Or@ . The default value is @And@ .
seOperator :: Lens' SearchExpression (Maybe BooleanOperator)
seOperator = lens _seOperator (\ s a -> s{_seOperator = a})

-- | A list of filter objects.
seFilters :: Lens' SearchExpression (Maybe (NonEmpty Filter))
seFilters = lens _seFilters (\ s a -> s{_seFilters = a}) . mapping _List1

-- | A list of nested filter objects.
seNestedFilters :: Lens' SearchExpression (Maybe (NonEmpty NestedFilters))
seNestedFilters = lens _seNestedFilters (\ s a -> s{_seNestedFilters = a}) . mapping _List1

instance Hashable SearchExpression where

instance NFData SearchExpression where

instance ToJSON SearchExpression where
        toJSON SearchExpression'{..}
          = object
              (catMaybes
                 [("SubExpressions" .=) <$> _seSubExpressions,
                  ("Operator" .=) <$> _seOperator,
                  ("Filters" .=) <$> _seFilters,
                  ("NestedFilters" .=) <$> _seNestedFilters])

-- | An individual search result record that contains a single resource object.
--
--
--
-- /See:/ 'searchRecord' smart constructor.
newtype SearchRecord = SearchRecord'
  { _srTrainingJob :: Maybe TrainingJob
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srTrainingJob' - A @TrainingJob@ object that is returned as part of a @Search@ request.
searchRecord
    :: SearchRecord
searchRecord = SearchRecord' {_srTrainingJob = Nothing}


-- | A @TrainingJob@ object that is returned as part of a @Search@ request.
srTrainingJob :: Lens' SearchRecord (Maybe TrainingJob)
srTrainingJob = lens _srTrainingJob (\ s a -> s{_srTrainingJob = a})

instance FromJSON SearchRecord where
        parseJSON
          = withObject "SearchRecord"
              (\ x -> SearchRecord' <$> (x .:? "TrainingJob"))

instance Hashable SearchRecord where

instance NFData SearchRecord where

-- | An array element of 'DescribeTrainingJobResponse$SecondaryStatusTransitions' . It provides additional details about a status that the training job has transitioned through. A training job can be in one of several states, for example, starting, downloading, training, or uploading. Within each state, there are a number of intermediate states. For example, within the starting state, Amazon SageMaker could be starting the training job or launching the ML instances. These transitional states are referred to as the job's secondary status.
--
--
--
--
--
-- /See:/ 'secondaryStatusTransition' smart constructor.
data SecondaryStatusTransition = SecondaryStatusTransition'
  { _sstStatusMessage :: !(Maybe Text)
  , _sstEndTime       :: !(Maybe POSIX)
  , _sstStatus        :: !SecondaryStatus
  , _sstStartTime     :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SecondaryStatusTransition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sstStatusMessage' - A detailed description of the progress within a secondary status.  Amazon SageMaker provides secondary statuses and status messages that apply to each of them:     * Starting    *     * Starting the training job.     * Launching requested ML instances.     * Insufficient capacity error from EC2 while launching instances, retrying!     * Launched instance was unhealthy, replacing it!     * Preparing the instances for training.     * Training    *     * Downloading the training image.     * Training image download completed. Training in progress. /Important:/ Status messages are subject to change. Therefore, we recommend not including them in code that programmatically initiates actions. For examples, don't use status messages in if statements. To have an overview of your training job's progress, view @TrainingJobStatus@ and @SecondaryStatus@ in 'DescribeTrainingJobResponse' , and @StatusMessage@ together. For example, at the start of a training job, you might see the following:     * @TrainingJobStatus@ - InProgress     * @SecondaryStatus@ - Training     * @StatusMessage@ - Downloading the training image
--
-- * 'sstEndTime' - A timestamp that shows when the training job transitioned out of this secondary status state into another secondary status state or when the training job has ended.
--
-- * 'sstStatus' - Contains a secondary status information from a training job. Status might be one of the following secondary statuses:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@
--
-- * 'sstStartTime' - A timestamp that shows when the training job transitioned to the current secondary status state.
secondaryStatusTransition
    :: SecondaryStatus -- ^ 'sstStatus'
    -> UTCTime -- ^ 'sstStartTime'
    -> SecondaryStatusTransition
secondaryStatusTransition pStatus_ pStartTime_ =
  SecondaryStatusTransition'
    { _sstStatusMessage = Nothing
    , _sstEndTime = Nothing
    , _sstStatus = pStatus_
    , _sstStartTime = _Time # pStartTime_
    }


-- | A detailed description of the progress within a secondary status.  Amazon SageMaker provides secondary statuses and status messages that apply to each of them:     * Starting    *     * Starting the training job.     * Launching requested ML instances.     * Insufficient capacity error from EC2 while launching instances, retrying!     * Launched instance was unhealthy, replacing it!     * Preparing the instances for training.     * Training    *     * Downloading the training image.     * Training image download completed. Training in progress. /Important:/ Status messages are subject to change. Therefore, we recommend not including them in code that programmatically initiates actions. For examples, don't use status messages in if statements. To have an overview of your training job's progress, view @TrainingJobStatus@ and @SecondaryStatus@ in 'DescribeTrainingJobResponse' , and @StatusMessage@ together. For example, at the start of a training job, you might see the following:     * @TrainingJobStatus@ - InProgress     * @SecondaryStatus@ - Training     * @StatusMessage@ - Downloading the training image
sstStatusMessage :: Lens' SecondaryStatusTransition (Maybe Text)
sstStatusMessage = lens _sstStatusMessage (\ s a -> s{_sstStatusMessage = a})

-- | A timestamp that shows when the training job transitioned out of this secondary status state into another secondary status state or when the training job has ended.
sstEndTime :: Lens' SecondaryStatusTransition (Maybe UTCTime)
sstEndTime = lens _sstEndTime (\ s a -> s{_sstEndTime = a}) . mapping _Time

-- | Contains a secondary status information from a training job. Status might be one of the following secondary statuses:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@
sstStatus :: Lens' SecondaryStatusTransition SecondaryStatus
sstStatus = lens _sstStatus (\ s a -> s{_sstStatus = a})

-- | A timestamp that shows when the training job transitioned to the current secondary status state.
sstStartTime :: Lens' SecondaryStatusTransition UTCTime
sstStartTime = lens _sstStartTime (\ s a -> s{_sstStartTime = a}) . _Time

instance FromJSON SecondaryStatusTransition where
        parseJSON
          = withObject "SecondaryStatusTransition"
              (\ x ->
                 SecondaryStatusTransition' <$>
                   (x .:? "StatusMessage") <*> (x .:? "EndTime") <*>
                     (x .: "Status")
                     <*> (x .: "StartTime"))

instance Hashable SecondaryStatusTransition where

instance NFData SecondaryStatusTransition where

-- | A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , the results of the S3 key prefix matches are shuffled. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value.
--
--
-- For Pipe input mode, shuffling is done at the start of every epoch. With large datasets, this ensures that the order of the training data is different for each epoch, and it helps reduce bias and possible overfitting. In a multi-node training job when @ShuffleConfig@ is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
--
--
-- /See:/ 'shuffleConfig' smart constructor.
newtype ShuffleConfig = ShuffleConfig'
  { _scSeed :: Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ShuffleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scSeed' - Determines the shuffling order in @ShuffleConfig@ value.
shuffleConfig
    :: Integer -- ^ 'scSeed'
    -> ShuffleConfig
shuffleConfig pSeed_ = ShuffleConfig' {_scSeed = pSeed_}


-- | Determines the shuffling order in @ShuffleConfig@ value.
scSeed :: Lens' ShuffleConfig Integer
scSeed = lens _scSeed (\ s a -> s{_scSeed = a})

instance FromJSON ShuffleConfig where
        parseJSON
          = withObject "ShuffleConfig"
              (\ x -> ShuffleConfig' <$> (x .: "Seed"))

instance Hashable ShuffleConfig where

instance NFData ShuffleConfig where

instance ToJSON ShuffleConfig where
        toJSON ShuffleConfig'{..}
          = object (catMaybes [Just ("Seed" .= _scSeed)])

-- | Specifies an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
--
--
--
-- /See:/ 'sourceAlgorithm' smart constructor.
data SourceAlgorithm = SourceAlgorithm'
  { _saModelDataURL  :: !(Maybe Text)
  , _saAlgorithmName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SourceAlgorithm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saModelDataURL' - The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
--
-- * 'saAlgorithmName' - The name of an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
sourceAlgorithm
    :: Text -- ^ 'saAlgorithmName'
    -> SourceAlgorithm
sourceAlgorithm pAlgorithmName_ =
  SourceAlgorithm'
    {_saModelDataURL = Nothing, _saAlgorithmName = pAlgorithmName_}


-- | The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
saModelDataURL :: Lens' SourceAlgorithm (Maybe Text)
saModelDataURL = lens _saModelDataURL (\ s a -> s{_saModelDataURL = a})

-- | The name of an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
saAlgorithmName :: Lens' SourceAlgorithm Text
saAlgorithmName = lens _saAlgorithmName (\ s a -> s{_saAlgorithmName = a})

instance FromJSON SourceAlgorithm where
        parseJSON
          = withObject "SourceAlgorithm"
              (\ x ->
                 SourceAlgorithm' <$>
                   (x .:? "ModelDataUrl") <*> (x .: "AlgorithmName"))

instance Hashable SourceAlgorithm where

instance NFData SourceAlgorithm where

instance ToJSON SourceAlgorithm where
        toJSON SourceAlgorithm'{..}
          = object
              (catMaybes
                 [("ModelDataUrl" .=) <$> _saModelDataURL,
                  Just ("AlgorithmName" .= _saAlgorithmName)])

-- | A list of algorithms that were used to create a model package.
--
--
--
-- /See:/ 'sourceAlgorithmSpecification' smart constructor.
newtype SourceAlgorithmSpecification = SourceAlgorithmSpecification'
  { _sasSourceAlgorithms :: List1 SourceAlgorithm
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SourceAlgorithmSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sasSourceAlgorithms' - A list of the algorithms that were used to create a model package.
sourceAlgorithmSpecification
    :: NonEmpty SourceAlgorithm -- ^ 'sasSourceAlgorithms'
    -> SourceAlgorithmSpecification
sourceAlgorithmSpecification pSourceAlgorithms_ =
  SourceAlgorithmSpecification'
    {_sasSourceAlgorithms = _List1 # pSourceAlgorithms_}


-- | A list of the algorithms that were used to create a model package.
sasSourceAlgorithms :: Lens' SourceAlgorithmSpecification (NonEmpty SourceAlgorithm)
sasSourceAlgorithms = lens _sasSourceAlgorithms (\ s a -> s{_sasSourceAlgorithms = a}) . _List1

instance FromJSON SourceAlgorithmSpecification where
        parseJSON
          = withObject "SourceAlgorithmSpecification"
              (\ x ->
                 SourceAlgorithmSpecification' <$>
                   (x .: "SourceAlgorithms"))

instance Hashable SourceAlgorithmSpecification where

instance NFData SourceAlgorithmSpecification where

instance ToJSON SourceAlgorithmSpecification where
        toJSON SourceAlgorithmSpecification'{..}
          = object
              (catMaybes
                 [Just ("SourceAlgorithms" .= _sasSourceAlgorithms)])

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
-- * 'scMaxRuntimeInSeconds' - The maximum length of time, in seconds, that the training job can run. If model training does not complete during this time, Amazon SageMaker ends the job. If value is not specified, default value is 1 day. Maximum value is 28 days.
stoppingCondition
    :: StoppingCondition
stoppingCondition = StoppingCondition' {_scMaxRuntimeInSeconds = Nothing}


-- | The maximum length of time, in seconds, that the training job can run. If model training does not complete during this time, Amazon SageMaker ends the job. If value is not specified, default value is 1 day. Maximum value is 28 days.
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

-- | Describes a work team of a vendor that does the a labelling job.
--
--
--
-- /See:/ 'subscribedWorkteam' smart constructor.
data SubscribedWorkteam = SubscribedWorkteam'
  { _swMarketplaceTitle       :: !(Maybe Text)
  , _swSellerName             :: !(Maybe Text)
  , _swListingId              :: !(Maybe Text)
  , _swMarketplaceDescription :: !(Maybe Text)
  , _swWorkteamARN            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscribedWorkteam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'swMarketplaceTitle' - The title of the service provided by the vendor in the Amazon Marketplace.
--
-- * 'swSellerName' - The name of the vendor in the Amazon Marketplace.
--
-- * 'swListingId' -
--
-- * 'swMarketplaceDescription' - The description of the vendor from the Amazon Marketplace.
--
-- * 'swWorkteamARN' - The Amazon Resource Name (ARN) of the vendor that you have subscribed.
subscribedWorkteam
    :: Text -- ^ 'swWorkteamARN'
    -> SubscribedWorkteam
subscribedWorkteam pWorkteamARN_ =
  SubscribedWorkteam'
    { _swMarketplaceTitle = Nothing
    , _swSellerName = Nothing
    , _swListingId = Nothing
    , _swMarketplaceDescription = Nothing
    , _swWorkteamARN = pWorkteamARN_
    }


-- | The title of the service provided by the vendor in the Amazon Marketplace.
swMarketplaceTitle :: Lens' SubscribedWorkteam (Maybe Text)
swMarketplaceTitle = lens _swMarketplaceTitle (\ s a -> s{_swMarketplaceTitle = a})

-- | The name of the vendor in the Amazon Marketplace.
swSellerName :: Lens' SubscribedWorkteam (Maybe Text)
swSellerName = lens _swSellerName (\ s a -> s{_swSellerName = a})

-- |
swListingId :: Lens' SubscribedWorkteam (Maybe Text)
swListingId = lens _swListingId (\ s a -> s{_swListingId = a})

-- | The description of the vendor from the Amazon Marketplace.
swMarketplaceDescription :: Lens' SubscribedWorkteam (Maybe Text)
swMarketplaceDescription = lens _swMarketplaceDescription (\ s a -> s{_swMarketplaceDescription = a})

-- | The Amazon Resource Name (ARN) of the vendor that you have subscribed.
swWorkteamARN :: Lens' SubscribedWorkteam Text
swWorkteamARN = lens _swWorkteamARN (\ s a -> s{_swWorkteamARN = a})

instance FromJSON SubscribedWorkteam where
        parseJSON
          = withObject "SubscribedWorkteam"
              (\ x ->
                 SubscribedWorkteam' <$>
                   (x .:? "MarketplaceTitle") <*> (x .:? "SellerName")
                     <*> (x .:? "ListingId")
                     <*> (x .:? "MarketplaceDescription")
                     <*> (x .: "WorkteamArn"))

instance Hashable SubscribedWorkteam where

instance NFData SubscribedWorkteam where

-- | Limits the property names that are included in the response.
--
--
--
-- /See:/ 'suggestionQuery' smart constructor.
newtype SuggestionQuery = SuggestionQuery'
  { _sqPropertyNameQuery :: Maybe PropertyNameQuery
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SuggestionQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sqPropertyNameQuery' - A type of @SuggestionQuery@ . Defines a property name hint. Only property names that match the specified hint are included in the response.
suggestionQuery
    :: SuggestionQuery
suggestionQuery = SuggestionQuery' {_sqPropertyNameQuery = Nothing}


-- | A type of @SuggestionQuery@ . Defines a property name hint. Only property names that match the specified hint are included in the response.
sqPropertyNameQuery :: Lens' SuggestionQuery (Maybe PropertyNameQuery)
sqPropertyNameQuery = lens _sqPropertyNameQuery (\ s a -> s{_sqPropertyNameQuery = a})

instance Hashable SuggestionQuery where

instance NFData SuggestionQuery where

instance ToJSON SuggestionQuery where
        toJSON SuggestionQuery'{..}
          = object
              (catMaybes
                 [("PropertyNameQuery" .=) <$> _sqPropertyNameQuery])

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

-- | Contains information about a training job.
--
--
--
-- /See:/ 'trainingJob' smart constructor.
data TrainingJob = TrainingJob'
  { _tjCreationTime :: !(Maybe POSIX)
  , _tjLabelingJobARN :: !(Maybe Text)
  , _tjFailureReason :: !(Maybe Text)
  , _tjSecondaryStatusTransitions :: !(Maybe [SecondaryStatusTransition])
  , _tjModelArtifacts :: !(Maybe ModelArtifacts)
  , _tjTrainingEndTime :: !(Maybe POSIX)
  , _tjStoppingCondition :: !(Maybe StoppingCondition)
  , _tjTrainingJobStatus :: !(Maybe TrainingJobStatus)
  , _tjEnableNetworkIsolation :: !(Maybe Bool)
  , _tjLastModifiedTime :: !(Maybe POSIX)
  , _tjHyperParameters :: !(Maybe (Map Text Text))
  , _tjInputDataConfig :: !(Maybe (List1 Channel))
  , _tjVPCConfig :: !(Maybe VPCConfig)
  , _tjTrainingJobARN :: !(Maybe Text)
  , _tjAlgorithmSpecification :: !(Maybe AlgorithmSpecification)
  , _tjFinalMetricDataList :: !(Maybe [MetricData])
  , _tjOutputDataConfig :: !(Maybe OutputDataConfig)
  , _tjTrainingStartTime :: !(Maybe POSIX)
  , _tjTuningJobARN :: !(Maybe Text)
  , _tjTrainingJobName :: !(Maybe Text)
  , _tjResourceConfig :: !(Maybe ResourceConfig)
  , _tjEnableInterContainerTrafficEncryption :: !(Maybe Bool)
  , _tjSecondaryStatus :: !(Maybe SecondaryStatus)
  , _tjTags :: !(Maybe [Tag])
  , _tjRoleARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrainingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjCreationTime' - A timestamp that indicates when the training job was created.
--
-- * 'tjLabelingJobARN' - The Amazon Resource Name (ARN) of the labeling job.
--
-- * 'tjFailureReason' - If the training job failed, the reason it failed.
--
-- * 'tjSecondaryStatusTransitions' - A history of all of the secondary statuses that the training job has transitioned through.
--
-- * 'tjModelArtifacts' - Information about the Amazon S3 location that is configured for storing model artifacts.
--
-- * 'tjTrainingEndTime' - Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
--
-- * 'tjStoppingCondition' - The condition under which to stop the training job.
--
-- * 'tjTrainingJobStatus' - The status of the training job. Training job statuses are:     * @InProgress@ - The training is in progress.     * @Completed@ - The training job has completed.     * @Failed@ - The training job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTrainingJobResponse@ call.     * @Stopping@ - The training job is stopping.     * @Stopped@ - The training job has stopped. For more detailed information, see @SecondaryStatus@ .
--
-- * 'tjEnableNetworkIsolation' - If the @TrainingJob@ was created with network isolation, the value is set to @true@ . If network isolation is enabled, nodes can't communicate beyond the VPC they run in.
--
-- * 'tjLastModifiedTime' - A timestamp that indicates when the status of the training job was last modified.
--
-- * 'tjHyperParameters' - Algorithm-specific parameters.
--
-- * 'tjInputDataConfig' - An array of @Channel@ objects that describes each data input channel.
--
-- * 'tjVPCConfig' - A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- * 'tjTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
--
-- * 'tjAlgorithmSpecification' - Information about the algorithm used for training, and algorithm metadata.
--
-- * 'tjFinalMetricDataList' - A list of final metric values that are set when the training job completes. Used only if the training job was configured to use metrics.
--
-- * 'tjOutputDataConfig' - The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
--
-- * 'tjTrainingStartTime' - Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
--
-- * 'tjTuningJobARN' - The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
--
-- * 'tjTrainingJobName' - The name of the training job.
--
-- * 'tjResourceConfig' - Resources, including ML compute instances and ML storage volumes, that are configured for model training.
--
-- * 'tjEnableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
--
-- * 'tjSecondaryStatus' - Provides detailed information about the state of the training job. For detailed information about the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' . Amazon SageMaker provides primary statuses and secondary statuses that apply to each of them:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. /Important:/ Valid values for @SecondaryStatus@ are subject to change.  We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@
--
-- * 'tjTags' - An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'tjRoleARN' - The AWS Identity and Access Management (IAM) role configured for the training job.
trainingJob
    :: TrainingJob
trainingJob =
  TrainingJob'
    { _tjCreationTime = Nothing
    , _tjLabelingJobARN = Nothing
    , _tjFailureReason = Nothing
    , _tjSecondaryStatusTransitions = Nothing
    , _tjModelArtifacts = Nothing
    , _tjTrainingEndTime = Nothing
    , _tjStoppingCondition = Nothing
    , _tjTrainingJobStatus = Nothing
    , _tjEnableNetworkIsolation = Nothing
    , _tjLastModifiedTime = Nothing
    , _tjHyperParameters = Nothing
    , _tjInputDataConfig = Nothing
    , _tjVPCConfig = Nothing
    , _tjTrainingJobARN = Nothing
    , _tjAlgorithmSpecification = Nothing
    , _tjFinalMetricDataList = Nothing
    , _tjOutputDataConfig = Nothing
    , _tjTrainingStartTime = Nothing
    , _tjTuningJobARN = Nothing
    , _tjTrainingJobName = Nothing
    , _tjResourceConfig = Nothing
    , _tjEnableInterContainerTrafficEncryption = Nothing
    , _tjSecondaryStatus = Nothing
    , _tjTags = Nothing
    , _tjRoleARN = Nothing
    }


-- | A timestamp that indicates when the training job was created.
tjCreationTime :: Lens' TrainingJob (Maybe UTCTime)
tjCreationTime = lens _tjCreationTime (\ s a -> s{_tjCreationTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the labeling job.
tjLabelingJobARN :: Lens' TrainingJob (Maybe Text)
tjLabelingJobARN = lens _tjLabelingJobARN (\ s a -> s{_tjLabelingJobARN = a})

-- | If the training job failed, the reason it failed.
tjFailureReason :: Lens' TrainingJob (Maybe Text)
tjFailureReason = lens _tjFailureReason (\ s a -> s{_tjFailureReason = a})

-- | A history of all of the secondary statuses that the training job has transitioned through.
tjSecondaryStatusTransitions :: Lens' TrainingJob [SecondaryStatusTransition]
tjSecondaryStatusTransitions = lens _tjSecondaryStatusTransitions (\ s a -> s{_tjSecondaryStatusTransitions = a}) . _Default . _Coerce

-- | Information about the Amazon S3 location that is configured for storing model artifacts.
tjModelArtifacts :: Lens' TrainingJob (Maybe ModelArtifacts)
tjModelArtifacts = lens _tjModelArtifacts (\ s a -> s{_tjModelArtifacts = a})

-- | Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
tjTrainingEndTime :: Lens' TrainingJob (Maybe UTCTime)
tjTrainingEndTime = lens _tjTrainingEndTime (\ s a -> s{_tjTrainingEndTime = a}) . mapping _Time

-- | The condition under which to stop the training job.
tjStoppingCondition :: Lens' TrainingJob (Maybe StoppingCondition)
tjStoppingCondition = lens _tjStoppingCondition (\ s a -> s{_tjStoppingCondition = a})

-- | The status of the training job. Training job statuses are:     * @InProgress@ - The training is in progress.     * @Completed@ - The training job has completed.     * @Failed@ - The training job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTrainingJobResponse@ call.     * @Stopping@ - The training job is stopping.     * @Stopped@ - The training job has stopped. For more detailed information, see @SecondaryStatus@ .
tjTrainingJobStatus :: Lens' TrainingJob (Maybe TrainingJobStatus)
tjTrainingJobStatus = lens _tjTrainingJobStatus (\ s a -> s{_tjTrainingJobStatus = a})

-- | If the @TrainingJob@ was created with network isolation, the value is set to @true@ . If network isolation is enabled, nodes can't communicate beyond the VPC they run in.
tjEnableNetworkIsolation :: Lens' TrainingJob (Maybe Bool)
tjEnableNetworkIsolation = lens _tjEnableNetworkIsolation (\ s a -> s{_tjEnableNetworkIsolation = a})

-- | A timestamp that indicates when the status of the training job was last modified.
tjLastModifiedTime :: Lens' TrainingJob (Maybe UTCTime)
tjLastModifiedTime = lens _tjLastModifiedTime (\ s a -> s{_tjLastModifiedTime = a}) . mapping _Time

-- | Algorithm-specific parameters.
tjHyperParameters :: Lens' TrainingJob (HashMap Text Text)
tjHyperParameters = lens _tjHyperParameters (\ s a -> s{_tjHyperParameters = a}) . _Default . _Map

-- | An array of @Channel@ objects that describes each data input channel.
tjInputDataConfig :: Lens' TrainingJob (Maybe (NonEmpty Channel))
tjInputDataConfig = lens _tjInputDataConfig (\ s a -> s{_tjInputDataConfig = a}) . mapping _List1

-- | A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
tjVPCConfig :: Lens' TrainingJob (Maybe VPCConfig)
tjVPCConfig = lens _tjVPCConfig (\ s a -> s{_tjVPCConfig = a})

-- | The Amazon Resource Name (ARN) of the training job.
tjTrainingJobARN :: Lens' TrainingJob (Maybe Text)
tjTrainingJobARN = lens _tjTrainingJobARN (\ s a -> s{_tjTrainingJobARN = a})

-- | Information about the algorithm used for training, and algorithm metadata.
tjAlgorithmSpecification :: Lens' TrainingJob (Maybe AlgorithmSpecification)
tjAlgorithmSpecification = lens _tjAlgorithmSpecification (\ s a -> s{_tjAlgorithmSpecification = a})

-- | A list of final metric values that are set when the training job completes. Used only if the training job was configured to use metrics.
tjFinalMetricDataList :: Lens' TrainingJob [MetricData]
tjFinalMetricDataList = lens _tjFinalMetricDataList (\ s a -> s{_tjFinalMetricDataList = a}) . _Default . _Coerce

-- | The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
tjOutputDataConfig :: Lens' TrainingJob (Maybe OutputDataConfig)
tjOutputDataConfig = lens _tjOutputDataConfig (\ s a -> s{_tjOutputDataConfig = a})

-- | Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
tjTrainingStartTime :: Lens' TrainingJob (Maybe UTCTime)
tjTrainingStartTime = lens _tjTrainingStartTime (\ s a -> s{_tjTrainingStartTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
tjTuningJobARN :: Lens' TrainingJob (Maybe Text)
tjTuningJobARN = lens _tjTuningJobARN (\ s a -> s{_tjTuningJobARN = a})

-- | The name of the training job.
tjTrainingJobName :: Lens' TrainingJob (Maybe Text)
tjTrainingJobName = lens _tjTrainingJobName (\ s a -> s{_tjTrainingJobName = a})

-- | Resources, including ML compute instances and ML storage volumes, that are configured for model training.
tjResourceConfig :: Lens' TrainingJob (Maybe ResourceConfig)
tjResourceConfig = lens _tjResourceConfig (\ s a -> s{_tjResourceConfig = a})

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
tjEnableInterContainerTrafficEncryption :: Lens' TrainingJob (Maybe Bool)
tjEnableInterContainerTrafficEncryption = lens _tjEnableInterContainerTrafficEncryption (\ s a -> s{_tjEnableInterContainerTrafficEncryption = a})

-- | Provides detailed information about the state of the training job. For detailed information about the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' . Amazon SageMaker provides primary statuses and secondary statuses that apply to each of them:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. /Important:/ Valid values for @SecondaryStatus@ are subject to change.  We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@
tjSecondaryStatus :: Lens' TrainingJob (Maybe SecondaryStatus)
tjSecondaryStatus = lens _tjSecondaryStatus (\ s a -> s{_tjSecondaryStatus = a})

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
tjTags :: Lens' TrainingJob [Tag]
tjTags = lens _tjTags (\ s a -> s{_tjTags = a}) . _Default . _Coerce

-- | The AWS Identity and Access Management (IAM) role configured for the training job.
tjRoleARN :: Lens' TrainingJob (Maybe Text)
tjRoleARN = lens _tjRoleARN (\ s a -> s{_tjRoleARN = a})

instance FromJSON TrainingJob where
        parseJSON
          = withObject "TrainingJob"
              (\ x ->
                 TrainingJob' <$>
                   (x .:? "CreationTime") <*> (x .:? "LabelingJobArn")
                     <*> (x .:? "FailureReason")
                     <*> (x .:? "SecondaryStatusTransitions" .!= mempty)
                     <*> (x .:? "ModelArtifacts")
                     <*> (x .:? "TrainingEndTime")
                     <*> (x .:? "StoppingCondition")
                     <*> (x .:? "TrainingJobStatus")
                     <*> (x .:? "EnableNetworkIsolation")
                     <*> (x .:? "LastModifiedTime")
                     <*> (x .:? "HyperParameters" .!= mempty)
                     <*> (x .:? "InputDataConfig")
                     <*> (x .:? "VpcConfig")
                     <*> (x .:? "TrainingJobArn")
                     <*> (x .:? "AlgorithmSpecification")
                     <*> (x .:? "FinalMetricDataList" .!= mempty)
                     <*> (x .:? "OutputDataConfig")
                     <*> (x .:? "TrainingStartTime")
                     <*> (x .:? "TuningJobArn")
                     <*> (x .:? "TrainingJobName")
                     <*> (x .:? "ResourceConfig")
                     <*> (x .:? "EnableInterContainerTrafficEncryption")
                     <*> (x .:? "SecondaryStatus")
                     <*> (x .:? "Tags" .!= mempty)
                     <*> (x .:? "RoleArn"))

instance Hashable TrainingJob where

instance NFData TrainingJob where

-- | Defines the input needed to run a training job using the algorithm.
--
--
--
-- /See:/ 'trainingJobDefinition' smart constructor.
data TrainingJobDefinition = TrainingJobDefinition'
  { _tjdHyperParameters   :: !(Maybe (Map Text Text))
  , _tjdTrainingInputMode :: !TrainingInputMode
  , _tjdInputDataConfig   :: !(List1 Channel)
  , _tjdOutputDataConfig  :: !OutputDataConfig
  , _tjdResourceConfig    :: !ResourceConfig
  , _tjdStoppingCondition :: !StoppingCondition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrainingJobDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjdHyperParameters' - The hyperparameters used for the training job.
--
-- * 'tjdTrainingInputMode' - The input mode used by the algorithm for the training job. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.
--
-- * 'tjdInputDataConfig' - An array of @Channel@ objects, each of which specifies an input source.
--
-- * 'tjdOutputDataConfig' - the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
--
-- * 'tjdResourceConfig' - The resources, including the ML compute instances and ML storage volumes, to use for model training.
--
-- * 'tjdStoppingCondition' - Sets a duration for training. Use this parameter to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal, which delays job termination for 120 seconds. Algorithms might use this 120-second window to save the model artifacts.
trainingJobDefinition
    :: TrainingInputMode -- ^ 'tjdTrainingInputMode'
    -> NonEmpty Channel -- ^ 'tjdInputDataConfig'
    -> OutputDataConfig -- ^ 'tjdOutputDataConfig'
    -> ResourceConfig -- ^ 'tjdResourceConfig'
    -> StoppingCondition -- ^ 'tjdStoppingCondition'
    -> TrainingJobDefinition
trainingJobDefinition pTrainingInputMode_ pInputDataConfig_ pOutputDataConfig_ pResourceConfig_ pStoppingCondition_ =
  TrainingJobDefinition'
    { _tjdHyperParameters = Nothing
    , _tjdTrainingInputMode = pTrainingInputMode_
    , _tjdInputDataConfig = _List1 # pInputDataConfig_
    , _tjdOutputDataConfig = pOutputDataConfig_
    , _tjdResourceConfig = pResourceConfig_
    , _tjdStoppingCondition = pStoppingCondition_
    }


-- | The hyperparameters used for the training job.
tjdHyperParameters :: Lens' TrainingJobDefinition (HashMap Text Text)
tjdHyperParameters = lens _tjdHyperParameters (\ s a -> s{_tjdHyperParameters = a}) . _Default . _Map

-- | The input mode used by the algorithm for the training job. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.
tjdTrainingInputMode :: Lens' TrainingJobDefinition TrainingInputMode
tjdTrainingInputMode = lens _tjdTrainingInputMode (\ s a -> s{_tjdTrainingInputMode = a})

-- | An array of @Channel@ objects, each of which specifies an input source.
tjdInputDataConfig :: Lens' TrainingJobDefinition (NonEmpty Channel)
tjdInputDataConfig = lens _tjdInputDataConfig (\ s a -> s{_tjdInputDataConfig = a}) . _List1

-- | the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
tjdOutputDataConfig :: Lens' TrainingJobDefinition OutputDataConfig
tjdOutputDataConfig = lens _tjdOutputDataConfig (\ s a -> s{_tjdOutputDataConfig = a})

-- | The resources, including the ML compute instances and ML storage volumes, to use for model training.
tjdResourceConfig :: Lens' TrainingJobDefinition ResourceConfig
tjdResourceConfig = lens _tjdResourceConfig (\ s a -> s{_tjdResourceConfig = a})

-- | Sets a duration for training. Use this parameter to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal, which delays job termination for 120 seconds. Algorithms might use this 120-second window to save the model artifacts.
tjdStoppingCondition :: Lens' TrainingJobDefinition StoppingCondition
tjdStoppingCondition = lens _tjdStoppingCondition (\ s a -> s{_tjdStoppingCondition = a})

instance FromJSON TrainingJobDefinition where
        parseJSON
          = withObject "TrainingJobDefinition"
              (\ x ->
                 TrainingJobDefinition' <$>
                   (x .:? "HyperParameters" .!= mempty) <*>
                     (x .: "TrainingInputMode")
                     <*> (x .: "InputDataConfig")
                     <*> (x .: "OutputDataConfig")
                     <*> (x .: "ResourceConfig")
                     <*> (x .: "StoppingCondition"))

instance Hashable TrainingJobDefinition where

instance NFData TrainingJobDefinition where

instance ToJSON TrainingJobDefinition where
        toJSON TrainingJobDefinition'{..}
          = object
              (catMaybes
                 [("HyperParameters" .=) <$> _tjdHyperParameters,
                  Just ("TrainingInputMode" .= _tjdTrainingInputMode),
                  Just ("InputDataConfig" .= _tjdInputDataConfig),
                  Just ("OutputDataConfig" .= _tjdOutputDataConfig),
                  Just ("ResourceConfig" .= _tjdResourceConfig),
                  Just ("StoppingCondition" .= _tjdStoppingCondition)])

-- | The numbers of training jobs launched by a hyperparameter tuning job, categorized by status.
--
--
--
-- /See:/ 'trainingJobStatusCounters' smart constructor.
data TrainingJobStatusCounters = TrainingJobStatusCounters'
  { _tjscStopped           :: !(Maybe Nat)
  , _tjscRetryableError    :: !(Maybe Nat)
  , _tjscInProgress        :: !(Maybe Nat)
  , _tjscNonRetryableError :: !(Maybe Nat)
  , _tjscCompleted         :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrainingJobStatusCounters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjscStopped' - The number of training jobs launched by a hyperparameter tuning job that were manually stopped.
--
-- * 'tjscRetryableError' - The number of training jobs that failed, but can be retried. A failed training job can be retried only if it failed because an internal service error occurred.
--
-- * 'tjscInProgress' - The number of in-progress training jobs launched by a hyperparameter tuning job.
--
-- * 'tjscNonRetryableError' - The number of training jobs that failed and can't be retried. A failed training job can't be retried if it failed because a client error occurred.
--
-- * 'tjscCompleted' - The number of completed training jobs launched by the hyperparameter tuning job.
trainingJobStatusCounters
    :: TrainingJobStatusCounters
trainingJobStatusCounters =
  TrainingJobStatusCounters'
    { _tjscStopped = Nothing
    , _tjscRetryableError = Nothing
    , _tjscInProgress = Nothing
    , _tjscNonRetryableError = Nothing
    , _tjscCompleted = Nothing
    }


-- | The number of training jobs launched by a hyperparameter tuning job that were manually stopped.
tjscStopped :: Lens' TrainingJobStatusCounters (Maybe Natural)
tjscStopped = lens _tjscStopped (\ s a -> s{_tjscStopped = a}) . mapping _Nat

-- | The number of training jobs that failed, but can be retried. A failed training job can be retried only if it failed because an internal service error occurred.
tjscRetryableError :: Lens' TrainingJobStatusCounters (Maybe Natural)
tjscRetryableError = lens _tjscRetryableError (\ s a -> s{_tjscRetryableError = a}) . mapping _Nat

-- | The number of in-progress training jobs launched by a hyperparameter tuning job.
tjscInProgress :: Lens' TrainingJobStatusCounters (Maybe Natural)
tjscInProgress = lens _tjscInProgress (\ s a -> s{_tjscInProgress = a}) . mapping _Nat

-- | The number of training jobs that failed and can't be retried. A failed training job can't be retried if it failed because a client error occurred.
tjscNonRetryableError :: Lens' TrainingJobStatusCounters (Maybe Natural)
tjscNonRetryableError = lens _tjscNonRetryableError (\ s a -> s{_tjscNonRetryableError = a}) . mapping _Nat

-- | The number of completed training jobs launched by the hyperparameter tuning job.
tjscCompleted :: Lens' TrainingJobStatusCounters (Maybe Natural)
tjscCompleted = lens _tjscCompleted (\ s a -> s{_tjscCompleted = a}) . mapping _Nat

instance FromJSON TrainingJobStatusCounters where
        parseJSON
          = withObject "TrainingJobStatusCounters"
              (\ x ->
                 TrainingJobStatusCounters' <$>
                   (x .:? "Stopped") <*> (x .:? "RetryableError") <*>
                     (x .:? "InProgress")
                     <*> (x .:? "NonRetryableError")
                     <*> (x .:? "Completed"))

instance Hashable TrainingJobStatusCounters where

instance NFData TrainingJobStatusCounters where

-- | Provides summary information about a training job.
--
--
--
-- /See:/ 'trainingJobSummary' smart constructor.
data TrainingJobSummary = TrainingJobSummary'
  { _tTrainingEndTime   :: !(Maybe POSIX)
  , _tLastModifiedTime  :: !(Maybe POSIX)
  , _tTrainingJobName   :: !Text
  , _tTrainingJobARN    :: !Text
  , _tCreationTime      :: !POSIX
  , _tTrainingJobStatus :: !TrainingJobStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrainingJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTrainingEndTime' - A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
--
-- * 'tLastModifiedTime' - Timestamp when the training job was last modified.
--
-- * 'tTrainingJobName' - The name of the training job that you want a summary for.
--
-- * 'tTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
--
-- * 'tCreationTime' - A timestamp that shows when the training job was created.
--
-- * 'tTrainingJobStatus' - The status of the training job.
trainingJobSummary
    :: Text -- ^ 'tTrainingJobName'
    -> Text -- ^ 'tTrainingJobARN'
    -> UTCTime -- ^ 'tCreationTime'
    -> TrainingJobStatus -- ^ 'tTrainingJobStatus'
    -> TrainingJobSummary
trainingJobSummary pTrainingJobName_ pTrainingJobARN_ pCreationTime_ pTrainingJobStatus_ =
  TrainingJobSummary'
    { _tTrainingEndTime = Nothing
    , _tLastModifiedTime = Nothing
    , _tTrainingJobName = pTrainingJobName_
    , _tTrainingJobARN = pTrainingJobARN_
    , _tCreationTime = _Time # pCreationTime_
    , _tTrainingJobStatus = pTrainingJobStatus_
    }


-- | A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
tTrainingEndTime :: Lens' TrainingJobSummary (Maybe UTCTime)
tTrainingEndTime = lens _tTrainingEndTime (\ s a -> s{_tTrainingEndTime = a}) . mapping _Time

-- | Timestamp when the training job was last modified.
tLastModifiedTime :: Lens' TrainingJobSummary (Maybe UTCTime)
tLastModifiedTime = lens _tLastModifiedTime (\ s a -> s{_tLastModifiedTime = a}) . mapping _Time

-- | The name of the training job that you want a summary for.
tTrainingJobName :: Lens' TrainingJobSummary Text
tTrainingJobName = lens _tTrainingJobName (\ s a -> s{_tTrainingJobName = a})

-- | The Amazon Resource Name (ARN) of the training job.
tTrainingJobARN :: Lens' TrainingJobSummary Text
tTrainingJobARN = lens _tTrainingJobARN (\ s a -> s{_tTrainingJobARN = a})

-- | A timestamp that shows when the training job was created.
tCreationTime :: Lens' TrainingJobSummary UTCTime
tCreationTime = lens _tCreationTime (\ s a -> s{_tCreationTime = a}) . _Time

-- | The status of the training job.
tTrainingJobStatus :: Lens' TrainingJobSummary TrainingJobStatus
tTrainingJobStatus = lens _tTrainingJobStatus (\ s a -> s{_tTrainingJobStatus = a})

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

-- | Defines how the algorithm is used for a training job.
--
--
--
-- /See:/ 'trainingSpecification' smart constructor.
data TrainingSpecification = TrainingSpecification'
  { _tsTrainingImageDigest :: !(Maybe Text)
  , _tsSupportsDistributedTraining :: !(Maybe Bool)
  , _tsSupportedHyperParameters :: !(Maybe [HyperParameterSpecification])
  , _tsSupportedTuningJobObjectiveMetrics :: !(Maybe [HyperParameterTuningJobObjective])
  , _tsMetricDefinitions :: !(Maybe [MetricDefinition])
  , _tsTrainingImage :: !Text
  , _tsSupportedTrainingInstanceTypes :: ![TrainingInstanceType]
  , _tsTrainingChannels :: !(List1 ChannelSpecification)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrainingSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsTrainingImageDigest' - An MD5 hash of the training algorithm that identifies the Docker image used for training.
--
-- * 'tsSupportsDistributedTraining' - Indicates whether the algorithm supports distributed training. If set to false, buyers can
