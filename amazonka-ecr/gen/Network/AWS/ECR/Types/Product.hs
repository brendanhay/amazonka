{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.Product where

import Network.AWS.ECR.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing authorization data for an Amazon ECR registry.
--
--
--
-- /See:/ 'authorizationData' smart constructor.
data AuthorizationData = AuthorizationData'
  { _adExpiresAt          :: !(Maybe POSIX)
  , _adProxyEndpoint      :: !(Maybe Text)
  , _adAuthorizationToken :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthorizationData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adExpiresAt' - The Unix time in seconds and milliseconds when the authorization token expires. Authorization tokens are valid for 12 hours.
--
-- * 'adProxyEndpoint' - The registry URL to use for this authorization token in a @docker login@ command. The Amazon ECR registry URL format is @https://aws_account_id.dkr.ecr.region.amazonaws.com@ . For example, @https://012345678910.dkr.ecr.us-east-1.amazonaws.com@ ..
--
-- * 'adAuthorizationToken' - A base64-encoded string that contains authorization data for the specified Amazon ECR registry. When the string is decoded, it is presented in the format @user:password@ for private registry authentication using @docker login@ .
authorizationData
    :: AuthorizationData
authorizationData =
  AuthorizationData'
    { _adExpiresAt = Nothing
    , _adProxyEndpoint = Nothing
    , _adAuthorizationToken = Nothing
    }


-- | The Unix time in seconds and milliseconds when the authorization token expires. Authorization tokens are valid for 12 hours.
adExpiresAt :: Lens' AuthorizationData (Maybe UTCTime)
adExpiresAt = lens _adExpiresAt (\ s a -> s{_adExpiresAt = a}) . mapping _Time

-- | The registry URL to use for this authorization token in a @docker login@ command. The Amazon ECR registry URL format is @https://aws_account_id.dkr.ecr.region.amazonaws.com@ . For example, @https://012345678910.dkr.ecr.us-east-1.amazonaws.com@ ..
adProxyEndpoint :: Lens' AuthorizationData (Maybe Text)
adProxyEndpoint = lens _adProxyEndpoint (\ s a -> s{_adProxyEndpoint = a})

-- | A base64-encoded string that contains authorization data for the specified Amazon ECR registry. When the string is decoded, it is presented in the format @user:password@ for private registry authentication using @docker login@ .
adAuthorizationToken :: Lens' AuthorizationData (Maybe Text)
adAuthorizationToken = lens _adAuthorizationToken (\ s a -> s{_adAuthorizationToken = a})

instance FromJSON AuthorizationData where
        parseJSON
          = withObject "AuthorizationData"
              (\ x ->
                 AuthorizationData' <$>
                   (x .:? "expiresAt") <*> (x .:? "proxyEndpoint") <*>
                     (x .:? "authorizationToken"))

instance Hashable AuthorizationData where

instance NFData AuthorizationData where

-- | An object representing a filter on a 'DescribeImages' operation.
--
--
--
-- /See:/ 'describeImagesFilter' smart constructor.
newtype DescribeImagesFilter = DescribeImagesFilter'
  { _difTagStatus :: Maybe TagStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImagesFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'difTagStatus' - The tag status with which to filter your 'DescribeImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
describeImagesFilter
    :: DescribeImagesFilter
describeImagesFilter = DescribeImagesFilter' {_difTagStatus = Nothing}


-- | The tag status with which to filter your 'DescribeImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
difTagStatus :: Lens' DescribeImagesFilter (Maybe TagStatus)
difTagStatus = lens _difTagStatus (\ s a -> s{_difTagStatus = a})

instance Hashable DescribeImagesFilter where

instance NFData DescribeImagesFilter where

instance ToJSON DescribeImagesFilter where
        toJSON DescribeImagesFilter'{..}
          = object
              (catMaybes [("tagStatus" .=) <$> _difTagStatus])

-- | An object representing an Amazon ECR image.
--
--
--
-- /See:/ 'image' smart constructor.
data Image = Image'
  { _iRegistryId     :: !(Maybe Text)
  , _iImageId        :: !(Maybe ImageIdentifier)
  , _iRepositoryName :: !(Maybe Text)
  , _iImageManifest  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iRegistryId' - The AWS account ID associated with the registry containing the image.
--
-- * 'iImageId' - An object containing the image tag and image digest associated with an image.
--
-- * 'iRepositoryName' - The name of the repository associated with the image.
--
-- * 'iImageManifest' - The image manifest associated with the image.
image
    :: Image
image =
  Image'
    { _iRegistryId = Nothing
    , _iImageId = Nothing
    , _iRepositoryName = Nothing
    , _iImageManifest = Nothing
    }


-- | The AWS account ID associated with the registry containing the image.
iRegistryId :: Lens' Image (Maybe Text)
iRegistryId = lens _iRegistryId (\ s a -> s{_iRegistryId = a})

-- | An object containing the image tag and image digest associated with an image.
iImageId :: Lens' Image (Maybe ImageIdentifier)
iImageId = lens _iImageId (\ s a -> s{_iImageId = a})

-- | The name of the repository associated with the image.
iRepositoryName :: Lens' Image (Maybe Text)
iRepositoryName = lens _iRepositoryName (\ s a -> s{_iRepositoryName = a})

-- | The image manifest associated with the image.
iImageManifest :: Lens' Image (Maybe Text)
iImageManifest = lens _iImageManifest (\ s a -> s{_iImageManifest = a})

instance FromJSON Image where
        parseJSON
          = withObject "Image"
              (\ x ->
                 Image' <$>
                   (x .:? "registryId") <*> (x .:? "imageId") <*>
                     (x .:? "repositoryName")
                     <*> (x .:? "imageManifest"))

instance Hashable Image where

instance NFData Image where

-- | An object that describes an image returned by a 'DescribeImages' operation.
--
--
--
-- /See:/ 'imageDetail' smart constructor.
data ImageDetail = ImageDetail'
  { _idRegistryId       :: !(Maybe Text)
  , _idImageTags        :: !(Maybe [Text])
  , _idImageSizeInBytes :: !(Maybe Integer)
  , _idImageDigest      :: !(Maybe Text)
  , _idImagePushedAt    :: !(Maybe POSIX)
  , _idRepositoryName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idRegistryId' - The AWS account ID associated with the registry to which this image belongs.
--
-- * 'idImageTags' - The list of tags associated with this image.
--
-- * 'idImageSizeInBytes' - The size, in bytes, of the image in the repository.
--
-- * 'idImageDigest' - The @sha256@ digest of the image manifest.
--
-- * 'idImagePushedAt' - The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
--
-- * 'idRepositoryName' - The name of the repository to which this image belongs.
imageDetail
    :: ImageDetail
imageDetail =
  ImageDetail'
    { _idRegistryId = Nothing
    , _idImageTags = Nothing
    , _idImageSizeInBytes = Nothing
    , _idImageDigest = Nothing
    , _idImagePushedAt = Nothing
    , _idRepositoryName = Nothing
    }


-- | The AWS account ID associated with the registry to which this image belongs.
idRegistryId :: Lens' ImageDetail (Maybe Text)
idRegistryId = lens _idRegistryId (\ s a -> s{_idRegistryId = a})

-- | The list of tags associated with this image.
idImageTags :: Lens' ImageDetail [Text]
idImageTags = lens _idImageTags (\ s a -> s{_idImageTags = a}) . _Default . _Coerce

-- | The size, in bytes, of the image in the repository.
idImageSizeInBytes :: Lens' ImageDetail (Maybe Integer)
idImageSizeInBytes = lens _idImageSizeInBytes (\ s a -> s{_idImageSizeInBytes = a})

-- | The @sha256@ digest of the image manifest.
idImageDigest :: Lens' ImageDetail (Maybe Text)
idImageDigest = lens _idImageDigest (\ s a -> s{_idImageDigest = a})

-- | The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
idImagePushedAt :: Lens' ImageDetail (Maybe UTCTime)
idImagePushedAt = lens _idImagePushedAt (\ s a -> s{_idImagePushedAt = a}) . mapping _Time

-- | The name of the repository to which this image belongs.
idRepositoryName :: Lens' ImageDetail (Maybe Text)
idRepositoryName = lens _idRepositoryName (\ s a -> s{_idRepositoryName = a})

instance FromJSON ImageDetail where
        parseJSON
          = withObject "ImageDetail"
              (\ x ->
                 ImageDetail' <$>
                   (x .:? "registryId") <*>
                     (x .:? "imageTags" .!= mempty)
                     <*> (x .:? "imageSizeInBytes")
                     <*> (x .:? "imageDigest")
                     <*> (x .:? "imagePushedAt")
                     <*> (x .:? "repositoryName"))

instance Hashable ImageDetail where

instance NFData ImageDetail where

-- | An object representing an Amazon ECR image failure.
--
--
--
-- /See:/ 'imageFailure' smart constructor.
data ImageFailure = ImageFailure'
  { _ifFailureReason :: !(Maybe Text)
  , _ifFailureCode   :: !(Maybe ImageFailureCode)
  , _ifImageId       :: !(Maybe ImageIdentifier)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageFailure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifFailureReason' - The reason for the failure.
--
-- * 'ifFailureCode' - The code associated with the failure.
--
-- * 'ifImageId' - The image ID associated with the failure.
imageFailure
    :: ImageFailure
imageFailure =
  ImageFailure'
    {_ifFailureReason = Nothing, _ifFailureCode = Nothing, _ifImageId = Nothing}


-- | The reason for the failure.
ifFailureReason :: Lens' ImageFailure (Maybe Text)
ifFailureReason = lens _ifFailureReason (\ s a -> s{_ifFailureReason = a})

-- | The code associated with the failure.
ifFailureCode :: Lens' ImageFailure (Maybe ImageFailureCode)
ifFailureCode = lens _ifFailureCode (\ s a -> s{_ifFailureCode = a})

-- | The image ID associated with the failure.
ifImageId :: Lens' ImageFailure (Maybe ImageIdentifier)
ifImageId = lens _ifImageId (\ s a -> s{_ifImageId = a})

instance FromJSON ImageFailure where
        parseJSON
          = withObject "ImageFailure"
              (\ x ->
                 ImageFailure' <$>
                   (x .:? "failureReason") <*> (x .:? "failureCode") <*>
                     (x .:? "imageId"))

instance Hashable ImageFailure where

instance NFData ImageFailure where

-- | An object with identifying information for an Amazon ECR image.
--
--
--
-- /See:/ 'imageIdentifier' smart constructor.
data ImageIdentifier = ImageIdentifier'
  { _iiImageDigest :: !(Maybe Text)
  , _iiImageTag    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiImageDigest' - The @sha256@ digest of the image manifest.
--
-- * 'iiImageTag' - The tag used for the image.
imageIdentifier
    :: ImageIdentifier
imageIdentifier =
  ImageIdentifier' {_iiImageDigest = Nothing, _iiImageTag = Nothing}


-- | The @sha256@ digest of the image manifest.
iiImageDigest :: Lens' ImageIdentifier (Maybe Text)
iiImageDigest = lens _iiImageDigest (\ s a -> s{_iiImageDigest = a})

-- | The tag used for the image.
iiImageTag :: Lens' ImageIdentifier (Maybe Text)
iiImageTag = lens _iiImageTag (\ s a -> s{_iiImageTag = a})

instance FromJSON ImageIdentifier where
        parseJSON
          = withObject "ImageIdentifier"
              (\ x ->
                 ImageIdentifier' <$>
                   (x .:? "imageDigest") <*> (x .:? "imageTag"))

instance Hashable ImageIdentifier where

instance NFData ImageIdentifier where

instance ToJSON ImageIdentifier where
        toJSON ImageIdentifier'{..}
          = object
              (catMaybes
                 [("imageDigest" .=) <$> _iiImageDigest,
                  ("imageTag" .=) <$> _iiImageTag])

-- | An object representing an Amazon ECR image layer.
--
--
--
-- /See:/ 'layer' smart constructor.
data Layer = Layer'
  { _lMediaType         :: !(Maybe Text)
  , _lLayerDigest       :: !(Maybe Text)
  , _lLayerSize         :: !(Maybe Integer)
  , _lLayerAvailability :: !(Maybe LayerAvailability)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Layer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lMediaType' - The media type of the layer, such as @application/vnd.docker.image.rootfs.diff.tar.gzip@ or @application/vnd.oci.image.layer.v1.tar+gzip@ .
--
-- * 'lLayerDigest' - The @sha256@ digest of the image layer.
--
-- * 'lLayerSize' - The size, in bytes, of the image layer.
--
-- * 'lLayerAvailability' - The availability status of the image layer.
layer
    :: Layer
layer =
  Layer'
    { _lMediaType = Nothing
    , _lLayerDigest = Nothing
    , _lLayerSize = Nothing
    , _lLayerAvailability = Nothing
    }


-- | The media type of the layer, such as @application/vnd.docker.image.rootfs.diff.tar.gzip@ or @application/vnd.oci.image.layer.v1.tar+gzip@ .
lMediaType :: Lens' Layer (Maybe Text)
lMediaType = lens _lMediaType (\ s a -> s{_lMediaType = a})

-- | The @sha256@ digest of the image layer.
lLayerDigest :: Lens' Layer (Maybe Text)
lLayerDigest = lens _lLayerDigest (\ s a -> s{_lLayerDigest = a})

-- | The size, in bytes, of the image layer.
lLayerSize :: Lens' Layer (Maybe Integer)
lLayerSize = lens _lLayerSize (\ s a -> s{_lLayerSize = a})

-- | The availability status of the image layer.
lLayerAvailability :: Lens' Layer (Maybe LayerAvailability)
lLayerAvailability = lens _lLayerAvailability (\ s a -> s{_lLayerAvailability = a})

instance FromJSON Layer where
        parseJSON
          = withObject "Layer"
              (\ x ->
                 Layer' <$>
                   (x .:? "mediaType") <*> (x .:? "layerDigest") <*>
                     (x .:? "layerSize")
                     <*> (x .:? "layerAvailability"))

instance Hashable Layer where

instance NFData Layer where

-- | An object representing an Amazon ECR image layer failure.
--
--
--
-- /See:/ 'layerFailure' smart constructor.
data LayerFailure = LayerFailure'
  { _lfFailureReason :: !(Maybe Text)
  , _lfFailureCode   :: !(Maybe LayerFailureCode)
  , _lfLayerDigest   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LayerFailure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfFailureReason' - The reason for the failure.
--
-- * 'lfFailureCode' - The failure code associated with the failure.
--
-- * 'lfLayerDigest' - The layer digest associated with the failure.
layerFailure
    :: LayerFailure
layerFailure =
  LayerFailure'
    { _lfFailureReason = Nothing
    , _lfFailureCode = Nothing
    , _lfLayerDigest = Nothing
    }


-- | The reason for the failure.
lfFailureReason :: Lens' LayerFailure (Maybe Text)
lfFailureReason = lens _lfFailureReason (\ s a -> s{_lfFailureReason = a})

-- | The failure code associated with the failure.
lfFailureCode :: Lens' LayerFailure (Maybe LayerFailureCode)
lfFailureCode = lens _lfFailureCode (\ s a -> s{_lfFailureCode = a})

-- | The layer digest associated with the failure.
lfLayerDigest :: Lens' LayerFailure (Maybe Text)
lfLayerDigest = lens _lfLayerDigest (\ s a -> s{_lfLayerDigest = a})

instance FromJSON LayerFailure where
        parseJSON
          = withObject "LayerFailure"
              (\ x ->
                 LayerFailure' <$>
                   (x .:? "failureReason") <*> (x .:? "failureCode") <*>
                     (x .:? "layerDigest"))

instance Hashable LayerFailure where

instance NFData LayerFailure where

-- | The filter for the lifecycle policy preview.
--
--
--
-- /See:/ 'lifecyclePolicyPreviewFilter' smart constructor.
newtype LifecyclePolicyPreviewFilter = LifecyclePolicyPreviewFilter'
  { _lppfTagStatus :: Maybe TagStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecyclePolicyPreviewFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lppfTagStatus' - The tag status of the image.
lifecyclePolicyPreviewFilter
    :: LifecyclePolicyPreviewFilter
lifecyclePolicyPreviewFilter =
  LifecyclePolicyPreviewFilter' {_lppfTagStatus = Nothing}


-- | The tag status of the image.
lppfTagStatus :: Lens' LifecyclePolicyPreviewFilter (Maybe TagStatus)
lppfTagStatus = lens _lppfTagStatus (\ s a -> s{_lppfTagStatus = a})

instance Hashable LifecyclePolicyPreviewFilter where

instance NFData LifecyclePolicyPreviewFilter where

instance ToJSON LifecyclePolicyPreviewFilter where
        toJSON LifecyclePolicyPreviewFilter'{..}
          = object
              (catMaybes [("tagStatus" .=) <$> _lppfTagStatus])

-- | The result of the lifecycle policy preview.
--
--
--
-- /See:/ 'lifecyclePolicyPreviewResult' smart constructor.
data LifecyclePolicyPreviewResult = LifecyclePolicyPreviewResult'
  { _lpprImageTags           :: !(Maybe [Text])
  , _lpprAction              :: !(Maybe LifecyclePolicyRuleAction)
  , _lpprImageDigest         :: !(Maybe Text)
  , _lpprImagePushedAt       :: !(Maybe POSIX)
  , _lpprAppliedRulePriority :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecyclePolicyPreviewResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpprImageTags' - The list of tags associated with this image.
--
-- * 'lpprAction' - The type of action to be taken.
--
-- * 'lpprImageDigest' - The @sha256@ digest of the image manifest.
--
-- * 'lpprImagePushedAt' - The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
--
-- * 'lpprAppliedRulePriority' - The priority of the applied rule.
lifecyclePolicyPreviewResult
    :: LifecyclePolicyPreviewResult
lifecyclePolicyPreviewResult =
  LifecyclePolicyPreviewResult'
    { _lpprImageTags = Nothing
    , _lpprAction = Nothing
    , _lpprImageDigest = Nothing
    , _lpprImagePushedAt = Nothing
    , _lpprAppliedRulePriority = Nothing
    }


-- | The list of tags associated with this image.
lpprImageTags :: Lens' LifecyclePolicyPreviewResult [Text]
lpprImageTags = lens _lpprImageTags (\ s a -> s{_lpprImageTags = a}) . _Default . _Coerce

-- | The type of action to be taken.
lpprAction :: Lens' LifecyclePolicyPreviewResult (Maybe LifecyclePolicyRuleAction)
lpprAction = lens _lpprAction (\ s a -> s{_lpprAction = a})

-- | The @sha256@ digest of the image manifest.
lpprImageDigest :: Lens' LifecyclePolicyPreviewResult (Maybe Text)
lpprImageDigest = lens _lpprImageDigest (\ s a -> s{_lpprImageDigest = a})

-- | The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
lpprImagePushedAt :: Lens' LifecyclePolicyPreviewResult (Maybe UTCTime)
lpprImagePushedAt = lens _lpprImagePushedAt (\ s a -> s{_lpprImagePushedAt = a}) . mapping _Time

-- | The priority of the applied rule.
lpprAppliedRulePriority :: Lens' LifecyclePolicyPreviewResult (Maybe Natural)
lpprAppliedRulePriority = lens _lpprAppliedRulePriority (\ s a -> s{_lpprAppliedRulePriority = a}) . mapping _Nat

instance FromJSON LifecyclePolicyPreviewResult where
        parseJSON
          = withObject "LifecyclePolicyPreviewResult"
              (\ x ->
                 LifecyclePolicyPreviewResult' <$>
                   (x .:? "imageTags" .!= mempty) <*> (x .:? "action")
                     <*> (x .:? "imageDigest")
                     <*> (x .:? "imagePushedAt")
                     <*> (x .:? "appliedRulePriority"))

instance Hashable LifecyclePolicyPreviewResult where

instance NFData LifecyclePolicyPreviewResult where

-- | The summary of the lifecycle policy preview request.
--
--
--
-- /See:/ 'lifecyclePolicyPreviewSummary' smart constructor.
newtype LifecyclePolicyPreviewSummary = LifecyclePolicyPreviewSummary'
  { _lppsExpiringImageTotalCount :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecyclePolicyPreviewSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lppsExpiringImageTotalCount' - The number of expiring images.
lifecyclePolicyPreviewSummary
    :: LifecyclePolicyPreviewSummary
lifecyclePolicyPreviewSummary =
  LifecyclePolicyPreviewSummary' {_lppsExpiringImageTotalCount = Nothing}


-- | The number of expiring images.
lppsExpiringImageTotalCount :: Lens' LifecyclePolicyPreviewSummary (Maybe Natural)
lppsExpiringImageTotalCount = lens _lppsExpiringImageTotalCount (\ s a -> s{_lppsExpiringImageTotalCount = a}) . mapping _Nat

instance FromJSON LifecyclePolicyPreviewSummary where
        parseJSON
          = withObject "LifecyclePolicyPreviewSummary"
              (\ x ->
                 LifecyclePolicyPreviewSummary' <$>
                   (x .:? "expiringImageTotalCount"))

instance Hashable LifecyclePolicyPreviewSummary where

instance NFData LifecyclePolicyPreviewSummary where

-- | The type of action to be taken.
--
--
--
-- /See:/ 'lifecyclePolicyRuleAction' smart constructor.
newtype LifecyclePolicyRuleAction = LifecyclePolicyRuleAction'
  { _lpraType :: Maybe ImageActionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecyclePolicyRuleAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpraType' - The type of action to be taken.
lifecyclePolicyRuleAction
    :: LifecyclePolicyRuleAction
lifecyclePolicyRuleAction = LifecyclePolicyRuleAction' {_lpraType = Nothing}


-- | The type of action to be taken.
lpraType :: Lens' LifecyclePolicyRuleAction (Maybe ImageActionType)
lpraType = lens _lpraType (\ s a -> s{_lpraType = a})

instance FromJSON LifecyclePolicyRuleAction where
        parseJSON
          = withObject "LifecyclePolicyRuleAction"
              (\ x ->
                 LifecyclePolicyRuleAction' <$> (x .:? "type"))

instance Hashable LifecyclePolicyRuleAction where

instance NFData LifecyclePolicyRuleAction where

-- | An object representing a filter on a 'ListImages' operation.
--
--
--
-- /See:/ 'listImagesFilter' smart constructor.
newtype ListImagesFilter = ListImagesFilter'
  { _lifTagStatus :: Maybe TagStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListImagesFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lifTagStatus' - The tag status with which to filter your 'ListImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
listImagesFilter
    :: ListImagesFilter
listImagesFilter = ListImagesFilter' {_lifTagStatus = Nothing}


-- | The tag status with which to filter your 'ListImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
lifTagStatus :: Lens' ListImagesFilter (Maybe TagStatus)
lifTagStatus = lens _lifTagStatus (\ s a -> s{_lifTagStatus = a})

instance Hashable ListImagesFilter where

instance NFData ListImagesFilter where

instance ToJSON ListImagesFilter where
        toJSON ListImagesFilter'{..}
          = object
              (catMaybes [("tagStatus" .=) <$> _lifTagStatus])

-- | An object representing a repository.
--
--
--
-- /See:/ 'repository' smart constructor.
data Repository = Repository'
  { _rRepositoryARN  :: !(Maybe Text)
  , _rCreatedAt      :: !(Maybe POSIX)
  , _rRegistryId     :: !(Maybe Text)
  , _rRepositoryURI  :: !(Maybe Text)
  , _rRepositoryName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Repository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rRepositoryARN' - The Amazon Resource Name (ARN) that identifies the repository. The ARN contains the @arn:aws:ecr@ namespace, followed by the region of the repository, AWS account ID of the repository owner, repository namespace, and repository name. For example, @arn:aws:ecr:region:012345678910:repository/test@ .
--
-- * 'rCreatedAt' - The date and time, in JavaScript date format, when the repository was created.
--
-- * 'rRegistryId' - The AWS account ID associated with the registry that contains the repository.
--
-- * 'rRepositoryURI' - The URI for the repository. You can use this URI for Docker @push@ or @pull@ operations.
--
-- * 'rRepositoryName' - The name of the repository.
repository
    :: Repository
repository =
  Repository'
    { _rRepositoryARN = Nothing
    , _rCreatedAt = Nothing
    , _rRegistryId = Nothing
    , _rRepositoryURI = Nothing
    , _rRepositoryName = Nothing
    }


-- | The Amazon Resource Name (ARN) that identifies the repository. The ARN contains the @arn:aws:ecr@ namespace, followed by the region of the repository, AWS account ID of the repository owner, repository namespace, and repository name. For example, @arn:aws:ecr:region:012345678910:repository/test@ .
rRepositoryARN :: Lens' Repository (Maybe Text)
rRepositoryARN = lens _rRepositoryARN (\ s a -> s{_rRepositoryARN = a})

-- | The date and time, in JavaScript date format, when the repository was created.
rCreatedAt :: Lens' Repository (Maybe UTCTime)
rCreatedAt = lens _rCreatedAt (\ s a -> s{_rCreatedAt = a}) . mapping _Time

-- | The AWS account ID associated with the registry that contains the repository.
rRegistryId :: Lens' Repository (Maybe Text)
rRegistryId = lens _rRegistryId (\ s a -> s{_rRegistryId = a})

-- | The URI for the repository. You can use this URI for Docker @push@ or @pull@ operations.
rRepositoryURI :: Lens' Repository (Maybe Text)
rRepositoryURI = lens _rRepositoryURI (\ s a -> s{_rRepositoryURI = a})

-- | The name of the repository.
rRepositoryName :: Lens' Repository (Maybe Text)
rRepositoryName = lens _rRepositoryName (\ s a -> s{_rRepositoryName = a})

instance FromJSON Repository where
        parseJSON
          = withObject "Repository"
              (\ x ->
                 Repository' <$>
                   (x .:? "repositoryArn") <*> (x .:? "createdAt") <*>
                     (x .:? "registryId")
                     <*> (x .:? "repositoryUri")
                     <*> (x .:? "repositoryName"))

instance Hashable Repository where

instance NFData Repository where
