{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.Product where

import           Network.AWS.ECR.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | An object representing authorization data for an Amazon ECR registry.
--
-- /See:/ 'authorizationData' smart constructor.
data AuthorizationData = AuthorizationData'
    { _adExpiresAt          :: !(Maybe POSIX)
    , _adProxyEndpoint      :: !(Maybe Text)
    , _adAuthorizationToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuthorizationData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adExpiresAt'
--
-- * 'adProxyEndpoint'
--
-- * 'adAuthorizationToken'
authorizationData
    :: AuthorizationData
authorizationData =
    AuthorizationData'
    { _adExpiresAt = Nothing
    , _adProxyEndpoint = Nothing
    , _adAuthorizationToken = Nothing
    }

-- | The Unix time in seconds and milliseconds when the authorization token
-- expires. Authorization tokens are valid for 12 hours.
adExpiresAt :: Lens' AuthorizationData (Maybe UTCTime)
adExpiresAt = lens _adExpiresAt (\ s a -> s{_adExpiresAt = a}) . mapping _Time;

-- | The registry URL to use for this authorization token in a 'docker login'
-- command. The Amazon ECR registry URL format is
-- 'https:\/\/aws_account_id.dkr.ecr.region.amazonaws.com'. For example,
-- 'https:\/\/012345678910.dkr.ecr.us-east-1.amazonaws.com'..
adProxyEndpoint :: Lens' AuthorizationData (Maybe Text)
adProxyEndpoint = lens _adProxyEndpoint (\ s a -> s{_adProxyEndpoint = a});

-- | A base64-encoded string that contains authorization data for the
-- specified Amazon ECR registry. When the string is decoded, it is
-- presented in the format 'user:password' for private registry
-- authentication using 'docker login'.
adAuthorizationToken :: Lens' AuthorizationData (Maybe Text)
adAuthorizationToken = lens _adAuthorizationToken (\ s a -> s{_adAuthorizationToken = a});

instance FromJSON AuthorizationData where
        parseJSON
          = withObject "AuthorizationData"
              (\ x ->
                 AuthorizationData' <$>
                   (x .:? "expiresAt") <*> (x .:? "proxyEndpoint") <*>
                     (x .:? "authorizationToken"))

instance Hashable AuthorizationData

instance NFData AuthorizationData

-- | Object representing an image.
--
-- /See:/ 'image' smart constructor.
data Image = Image'
    { _iRegistryId     :: !(Maybe Text)
    , _iImageId        :: !(Maybe ImageIdentifier)
    , _iRepositoryName :: !(Maybe Text)
    , _iImageManifest  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iRegistryId'
--
-- * 'iImageId'
--
-- * 'iRepositoryName'
--
-- * 'iImageManifest'
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
iRegistryId = lens _iRegistryId (\ s a -> s{_iRegistryId = a});

-- | An object containing the image tag and image digest associated with an
-- image.
iImageId :: Lens' Image (Maybe ImageIdentifier)
iImageId = lens _iImageId (\ s a -> s{_iImageId = a});

-- | The name of the repository associated with the image.
iRepositoryName :: Lens' Image (Maybe Text)
iRepositoryName = lens _iRepositoryName (\ s a -> s{_iRepositoryName = a});

-- | The image manifest associated with the image.
iImageManifest :: Lens' Image (Maybe Text)
iImageManifest = lens _iImageManifest (\ s a -> s{_iImageManifest = a});

instance FromJSON Image where
        parseJSON
          = withObject "Image"
              (\ x ->
                 Image' <$>
                   (x .:? "registryId") <*> (x .:? "imageId") <*>
                     (x .:? "repositoryName")
                     <*> (x .:? "imageManifest"))

instance Hashable Image

instance NFData Image

-- | /See:/ 'imageFailure' smart constructor.
data ImageFailure = ImageFailure'
    { _ifFailureReason :: !(Maybe Text)
    , _ifFailureCode   :: !(Maybe ImageFailureCode)
    , _ifImageId       :: !(Maybe ImageIdentifier)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImageFailure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifFailureReason'
--
-- * 'ifFailureCode'
--
-- * 'ifImageId'
imageFailure
    :: ImageFailure
imageFailure =
    ImageFailure'
    { _ifFailureReason = Nothing
    , _ifFailureCode = Nothing
    , _ifImageId = Nothing
    }

-- | The reason for the failure.
ifFailureReason :: Lens' ImageFailure (Maybe Text)
ifFailureReason = lens _ifFailureReason (\ s a -> s{_ifFailureReason = a});

-- | The code associated with the failure.
ifFailureCode :: Lens' ImageFailure (Maybe ImageFailureCode)
ifFailureCode = lens _ifFailureCode (\ s a -> s{_ifFailureCode = a});

-- | The image ID associated with the failure.
ifImageId :: Lens' ImageFailure (Maybe ImageIdentifier)
ifImageId = lens _ifImageId (\ s a -> s{_ifImageId = a});

instance FromJSON ImageFailure where
        parseJSON
          = withObject "ImageFailure"
              (\ x ->
                 ImageFailure' <$>
                   (x .:? "failureReason") <*> (x .:? "failureCode") <*>
                     (x .:? "imageId"))

instance Hashable ImageFailure

instance NFData ImageFailure

-- | /See:/ 'imageIdentifier' smart constructor.
data ImageIdentifier = ImageIdentifier'
    { _iiImageDigest :: !(Maybe Text)
    , _iiImageTag    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImageIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiImageDigest'
--
-- * 'iiImageTag'
imageIdentifier
    :: ImageIdentifier
imageIdentifier =
    ImageIdentifier'
    { _iiImageDigest = Nothing
    , _iiImageTag = Nothing
    }

-- | The 'sha256' digest of the image manifest.
iiImageDigest :: Lens' ImageIdentifier (Maybe Text)
iiImageDigest = lens _iiImageDigest (\ s a -> s{_iiImageDigest = a});

-- | The tag used for the image.
iiImageTag :: Lens' ImageIdentifier (Maybe Text)
iiImageTag = lens _iiImageTag (\ s a -> s{_iiImageTag = a});

instance FromJSON ImageIdentifier where
        parseJSON
          = withObject "ImageIdentifier"
              (\ x ->
                 ImageIdentifier' <$>
                   (x .:? "imageDigest") <*> (x .:? "imageTag"))

instance Hashable ImageIdentifier

instance NFData ImageIdentifier

instance ToJSON ImageIdentifier where
        toJSON ImageIdentifier'{..}
          = object
              (catMaybes
                 [("imageDigest" .=) <$> _iiImageDigest,
                  ("imageTag" .=) <$> _iiImageTag])

-- | /See:/ 'layer' smart constructor.
data Layer = Layer'
    { _lLayerDigest       :: !(Maybe Text)
    , _lLayerSize         :: !(Maybe Integer)
    , _lLayerAvailability :: !(Maybe LayerAvailability)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Layer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lLayerDigest'
--
-- * 'lLayerSize'
--
-- * 'lLayerAvailability'
layer
    :: Layer
layer =
    Layer'
    { _lLayerDigest = Nothing
    , _lLayerSize = Nothing
    , _lLayerAvailability = Nothing
    }

-- | The 'sha256' digest of the image layer.
lLayerDigest :: Lens' Layer (Maybe Text)
lLayerDigest = lens _lLayerDigest (\ s a -> s{_lLayerDigest = a});

-- | The size, in bytes, of the image layer.
lLayerSize :: Lens' Layer (Maybe Integer)
lLayerSize = lens _lLayerSize (\ s a -> s{_lLayerSize = a});

-- | The availability status of the image layer. Valid values are 'AVAILABLE'
-- and 'UNAVAILABLE'.
lLayerAvailability :: Lens' Layer (Maybe LayerAvailability)
lLayerAvailability = lens _lLayerAvailability (\ s a -> s{_lLayerAvailability = a});

instance FromJSON Layer where
        parseJSON
          = withObject "Layer"
              (\ x ->
                 Layer' <$>
                   (x .:? "layerDigest") <*> (x .:? "layerSize") <*>
                     (x .:? "layerAvailability"))

instance Hashable Layer

instance NFData Layer

-- | /See:/ 'layerFailure' smart constructor.
data LayerFailure = LayerFailure'
    { _lfFailureReason :: !(Maybe Text)
    , _lfFailureCode   :: !(Maybe LayerFailureCode)
    , _lfLayerDigest   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LayerFailure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfFailureReason'
--
-- * 'lfFailureCode'
--
-- * 'lfLayerDigest'
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
lfFailureReason = lens _lfFailureReason (\ s a -> s{_lfFailureReason = a});

-- | The failure code associated with the failure.
lfFailureCode :: Lens' LayerFailure (Maybe LayerFailureCode)
lfFailureCode = lens _lfFailureCode (\ s a -> s{_lfFailureCode = a});

-- | The layer digest associated with the failure.
lfLayerDigest :: Lens' LayerFailure (Maybe Text)
lfLayerDigest = lens _lfLayerDigest (\ s a -> s{_lfLayerDigest = a});

instance FromJSON LayerFailure where
        parseJSON
          = withObject "LayerFailure"
              (\ x ->
                 LayerFailure' <$>
                   (x .:? "failureReason") <*> (x .:? "failureCode") <*>
                     (x .:? "layerDigest"))

instance Hashable LayerFailure

instance NFData LayerFailure

-- | Object representing a repository.
--
-- /See:/ 'repository' smart constructor.
data Repository = Repository'
    { _rRepositoryARN  :: !(Maybe Text)
    , _rRegistryId     :: !(Maybe Text)
    , _rRepositoryURI  :: !(Maybe Text)
    , _rRepositoryName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Repository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rRepositoryARN'
--
-- * 'rRegistryId'
--
-- * 'rRepositoryURI'
--
-- * 'rRepositoryName'
repository
    :: Repository
repository =
    Repository'
    { _rRepositoryARN = Nothing
    , _rRegistryId = Nothing
    , _rRepositoryURI = Nothing
    , _rRepositoryName = Nothing
    }

-- | The Amazon Resource Name (ARN) that identifies the repository. The ARN
-- contains the 'arn:aws:ecr' namespace, followed by the region of the
-- repository, the AWS account ID of the repository owner, the repository
-- namespace, and then the repository name. For example,
-- 'arn:aws:ecr:region:012345678910:repository\/test'.
rRepositoryARN :: Lens' Repository (Maybe Text)
rRepositoryARN = lens _rRepositoryARN (\ s a -> s{_rRepositoryARN = a});

-- | The AWS account ID associated with the registry that contains the
-- repository.
rRegistryId :: Lens' Repository (Maybe Text)
rRegistryId = lens _rRegistryId (\ s a -> s{_rRegistryId = a});

-- | The URI for the repository. You can use this URI for Docker 'push' and
-- 'pull' operations.
rRepositoryURI :: Lens' Repository (Maybe Text)
rRepositoryURI = lens _rRepositoryURI (\ s a -> s{_rRepositoryURI = a});

-- | The name of the repository.
rRepositoryName :: Lens' Repository (Maybe Text)
rRepositoryName = lens _rRepositoryName (\ s a -> s{_rRepositoryName = a});

instance FromJSON Repository where
        parseJSON
          = withObject "Repository"
              (\ x ->
                 Repository' <$>
                   (x .:? "repositoryArn") <*> (x .:? "registryId") <*>
                     (x .:? "repositoryUri")
                     <*> (x .:? "repositoryName"))

instance Hashable Repository

instance NFData Repository
