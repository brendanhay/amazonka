{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Stage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Stage where

import Network.AWS.APIGateway.Types.AccessLogSettings
import Network.AWS.APIGateway.Types.CacheClusterSize
import Network.AWS.APIGateway.Types.CacheClusterStatus
import Network.AWS.APIGateway.Types.CanarySettings
import Network.AWS.APIGateway.Types.MethodSetting
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a unique identifier for a version of a deployed 'RestApi' that is callable by users.
--
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html Deploy an API>
--
-- /See:/ 'stage' smart constructor.
data Stage = Stage'
  { _sDeploymentId :: !(Maybe Text),
    _sVariables :: !(Maybe (Map Text (Text))),
    _sAccessLogSettings :: !(Maybe AccessLogSettings),
    _sDocumentationVersion :: !(Maybe Text),
    _sClientCertificateId :: !(Maybe Text),
    _sTracingEnabled :: !(Maybe Bool),
    _sCreatedDate :: !(Maybe POSIX),
    _sCacheClusterStatus :: !(Maybe CacheClusterStatus),
    _sMethodSettings :: !(Maybe (Map Text (MethodSetting))),
    _sLastUpdatedDate :: !(Maybe POSIX),
    _sCacheClusterSize :: !(Maybe CacheClusterSize),
    _sWebACLARN :: !(Maybe Text),
    _sCanarySettings :: !(Maybe CanarySettings),
    _sCacheClusterEnabled :: !(Maybe Bool),
    _sStageName :: !(Maybe Text),
    _sDescription :: !(Maybe Text),
    _sTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Stage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDeploymentId' - The identifier of the 'Deployment' that the stage points to.
--
-- * 'sVariables' - A map that defines the stage variables for a 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
--
-- * 'sAccessLogSettings' - Settings for logging access in this stage.
--
-- * 'sDocumentationVersion' - The version of the associated API documentation.
--
-- * 'sClientCertificateId' - The identifier of a client certificate for an API stage.
--
-- * 'sTracingEnabled' - Specifies whether active tracing with X-ray is enabled for the 'Stage' .
--
-- * 'sCreatedDate' - The timestamp when the stage was created.
--
-- * 'sCacheClusterStatus' - The status of the cache cluster for the stage, if enabled.
--
-- * 'sMethodSettings' - A map that defines the method settings for a 'Stage' resource. Keys (designated as @/{method_setting_key@ below) are method paths defined as @{resource_path}/{http_method}@ for an individual method override, or @/\*/\*@ for overriding all methods in the stage.
--
-- * 'sLastUpdatedDate' - The timestamp when the stage last updated.
--
-- * 'sCacheClusterSize' - The size of the cache cluster for the stage, if enabled.
--
-- * 'sWebACLARN' - The ARN of the WebAcl associated with the 'Stage' .
--
-- * 'sCanarySettings' - Settings for the canary deployment in this stage.
--
-- * 'sCacheClusterEnabled' - Specifies whether a cache cluster is enabled for the stage.
--
-- * 'sStageName' - The name of the stage is the first path segment in the Uniform Resource Identifier (URI) of a call to API Gateway. Stage names can only contain alphanumeric characters, hyphens, and underscores. Maximum length is 128 characters.
--
-- * 'sDescription' - The stage's description.
--
-- * 'sTags' - The collection of tags. Each tag element is associated with a given resource.
stage ::
  Stage
stage =
  Stage'
    { _sDeploymentId = Nothing,
      _sVariables = Nothing,
      _sAccessLogSettings = Nothing,
      _sDocumentationVersion = Nothing,
      _sClientCertificateId = Nothing,
      _sTracingEnabled = Nothing,
      _sCreatedDate = Nothing,
      _sCacheClusterStatus = Nothing,
      _sMethodSettings = Nothing,
      _sLastUpdatedDate = Nothing,
      _sCacheClusterSize = Nothing,
      _sWebACLARN = Nothing,
      _sCanarySettings = Nothing,
      _sCacheClusterEnabled = Nothing,
      _sStageName = Nothing,
      _sDescription = Nothing,
      _sTags = Nothing
    }

-- | The identifier of the 'Deployment' that the stage points to.
sDeploymentId :: Lens' Stage (Maybe Text)
sDeploymentId = lens _sDeploymentId (\s a -> s {_sDeploymentId = a})

-- | A map that defines the stage variables for a 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
sVariables :: Lens' Stage (HashMap Text (Text))
sVariables = lens _sVariables (\s a -> s {_sVariables = a}) . _Default . _Map

-- | Settings for logging access in this stage.
sAccessLogSettings :: Lens' Stage (Maybe AccessLogSettings)
sAccessLogSettings = lens _sAccessLogSettings (\s a -> s {_sAccessLogSettings = a})

-- | The version of the associated API documentation.
sDocumentationVersion :: Lens' Stage (Maybe Text)
sDocumentationVersion = lens _sDocumentationVersion (\s a -> s {_sDocumentationVersion = a})

-- | The identifier of a client certificate for an API stage.
sClientCertificateId :: Lens' Stage (Maybe Text)
sClientCertificateId = lens _sClientCertificateId (\s a -> s {_sClientCertificateId = a})

-- | Specifies whether active tracing with X-ray is enabled for the 'Stage' .
sTracingEnabled :: Lens' Stage (Maybe Bool)
sTracingEnabled = lens _sTracingEnabled (\s a -> s {_sTracingEnabled = a})

-- | The timestamp when the stage was created.
sCreatedDate :: Lens' Stage (Maybe UTCTime)
sCreatedDate = lens _sCreatedDate (\s a -> s {_sCreatedDate = a}) . mapping _Time

-- | The status of the cache cluster for the stage, if enabled.
sCacheClusterStatus :: Lens' Stage (Maybe CacheClusterStatus)
sCacheClusterStatus = lens _sCacheClusterStatus (\s a -> s {_sCacheClusterStatus = a})

-- | A map that defines the method settings for a 'Stage' resource. Keys (designated as @/{method_setting_key@ below) are method paths defined as @{resource_path}/{http_method}@ for an individual method override, or @/\*/\*@ for overriding all methods in the stage.
sMethodSettings :: Lens' Stage (HashMap Text (MethodSetting))
sMethodSettings = lens _sMethodSettings (\s a -> s {_sMethodSettings = a}) . _Default . _Map

-- | The timestamp when the stage last updated.
sLastUpdatedDate :: Lens' Stage (Maybe UTCTime)
sLastUpdatedDate = lens _sLastUpdatedDate (\s a -> s {_sLastUpdatedDate = a}) . mapping _Time

-- | The size of the cache cluster for the stage, if enabled.
sCacheClusterSize :: Lens' Stage (Maybe CacheClusterSize)
sCacheClusterSize = lens _sCacheClusterSize (\s a -> s {_sCacheClusterSize = a})

-- | The ARN of the WebAcl associated with the 'Stage' .
sWebACLARN :: Lens' Stage (Maybe Text)
sWebACLARN = lens _sWebACLARN (\s a -> s {_sWebACLARN = a})

-- | Settings for the canary deployment in this stage.
sCanarySettings :: Lens' Stage (Maybe CanarySettings)
sCanarySettings = lens _sCanarySettings (\s a -> s {_sCanarySettings = a})

-- | Specifies whether a cache cluster is enabled for the stage.
sCacheClusterEnabled :: Lens' Stage (Maybe Bool)
sCacheClusterEnabled = lens _sCacheClusterEnabled (\s a -> s {_sCacheClusterEnabled = a})

-- | The name of the stage is the first path segment in the Uniform Resource Identifier (URI) of a call to API Gateway. Stage names can only contain alphanumeric characters, hyphens, and underscores. Maximum length is 128 characters.
sStageName :: Lens' Stage (Maybe Text)
sStageName = lens _sStageName (\s a -> s {_sStageName = a})

-- | The stage's description.
sDescription :: Lens' Stage (Maybe Text)
sDescription = lens _sDescription (\s a -> s {_sDescription = a})

-- | The collection of tags. Each tag element is associated with a given resource.
sTags :: Lens' Stage (HashMap Text (Text))
sTags = lens _sTags (\s a -> s {_sTags = a}) . _Default . _Map

instance FromJSON Stage where
  parseJSON =
    withObject
      "Stage"
      ( \x ->
          Stage'
            <$> (x .:? "deploymentId")
            <*> (x .:? "variables" .!= mempty)
            <*> (x .:? "accessLogSettings")
            <*> (x .:? "documentationVersion")
            <*> (x .:? "clientCertificateId")
            <*> (x .:? "tracingEnabled")
            <*> (x .:? "createdDate")
            <*> (x .:? "cacheClusterStatus")
            <*> (x .:? "methodSettings" .!= mempty)
            <*> (x .:? "lastUpdatedDate")
            <*> (x .:? "cacheClusterSize")
            <*> (x .:? "webAclArn")
            <*> (x .:? "canarySettings")
            <*> (x .:? "cacheClusterEnabled")
            <*> (x .:? "stageName")
            <*> (x .:? "description")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable Stage

instance NFData Stage
