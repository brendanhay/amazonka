{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateStage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new 'Stage' resource that references a pre-existing 'Deployment' for the API.
--
--
module Network.AWS.APIGateway.CreateStage
    (
    -- * Creating a Request
      createStage
    , CreateStage
    -- * Request Lenses
    , cVariables
    , cDocumentationVersion
    , cCacheClusterSize
    , cCanarySettings
    , cCacheClusterEnabled
    , cDescription
    , cTags
    , cRestAPIId
    , cStageName
    , cDeploymentId

    -- * Destructuring the Response
    , stage
    , Stage
    -- * Response Lenses
    , sDeploymentId
    , sVariables
    , sAccessLogSettings
    , sDocumentationVersion
    , sClientCertificateId
    , sCreatedDate
    , sCacheClusterStatus
    , sMethodSettings
    , sLastUpdatedDate
    , sCacheClusterSize
    , sCanarySettings
    , sCacheClusterEnabled
    , sStageName
    , sDescription
    , sTags
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Requests API Gateway to create a 'Stage' resource.
--
--
--
-- /See:/ 'createStage' smart constructor.
data CreateStage = CreateStage'
  { _cVariables            :: !(Maybe (Map Text Text))
  , _cDocumentationVersion :: !(Maybe Text)
  , _cCacheClusterSize     :: !(Maybe CacheClusterSize)
  , _cCanarySettings       :: !(Maybe CanarySettings)
  , _cCacheClusterEnabled  :: !(Maybe Bool)
  , _cDescription          :: !(Maybe Text)
  , _cTags                 :: !(Maybe (Map Text Text))
  , _cRestAPIId            :: !Text
  , _cStageName            :: !Text
  , _cDeploymentId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cVariables' - A map that defines the stage variables for the new 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
--
-- * 'cDocumentationVersion' - The version of the associated API documentation.
--
-- * 'cCacheClusterSize' - The stage's cache cluster size.
--
-- * 'cCanarySettings' - The canary deployment settings of this stage.
--
-- * 'cCacheClusterEnabled' - Whether cache clustering is enabled for the stage.
--
-- * 'cDescription' - The description of the 'Stage' resource.
--
-- * 'cTags' - The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- * 'cRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'cStageName' - [Required] The name for the 'Stage' resource.
--
-- * 'cDeploymentId' - [Required] The identifier of the 'Deployment' resource for the 'Stage' resource.
createStage
    :: Text -- ^ 'cRestAPIId'
    -> Text -- ^ 'cStageName'
    -> Text -- ^ 'cDeploymentId'
    -> CreateStage
createStage pRestAPIId_ pStageName_ pDeploymentId_ =
  CreateStage'
    { _cVariables = Nothing
    , _cDocumentationVersion = Nothing
    , _cCacheClusterSize = Nothing
    , _cCanarySettings = Nothing
    , _cCacheClusterEnabled = Nothing
    , _cDescription = Nothing
    , _cTags = Nothing
    , _cRestAPIId = pRestAPIId_
    , _cStageName = pStageName_
    , _cDeploymentId = pDeploymentId_
    }


-- | A map that defines the stage variables for the new 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
cVariables :: Lens' CreateStage (HashMap Text Text)
cVariables = lens _cVariables (\ s a -> s{_cVariables = a}) . _Default . _Map

-- | The version of the associated API documentation.
cDocumentationVersion :: Lens' CreateStage (Maybe Text)
cDocumentationVersion = lens _cDocumentationVersion (\ s a -> s{_cDocumentationVersion = a})

-- | The stage's cache cluster size.
cCacheClusterSize :: Lens' CreateStage (Maybe CacheClusterSize)
cCacheClusterSize = lens _cCacheClusterSize (\ s a -> s{_cCacheClusterSize = a})

-- | The canary deployment settings of this stage.
cCanarySettings :: Lens' CreateStage (Maybe CanarySettings)
cCanarySettings = lens _cCanarySettings (\ s a -> s{_cCanarySettings = a})

-- | Whether cache clustering is enabled for the stage.
cCacheClusterEnabled :: Lens' CreateStage (Maybe Bool)
cCacheClusterEnabled = lens _cCacheClusterEnabled (\ s a -> s{_cCacheClusterEnabled = a})

-- | The description of the 'Stage' resource.
cDescription :: Lens' CreateStage (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a})

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
cTags :: Lens' CreateStage (HashMap Text Text)
cTags = lens _cTags (\ s a -> s{_cTags = a}) . _Default . _Map

-- | [Required] The string identifier of the associated 'RestApi' .
cRestAPIId :: Lens' CreateStage Text
cRestAPIId = lens _cRestAPIId (\ s a -> s{_cRestAPIId = a})

-- | [Required] The name for the 'Stage' resource.
cStageName :: Lens' CreateStage Text
cStageName = lens _cStageName (\ s a -> s{_cStageName = a})

-- | [Required] The identifier of the 'Deployment' resource for the 'Stage' resource.
cDeploymentId :: Lens' CreateStage Text
cDeploymentId = lens _cDeploymentId (\ s a -> s{_cDeploymentId = a})

instance AWSRequest CreateStage where
        type Rs CreateStage = Stage
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateStage where

instance NFData CreateStage where

instance ToHeaders CreateStage where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateStage where
        toJSON CreateStage'{..}
          = object
              (catMaybes
                 [("variables" .=) <$> _cVariables,
                  ("documentationVersion" .=) <$>
                    _cDocumentationVersion,
                  ("cacheClusterSize" .=) <$> _cCacheClusterSize,
                  ("canarySettings" .=) <$> _cCanarySettings,
                  ("cacheClusterEnabled" .=) <$> _cCacheClusterEnabled,
                  ("description" .=) <$> _cDescription,
                  ("tags" .=) <$> _cTags,
                  Just ("stageName" .= _cStageName),
                  Just ("deploymentId" .= _cDeploymentId)])

instance ToPath CreateStage where
        toPath CreateStage'{..}
          = mconcat ["/restapis/", toBS _cRestAPIId, "/stages"]

instance ToQuery CreateStage where
        toQuery = const mempty
