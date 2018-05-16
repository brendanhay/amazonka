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
-- Module      : Network.AWS.APIGateway.CreateDeployment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'Deployment' resource, which makes a specified 'RestApi' callable over the internet.
--
--
module Network.AWS.APIGateway.CreateDeployment
    (
    -- * Creating a Request
      createDeployment
    , CreateDeployment
    -- * Request Lenses
    , cdStageDescription
    , cdVariables
    , cdCacheClusterSize
    , cdCanarySettings
    , cdCacheClusterEnabled
    , cdStageName
    , cdDescription
    , cdRestAPIId

    -- * Destructuring the Response
    , deployment
    , Deployment
    -- * Response Lenses
    , dApiSummary
    , dCreatedDate
    , dId
    , dDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Requests API Gateway to create a 'Deployment' resource.
--
--
--
-- /See:/ 'createDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { _cdStageDescription    :: !(Maybe Text)
  , _cdVariables           :: !(Maybe (Map Text Text))
  , _cdCacheClusterSize    :: !(Maybe CacheClusterSize)
  , _cdCanarySettings      :: !(Maybe DeploymentCanarySettings)
  , _cdCacheClusterEnabled :: !(Maybe Bool)
  , _cdStageName           :: !(Maybe Text)
  , _cdDescription         :: !(Maybe Text)
  , _cdRestAPIId           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdStageDescription' - The description of the 'Stage' resource for the 'Deployment' resource to create.
--
-- * 'cdVariables' - A map that defines the stage variables for the 'Stage' resource that is associated with the new deployment. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
--
-- * 'cdCacheClusterSize' - Specifies the cache cluster size for the 'Stage' resource specified in the input, if a cache cluster is enabled.
--
-- * 'cdCanarySettings' - The input configuration for the canary deployment when the deployment is a canary release deployment.
--
-- * 'cdCacheClusterEnabled' - Enables a cache cluster for the 'Stage' resource specified in the input.
--
-- * 'cdStageName' - The name of the 'Stage' resource for the 'Deployment' resource to create.
--
-- * 'cdDescription' - The description for the 'Deployment' resource to create.
--
-- * 'cdRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
createDeployment
    :: Text -- ^ 'cdRestAPIId'
    -> CreateDeployment
createDeployment pRestAPIId_ =
  CreateDeployment'
    { _cdStageDescription = Nothing
    , _cdVariables = Nothing
    , _cdCacheClusterSize = Nothing
    , _cdCanarySettings = Nothing
    , _cdCacheClusterEnabled = Nothing
    , _cdStageName = Nothing
    , _cdDescription = Nothing
    , _cdRestAPIId = pRestAPIId_
    }


-- | The description of the 'Stage' resource for the 'Deployment' resource to create.
cdStageDescription :: Lens' CreateDeployment (Maybe Text)
cdStageDescription = lens _cdStageDescription (\ s a -> s{_cdStageDescription = a})

-- | A map that defines the stage variables for the 'Stage' resource that is associated with the new deployment. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
cdVariables :: Lens' CreateDeployment (HashMap Text Text)
cdVariables = lens _cdVariables (\ s a -> s{_cdVariables = a}) . _Default . _Map

-- | Specifies the cache cluster size for the 'Stage' resource specified in the input, if a cache cluster is enabled.
cdCacheClusterSize :: Lens' CreateDeployment (Maybe CacheClusterSize)
cdCacheClusterSize = lens _cdCacheClusterSize (\ s a -> s{_cdCacheClusterSize = a})

-- | The input configuration for the canary deployment when the deployment is a canary release deployment.
cdCanarySettings :: Lens' CreateDeployment (Maybe DeploymentCanarySettings)
cdCanarySettings = lens _cdCanarySettings (\ s a -> s{_cdCanarySettings = a})

-- | Enables a cache cluster for the 'Stage' resource specified in the input.
cdCacheClusterEnabled :: Lens' CreateDeployment (Maybe Bool)
cdCacheClusterEnabled = lens _cdCacheClusterEnabled (\ s a -> s{_cdCacheClusterEnabled = a})

-- | The name of the 'Stage' resource for the 'Deployment' resource to create.
cdStageName :: Lens' CreateDeployment (Maybe Text)
cdStageName = lens _cdStageName (\ s a -> s{_cdStageName = a})

-- | The description for the 'Deployment' resource to create.
cdDescription :: Lens' CreateDeployment (Maybe Text)
cdDescription = lens _cdDescription (\ s a -> s{_cdDescription = a})

-- | [Required] The string identifier of the associated 'RestApi' .
cdRestAPIId :: Lens' CreateDeployment Text
cdRestAPIId = lens _cdRestAPIId (\ s a -> s{_cdRestAPIId = a})

instance AWSRequest CreateDeployment where
        type Rs CreateDeployment = Deployment
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateDeployment where

instance NFData CreateDeployment where

instance ToHeaders CreateDeployment where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateDeployment where
        toJSON CreateDeployment'{..}
          = object
              (catMaybes
                 [("stageDescription" .=) <$> _cdStageDescription,
                  ("variables" .=) <$> _cdVariables,
                  ("cacheClusterSize" .=) <$> _cdCacheClusterSize,
                  ("canarySettings" .=) <$> _cdCanarySettings,
                  ("cacheClusterEnabled" .=) <$>
                    _cdCacheClusterEnabled,
                  ("stageName" .=) <$> _cdStageName,
                  ("description" .=) <$> _cdDescription])

instance ToPath CreateDeployment where
        toPath CreateDeployment'{..}
          = mconcat
              ["/restapis/", toBS _cdRestAPIId, "/deployments"]

instance ToQuery CreateDeployment where
        toQuery = const mempty
