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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a < Deployment> resource, which makes a specified < RestApi> callable over the internet.
module Network.AWS.APIGateway.CreateDeployment
    (
    -- * Creating a Request
      createDeployment
    , CreateDeployment
    -- * Request Lenses
    , cdStageDescription
    , cdVariables
    , cdCacheClusterSize
    , cdCacheClusterEnabled
    , cdDescription
    , cdRestAPIId
    , cdStageName

    -- * Destructuring the Response
    , deployment
    , Deployment
    -- * Response Lenses
    , dApiSummary
    , dCreatedDate
    , dId
    , dDescription
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Requests Amazon API Gateway to create a < Deployment> resource.
--
-- /See:/ 'createDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
    { _cdStageDescription    :: !(Maybe Text)
    , _cdVariables           :: !(Maybe (Map Text Text))
    , _cdCacheClusterSize    :: !(Maybe CacheClusterSize)
    , _cdCacheClusterEnabled :: !(Maybe Bool)
    , _cdDescription         :: !(Maybe Text)
    , _cdRestAPIId           :: !Text
    , _cdStageName           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdStageDescription'
--
-- * 'cdVariables'
--
-- * 'cdCacheClusterSize'
--
-- * 'cdCacheClusterEnabled'
--
-- * 'cdDescription'
--
-- * 'cdRestAPIId'
--
-- * 'cdStageName'
createDeployment
    :: Text -- ^ 'cdRestAPIId'
    -> Text -- ^ 'cdStageName'
    -> CreateDeployment
createDeployment pRestAPIId_ pStageName_ =
    CreateDeployment'
    { _cdStageDescription = Nothing
    , _cdVariables = Nothing
    , _cdCacheClusterSize = Nothing
    , _cdCacheClusterEnabled = Nothing
    , _cdDescription = Nothing
    , _cdRestAPIId = pRestAPIId_
    , _cdStageName = pStageName_
    }

-- | The description of the < Stage> resource for the < Deployment> resource to create.
cdStageDescription :: Lens' CreateDeployment (Maybe Text)
cdStageDescription = lens _cdStageDescription (\ s a -> s{_cdStageDescription = a});

-- | A map that defines the stage variables for the < Stage> resource that is associated with the new deployment. Variable names can have alphanumeric characters, and the values must match '[A-Za-z0-9-._~:\/?#&=,]+'.
cdVariables :: Lens' CreateDeployment (HashMap Text Text)
cdVariables = lens _cdVariables (\ s a -> s{_cdVariables = a}) . _Default . _Map;

-- | Specifies the cache cluster size for the < Stage> resource specified in the input, if a cache cluster is enabled.
cdCacheClusterSize :: Lens' CreateDeployment (Maybe CacheClusterSize)
cdCacheClusterSize = lens _cdCacheClusterSize (\ s a -> s{_cdCacheClusterSize = a});

-- | Enables a cache cluster for the < Stage> resource specified in the input.
cdCacheClusterEnabled :: Lens' CreateDeployment (Maybe Bool)
cdCacheClusterEnabled = lens _cdCacheClusterEnabled (\ s a -> s{_cdCacheClusterEnabled = a});

-- | The description for the < Deployment> resource to create.
cdDescription :: Lens' CreateDeployment (Maybe Text)
cdDescription = lens _cdDescription (\ s a -> s{_cdDescription = a});

-- | The < RestApi> resource identifier for the < Deployment> resource to create.
cdRestAPIId :: Lens' CreateDeployment Text
cdRestAPIId = lens _cdRestAPIId (\ s a -> s{_cdRestAPIId = a});

-- | The name of the < Stage> resource for the < Deployment> resource to create.
cdStageName :: Lens' CreateDeployment Text
cdStageName = lens _cdStageName (\ s a -> s{_cdStageName = a});

instance AWSRequest CreateDeployment where
        type Rs CreateDeployment = Deployment
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateDeployment

instance NFData CreateDeployment

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
                  ("cacheClusterEnabled" .=) <$>
                    _cdCacheClusterEnabled,
                  ("description" .=) <$> _cdDescription,
                  Just ("stageName" .= _cdStageName)])

instance ToPath CreateDeployment where
        toPath CreateDeployment'{..}
          = mconcat
              ["/restapis/", toBS _cdRestAPIId, "/deployments"]

instance ToQuery CreateDeployment where
        toQuery = const mempty
