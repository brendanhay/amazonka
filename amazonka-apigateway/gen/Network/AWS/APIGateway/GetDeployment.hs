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
-- Module      : Network.AWS.APIGateway.GetDeployment
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a < Deployment> resource.
module Network.AWS.APIGateway.GetDeployment
    (
    -- * Creating a Request
      getDeployment
    , GetDeployment
    -- * Request Lenses
    , gRestAPIId
    , gDeploymentId

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

-- | Requests Amazon API Gateway to get information about a < Deployment> resource.
--
-- /See:/ 'getDeployment' smart constructor.
data GetDeployment = GetDeployment'
    { _gRestAPIId    :: !Text
    , _gDeploymentId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gRestAPIId'
--
-- * 'gDeploymentId'
getDeployment
    :: Text -- ^ 'gRestAPIId'
    -> Text -- ^ 'gDeploymentId'
    -> GetDeployment
getDeployment pRestAPIId_ pDeploymentId_ =
    GetDeployment'
    { _gRestAPIId = pRestAPIId_
    , _gDeploymentId = pDeploymentId_
    }

-- | The identifier of the < RestApi> resource for the < Deployment> resource to get information about.
gRestAPIId :: Lens' GetDeployment Text
gRestAPIId = lens _gRestAPIId (\ s a -> s{_gRestAPIId = a});

-- | The identifier of the < Deployment> resource to get information about.
gDeploymentId :: Lens' GetDeployment Text
gDeploymentId = lens _gDeploymentId (\ s a -> s{_gDeploymentId = a});

instance AWSRequest GetDeployment where
        type Rs GetDeployment = Deployment
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetDeployment

instance NFData GetDeployment

instance ToHeaders GetDeployment where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetDeployment where
        toPath GetDeployment'{..}
          = mconcat
              ["/restapis/", toBS _gRestAPIId, "/deployments/",
               toBS _gDeploymentId]

instance ToQuery GetDeployment where
        toQuery = const mempty
