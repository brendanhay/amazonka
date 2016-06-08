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
-- Module      : Network.AWS.APIGateway.GetStages
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more < Stage> resources.
module Network.AWS.APIGateway.GetStages
    (
    -- * Creating a Request
      getStages
    , GetStages
    -- * Request Lenses
    , gsDeploymentId
    , gsRestAPIId

    -- * Destructuring the Response
    , getStagesResponse
    , GetStagesResponse
    -- * Response Lenses
    , gsrsItem
    , gsrsResponseStatus
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Requests Amazon API Gateway to get information about one or more < Stage> resources.
--
-- /See:/ 'getStages' smart constructor.
data GetStages = GetStages'
    { _gsDeploymentId :: !(Maybe Text)
    , _gsRestAPIId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetStages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsDeploymentId'
--
-- * 'gsRestAPIId'
getStages
    :: Text -- ^ 'gsRestAPIId'
    -> GetStages
getStages pRestAPIId_ =
    GetStages'
    { _gsDeploymentId = Nothing
    , _gsRestAPIId = pRestAPIId_
    }

-- | The stages\' deployment identifiers.
gsDeploymentId :: Lens' GetStages (Maybe Text)
gsDeploymentId = lens _gsDeploymentId (\ s a -> s{_gsDeploymentId = a});

-- | The stages\' API identifiers.
gsRestAPIId :: Lens' GetStages Text
gsRestAPIId = lens _gsRestAPIId (\ s a -> s{_gsRestAPIId = a});

instance AWSRequest GetStages where
        type Rs GetStages = GetStagesResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetStagesResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable GetStages

instance NFData GetStages

instance ToHeaders GetStages where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetStages where
        toPath GetStages'{..}
          = mconcat
              ["/restapis/", toBS _gsRestAPIId, "/stages"]

instance ToQuery GetStages where
        toQuery GetStages'{..}
          = mconcat ["deploymentId" =: _gsDeploymentId]

-- | A list of < Stage> resource that are associated with the < ApiKey> resource.
--
-- /See:/ 'getStagesResponse' smart constructor.
data GetStagesResponse = GetStagesResponse'
    { _gsrsItem           :: !(Maybe [Stage])
    , _gsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetStagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrsItem'
--
-- * 'gsrsResponseStatus'
getStagesResponse
    :: Int -- ^ 'gsrsResponseStatus'
    -> GetStagesResponse
getStagesResponse pResponseStatus_ =
    GetStagesResponse'
    { _gsrsItem = Nothing
    , _gsrsResponseStatus = pResponseStatus_
    }

-- | An individual < Stage> resource.
gsrsItem :: Lens' GetStagesResponse [Stage]
gsrsItem = lens _gsrsItem (\ s a -> s{_gsrsItem = a}) . _Default . _Coerce;

-- | The response status code.
gsrsResponseStatus :: Lens' GetStagesResponse Int
gsrsResponseStatus = lens _gsrsResponseStatus (\ s a -> s{_gsrsResponseStatus = a});

instance NFData GetStagesResponse
