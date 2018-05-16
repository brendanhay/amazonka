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
-- Module      : Network.AWS.Pinpoint.CreateCampaign
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a campaign.
module Network.AWS.Pinpoint.CreateCampaign
    (
    -- * Creating a Request
      createCampaign
    , CreateCampaign
    -- * Request Lenses
    , ccApplicationId
    , ccWriteCampaignRequest

    -- * Destructuring the Response
    , createCampaignResponse
    , CreateCampaignResponse
    -- * Response Lenses
    , ccrsResponseStatus
    , ccrsCampaignResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCampaign' smart constructor.
data CreateCampaign = CreateCampaign'
  { _ccApplicationId        :: !Text
  , _ccWriteCampaignRequest :: !WriteCampaignRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCampaign' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccApplicationId' - Undocumented member.
--
-- * 'ccWriteCampaignRequest' - Undocumented member.
createCampaign
    :: Text -- ^ 'ccApplicationId'
    -> WriteCampaignRequest -- ^ 'ccWriteCampaignRequest'
    -> CreateCampaign
createCampaign pApplicationId_ pWriteCampaignRequest_ =
  CreateCampaign'
    { _ccApplicationId = pApplicationId_
    , _ccWriteCampaignRequest = pWriteCampaignRequest_
    }


-- | Undocumented member.
ccApplicationId :: Lens' CreateCampaign Text
ccApplicationId = lens _ccApplicationId (\ s a -> s{_ccApplicationId = a})

-- | Undocumented member.
ccWriteCampaignRequest :: Lens' CreateCampaign WriteCampaignRequest
ccWriteCampaignRequest = lens _ccWriteCampaignRequest (\ s a -> s{_ccWriteCampaignRequest = a})

instance AWSRequest CreateCampaign where
        type Rs CreateCampaign = CreateCampaignResponse
        request = postJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 CreateCampaignResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable CreateCampaign where

instance NFData CreateCampaign where

instance ToHeaders CreateCampaign where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCampaign where
        toJSON CreateCampaign'{..}
          = object
              (catMaybes
                 [Just
                    ("WriteCampaignRequest" .= _ccWriteCampaignRequest)])

instance ToPath CreateCampaign where
        toPath CreateCampaign'{..}
          = mconcat
              ["/v1/apps/", toBS _ccApplicationId, "/campaigns"]

instance ToQuery CreateCampaign where
        toQuery = const mempty

-- | /See:/ 'createCampaignResponse' smart constructor.
data CreateCampaignResponse = CreateCampaignResponse'
  { _ccrsResponseStatus   :: !Int
  , _ccrsCampaignResponse :: !CampaignResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCampaignResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsResponseStatus' - -- | The response status code.
--
-- * 'ccrsCampaignResponse' - Undocumented member.
createCampaignResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CampaignResponse -- ^ 'ccrsCampaignResponse'
    -> CreateCampaignResponse
createCampaignResponse pResponseStatus_ pCampaignResponse_ =
  CreateCampaignResponse'
    { _ccrsResponseStatus = pResponseStatus_
    , _ccrsCampaignResponse = pCampaignResponse_
    }


-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateCampaignResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

-- | Undocumented member.
ccrsCampaignResponse :: Lens' CreateCampaignResponse CampaignResponse
ccrsCampaignResponse = lens _ccrsCampaignResponse (\ s a -> s{_ccrsCampaignResponse = a})

instance NFData CreateCampaignResponse where
