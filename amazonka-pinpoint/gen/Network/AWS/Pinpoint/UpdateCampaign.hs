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
-- Module      : Network.AWS.Pinpoint.UpdateCampaign
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use to update a campaign.
module Network.AWS.Pinpoint.UpdateCampaign
    (
    -- * Creating a Request
      updateCampaign
    , UpdateCampaign
    -- * Request Lenses
    , ucCampaignId
    , ucApplicationId
    , ucWriteCampaignRequest

    -- * Destructuring the Response
    , updateCampaignResponse
    , UpdateCampaignResponse
    -- * Response Lenses
    , ucrsResponseStatus
    , ucrsCampaignResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateCampaign' smart constructor.
data UpdateCampaign = UpdateCampaign'
  { _ucCampaignId           :: !Text
  , _ucApplicationId        :: !Text
  , _ucWriteCampaignRequest :: !WriteCampaignRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCampaign' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucCampaignId' - Undocumented member.
--
-- * 'ucApplicationId' - Undocumented member.
--
-- * 'ucWriteCampaignRequest' - Undocumented member.
updateCampaign
    :: Text -- ^ 'ucCampaignId'
    -> Text -- ^ 'ucApplicationId'
    -> WriteCampaignRequest -- ^ 'ucWriteCampaignRequest'
    -> UpdateCampaign
updateCampaign pCampaignId_ pApplicationId_ pWriteCampaignRequest_ =
  UpdateCampaign'
    { _ucCampaignId = pCampaignId_
    , _ucApplicationId = pApplicationId_
    , _ucWriteCampaignRequest = pWriteCampaignRequest_
    }


-- | Undocumented member.
ucCampaignId :: Lens' UpdateCampaign Text
ucCampaignId = lens _ucCampaignId (\ s a -> s{_ucCampaignId = a})

-- | Undocumented member.
ucApplicationId :: Lens' UpdateCampaign Text
ucApplicationId = lens _ucApplicationId (\ s a -> s{_ucApplicationId = a})

-- | Undocumented member.
ucWriteCampaignRequest :: Lens' UpdateCampaign WriteCampaignRequest
ucWriteCampaignRequest = lens _ucWriteCampaignRequest (\ s a -> s{_ucWriteCampaignRequest = a})

instance AWSRequest UpdateCampaign where
        type Rs UpdateCampaign = UpdateCampaignResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateCampaignResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateCampaign where

instance NFData UpdateCampaign where

instance ToHeaders UpdateCampaign where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateCampaign where
        toJSON UpdateCampaign'{..}
          = object
              (catMaybes
                 [Just
                    ("WriteCampaignRequest" .= _ucWriteCampaignRequest)])

instance ToPath UpdateCampaign where
        toPath UpdateCampaign'{..}
          = mconcat
              ["/v1/apps/", toBS _ucApplicationId, "/campaigns/",
               toBS _ucCampaignId]

instance ToQuery UpdateCampaign where
        toQuery = const mempty

-- | /See:/ 'updateCampaignResponse' smart constructor.
data UpdateCampaignResponse = UpdateCampaignResponse'
  { _ucrsResponseStatus   :: !Int
  , _ucrsCampaignResponse :: !CampaignResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCampaignResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrsResponseStatus' - -- | The response status code.
--
-- * 'ucrsCampaignResponse' - Undocumented member.
updateCampaignResponse
    :: Int -- ^ 'ucrsResponseStatus'
    -> CampaignResponse -- ^ 'ucrsCampaignResponse'
    -> UpdateCampaignResponse
updateCampaignResponse pResponseStatus_ pCampaignResponse_ =
  UpdateCampaignResponse'
    { _ucrsResponseStatus = pResponseStatus_
    , _ucrsCampaignResponse = pCampaignResponse_
    }


-- | -- | The response status code.
ucrsResponseStatus :: Lens' UpdateCampaignResponse Int
ucrsResponseStatus = lens _ucrsResponseStatus (\ s a -> s{_ucrsResponseStatus = a})

-- | Undocumented member.
ucrsCampaignResponse :: Lens' UpdateCampaignResponse CampaignResponse
ucrsCampaignResponse = lens _ucrsCampaignResponse (\ s a -> s{_ucrsCampaignResponse = a})

instance NFData UpdateCampaignResponse where
