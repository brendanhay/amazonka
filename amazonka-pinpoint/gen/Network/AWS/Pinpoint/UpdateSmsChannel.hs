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
-- Module      : Network.AWS.Pinpoint.UpdateSmsChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an SMS channel
module Network.AWS.Pinpoint.UpdateSmsChannel
    (
    -- * Creating a Request
      updateSmsChannel
    , UpdateSmsChannel
    -- * Request Lenses
    , uscApplicationId
    , uscSMSChannelRequest

    -- * Destructuring the Response
    , updateSmsChannelResponse
    , UpdateSmsChannelResponse
    -- * Response Lenses
    , uscrsResponseStatus
    , uscrsSMSChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSmsChannel' smart constructor.
data UpdateSmsChannel = UpdateSmsChannel'
  { _uscApplicationId     :: !Text
  , _uscSMSChannelRequest :: !SMSChannelRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSmsChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscApplicationId' - Undocumented member.
--
-- * 'uscSMSChannelRequest' - Undocumented member.
updateSmsChannel
    :: Text -- ^ 'uscApplicationId'
    -> SMSChannelRequest -- ^ 'uscSMSChannelRequest'
    -> UpdateSmsChannel
updateSmsChannel pApplicationId_ pSMSChannelRequest_ =
  UpdateSmsChannel'
    { _uscApplicationId = pApplicationId_
    , _uscSMSChannelRequest = pSMSChannelRequest_
    }


-- | Undocumented member.
uscApplicationId :: Lens' UpdateSmsChannel Text
uscApplicationId = lens _uscApplicationId (\ s a -> s{_uscApplicationId = a})

-- | Undocumented member.
uscSMSChannelRequest :: Lens' UpdateSmsChannel SMSChannelRequest
uscSMSChannelRequest = lens _uscSMSChannelRequest (\ s a -> s{_uscSMSChannelRequest = a})

instance AWSRequest UpdateSmsChannel where
        type Rs UpdateSmsChannel = UpdateSmsChannelResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateSmsChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateSmsChannel where

instance NFData UpdateSmsChannel where

instance ToHeaders UpdateSmsChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateSmsChannel where
        toJSON UpdateSmsChannel'{..}
          = object
              (catMaybes
                 [Just
                    ("SMSChannelRequest" .= _uscSMSChannelRequest)])

instance ToPath UpdateSmsChannel where
        toPath UpdateSmsChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _uscApplicationId,
               "/channels/sms"]

instance ToQuery UpdateSmsChannel where
        toQuery = const mempty

-- | /See:/ 'updateSmsChannelResponse' smart constructor.
data UpdateSmsChannelResponse = UpdateSmsChannelResponse'
  { _uscrsResponseStatus     :: !Int
  , _uscrsSMSChannelResponse :: !SMSChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSmsChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscrsResponseStatus' - -- | The response status code.
--
-- * 'uscrsSMSChannelResponse' - Undocumented member.
updateSmsChannelResponse
    :: Int -- ^ 'uscrsResponseStatus'
    -> SMSChannelResponse -- ^ 'uscrsSMSChannelResponse'
    -> UpdateSmsChannelResponse
updateSmsChannelResponse pResponseStatus_ pSMSChannelResponse_ =
  UpdateSmsChannelResponse'
    { _uscrsResponseStatus = pResponseStatus_
    , _uscrsSMSChannelResponse = pSMSChannelResponse_
    }


-- | -- | The response status code.
uscrsResponseStatus :: Lens' UpdateSmsChannelResponse Int
uscrsResponseStatus = lens _uscrsResponseStatus (\ s a -> s{_uscrsResponseStatus = a})

-- | Undocumented member.
uscrsSMSChannelResponse :: Lens' UpdateSmsChannelResponse SMSChannelResponse
uscrsSMSChannelResponse = lens _uscrsSMSChannelResponse (\ s a -> s{_uscrsSMSChannelResponse = a})

instance NFData UpdateSmsChannelResponse where
