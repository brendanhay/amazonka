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
-- Module      : Network.AWS.Pinpoint.DeleteSmsChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an SMS channel
module Network.AWS.Pinpoint.DeleteSmsChannel
    (
    -- * Creating a Request
      deleteSmsChannel
    , DeleteSmsChannel
    -- * Request Lenses
    , dscApplicationId

    -- * Destructuring the Response
    , deleteSmsChannelResponse
    , DeleteSmsChannelResponse
    -- * Response Lenses
    , dscrsResponseStatus
    , dscrsSMSChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSmsChannel' smart constructor.
newtype DeleteSmsChannel = DeleteSmsChannel'
  { _dscApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSmsChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscApplicationId' - Undocumented member.
deleteSmsChannel
    :: Text -- ^ 'dscApplicationId'
    -> DeleteSmsChannel
deleteSmsChannel pApplicationId_ =
  DeleteSmsChannel' {_dscApplicationId = pApplicationId_}


-- | Undocumented member.
dscApplicationId :: Lens' DeleteSmsChannel Text
dscApplicationId = lens _dscApplicationId (\ s a -> s{_dscApplicationId = a})

instance AWSRequest DeleteSmsChannel where
        type Rs DeleteSmsChannel = DeleteSmsChannelResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteSmsChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteSmsChannel where

instance NFData DeleteSmsChannel where

instance ToHeaders DeleteSmsChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteSmsChannel where
        toPath DeleteSmsChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _dscApplicationId,
               "/channels/sms"]

instance ToQuery DeleteSmsChannel where
        toQuery = const mempty

-- | /See:/ 'deleteSmsChannelResponse' smart constructor.
data DeleteSmsChannelResponse = DeleteSmsChannelResponse'
  { _dscrsResponseStatus     :: !Int
  , _dscrsSMSChannelResponse :: !SMSChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSmsChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrsResponseStatus' - -- | The response status code.
--
-- * 'dscrsSMSChannelResponse' - Undocumented member.
deleteSmsChannelResponse
    :: Int -- ^ 'dscrsResponseStatus'
    -> SMSChannelResponse -- ^ 'dscrsSMSChannelResponse'
    -> DeleteSmsChannelResponse
deleteSmsChannelResponse pResponseStatus_ pSMSChannelResponse_ =
  DeleteSmsChannelResponse'
    { _dscrsResponseStatus = pResponseStatus_
    , _dscrsSMSChannelResponse = pSMSChannelResponse_
    }


-- | -- | The response status code.
dscrsResponseStatus :: Lens' DeleteSmsChannelResponse Int
dscrsResponseStatus = lens _dscrsResponseStatus (\ s a -> s{_dscrsResponseStatus = a})

-- | Undocumented member.
dscrsSMSChannelResponse :: Lens' DeleteSmsChannelResponse SMSChannelResponse
dscrsSMSChannelResponse = lens _dscrsSMSChannelResponse (\ s a -> s{_dscrsSMSChannelResponse = a})

instance NFData DeleteSmsChannelResponse where
