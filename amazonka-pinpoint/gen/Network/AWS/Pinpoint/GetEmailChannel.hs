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
-- Module      : Network.AWS.Pinpoint.GetEmailChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get an email channel
module Network.AWS.Pinpoint.GetEmailChannel
    (
    -- * Creating a Request
      getEmailChannel
    , GetEmailChannel
    -- * Request Lenses
    , gecApplicationId

    -- * Destructuring the Response
    , getEmailChannelResponse
    , GetEmailChannelResponse
    -- * Response Lenses
    , gecrsResponseStatus
    , gecrsEmailChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getEmailChannel' smart constructor.
newtype GetEmailChannel = GetEmailChannel'
  { _gecApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEmailChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gecApplicationId' - Undocumented member.
getEmailChannel
    :: Text -- ^ 'gecApplicationId'
    -> GetEmailChannel
getEmailChannel pApplicationId_ =
  GetEmailChannel' {_gecApplicationId = pApplicationId_}


-- | Undocumented member.
gecApplicationId :: Lens' GetEmailChannel Text
gecApplicationId = lens _gecApplicationId (\ s a -> s{_gecApplicationId = a})

instance AWSRequest GetEmailChannel where
        type Rs GetEmailChannel = GetEmailChannelResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetEmailChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetEmailChannel where

instance NFData GetEmailChannel where

instance ToHeaders GetEmailChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetEmailChannel where
        toPath GetEmailChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _gecApplicationId,
               "/channels/email"]

instance ToQuery GetEmailChannel where
        toQuery = const mempty

-- | /See:/ 'getEmailChannelResponse' smart constructor.
data GetEmailChannelResponse = GetEmailChannelResponse'
  { _gecrsResponseStatus       :: !Int
  , _gecrsEmailChannelResponse :: !EmailChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEmailChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gecrsResponseStatus' - -- | The response status code.
--
-- * 'gecrsEmailChannelResponse' - Undocumented member.
getEmailChannelResponse
    :: Int -- ^ 'gecrsResponseStatus'
    -> EmailChannelResponse -- ^ 'gecrsEmailChannelResponse'
    -> GetEmailChannelResponse
getEmailChannelResponse pResponseStatus_ pEmailChannelResponse_ =
  GetEmailChannelResponse'
    { _gecrsResponseStatus = pResponseStatus_
    , _gecrsEmailChannelResponse = pEmailChannelResponse_
    }


-- | -- | The response status code.
gecrsResponseStatus :: Lens' GetEmailChannelResponse Int
gecrsResponseStatus = lens _gecrsResponseStatus (\ s a -> s{_gecrsResponseStatus = a})

-- | Undocumented member.
gecrsEmailChannelResponse :: Lens' GetEmailChannelResponse EmailChannelResponse
gecrsEmailChannelResponse = lens _gecrsEmailChannelResponse (\ s a -> s{_gecrsEmailChannelResponse = a})

instance NFData GetEmailChannelResponse where
