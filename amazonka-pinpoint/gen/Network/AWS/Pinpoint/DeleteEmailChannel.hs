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
-- Module      : Network.AWS.Pinpoint.DeleteEmailChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an email channel
module Network.AWS.Pinpoint.DeleteEmailChannel
    (
    -- * Creating a Request
      deleteEmailChannel
    , DeleteEmailChannel
    -- * Request Lenses
    , decApplicationId

    -- * Destructuring the Response
    , deleteEmailChannelResponse
    , DeleteEmailChannelResponse
    -- * Response Lenses
    , decrsResponseStatus
    , decrsEmailChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEmailChannel' smart constructor.
newtype DeleteEmailChannel = DeleteEmailChannel'
  { _decApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEmailChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decApplicationId' - Undocumented member.
deleteEmailChannel
    :: Text -- ^ 'decApplicationId'
    -> DeleteEmailChannel
deleteEmailChannel pApplicationId_ =
  DeleteEmailChannel' {_decApplicationId = pApplicationId_}


-- | Undocumented member.
decApplicationId :: Lens' DeleteEmailChannel Text
decApplicationId = lens _decApplicationId (\ s a -> s{_decApplicationId = a})

instance AWSRequest DeleteEmailChannel where
        type Rs DeleteEmailChannel =
             DeleteEmailChannelResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteEmailChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteEmailChannel where

instance NFData DeleteEmailChannel where

instance ToHeaders DeleteEmailChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteEmailChannel where
        toPath DeleteEmailChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _decApplicationId,
               "/channels/email"]

instance ToQuery DeleteEmailChannel where
        toQuery = const mempty

-- | /See:/ 'deleteEmailChannelResponse' smart constructor.
data DeleteEmailChannelResponse = DeleteEmailChannelResponse'
  { _decrsResponseStatus       :: !Int
  , _decrsEmailChannelResponse :: !EmailChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEmailChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decrsResponseStatus' - -- | The response status code.
--
-- * 'decrsEmailChannelResponse' - Undocumented member.
deleteEmailChannelResponse
    :: Int -- ^ 'decrsResponseStatus'
    -> EmailChannelResponse -- ^ 'decrsEmailChannelResponse'
    -> DeleteEmailChannelResponse
deleteEmailChannelResponse pResponseStatus_ pEmailChannelResponse_ =
  DeleteEmailChannelResponse'
    { _decrsResponseStatus = pResponseStatus_
    , _decrsEmailChannelResponse = pEmailChannelResponse_
    }


-- | -- | The response status code.
decrsResponseStatus :: Lens' DeleteEmailChannelResponse Int
decrsResponseStatus = lens _decrsResponseStatus (\ s a -> s{_decrsResponseStatus = a})

-- | Undocumented member.
decrsEmailChannelResponse :: Lens' DeleteEmailChannelResponse EmailChannelResponse
decrsEmailChannelResponse = lens _decrsEmailChannelResponse (\ s a -> s{_decrsEmailChannelResponse = a})

instance NFData DeleteEmailChannelResponse where
