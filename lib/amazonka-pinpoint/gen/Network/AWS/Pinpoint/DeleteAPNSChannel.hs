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
-- Module      : Network.AWS.Pinpoint.DeleteAPNSChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the APNs channel for an app.
module Network.AWS.Pinpoint.DeleteAPNSChannel
    (
    -- * Creating a Request
      deleteAPNSChannel
    , DeleteAPNSChannel
    -- * Request Lenses
    , dacApplicationId

    -- * Destructuring the Response
    , deleteAPNSChannelResponse
    , DeleteAPNSChannelResponse
    -- * Response Lenses
    , dacrsResponseStatus
    , dacrsAPNSChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAPNSChannel' smart constructor.
newtype DeleteAPNSChannel = DeleteAPNSChannel'
  { _dacApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPNSChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dacApplicationId' - Undocumented member.
deleteAPNSChannel
    :: Text -- ^ 'dacApplicationId'
    -> DeleteAPNSChannel
deleteAPNSChannel pApplicationId_ =
  DeleteAPNSChannel' {_dacApplicationId = pApplicationId_}


-- | Undocumented member.
dacApplicationId :: Lens' DeleteAPNSChannel Text
dacApplicationId = lens _dacApplicationId (\ s a -> s{_dacApplicationId = a})

instance AWSRequest DeleteAPNSChannel where
        type Rs DeleteAPNSChannel = DeleteAPNSChannelResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteAPNSChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteAPNSChannel where

instance NFData DeleteAPNSChannel where

instance ToHeaders DeleteAPNSChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteAPNSChannel where
        toPath DeleteAPNSChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _dacApplicationId,
               "/channels/apns"]

instance ToQuery DeleteAPNSChannel where
        toQuery = const mempty

-- | /See:/ 'deleteAPNSChannelResponse' smart constructor.
data DeleteAPNSChannelResponse = DeleteAPNSChannelResponse'
  { _dacrsResponseStatus      :: !Int
  , _dacrsAPNSChannelResponse :: !APNSChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPNSChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dacrsResponseStatus' - -- | The response status code.
--
-- * 'dacrsAPNSChannelResponse' - Undocumented member.
deleteAPNSChannelResponse
    :: Int -- ^ 'dacrsResponseStatus'
    -> APNSChannelResponse -- ^ 'dacrsAPNSChannelResponse'
    -> DeleteAPNSChannelResponse
deleteAPNSChannelResponse pResponseStatus_ pAPNSChannelResponse_ =
  DeleteAPNSChannelResponse'
    { _dacrsResponseStatus = pResponseStatus_
    , _dacrsAPNSChannelResponse = pAPNSChannelResponse_
    }


-- | -- | The response status code.
dacrsResponseStatus :: Lens' DeleteAPNSChannelResponse Int
dacrsResponseStatus = lens _dacrsResponseStatus (\ s a -> s{_dacrsResponseStatus = a})

-- | Undocumented member.
dacrsAPNSChannelResponse :: Lens' DeleteAPNSChannelResponse APNSChannelResponse
dacrsAPNSChannelResponse = lens _dacrsAPNSChannelResponse (\ s a -> s{_dacrsAPNSChannelResponse = a})

instance NFData DeleteAPNSChannelResponse where
