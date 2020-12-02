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
-- Module      : Network.AWS.Pinpoint.DeleteAPNSVoipSandboxChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an APNS VoIP sandbox channel
module Network.AWS.Pinpoint.DeleteAPNSVoipSandboxChannel
    (
    -- * Creating a Request
      deleteAPNSVoipSandboxChannel
    , DeleteAPNSVoipSandboxChannel
    -- * Request Lenses
    , davscApplicationId

    -- * Destructuring the Response
    , deleteAPNSVoipSandboxChannelResponse
    , DeleteAPNSVoipSandboxChannelResponse
    -- * Response Lenses
    , davscrsResponseStatus
    , davscrsAPNSVoipSandboxChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAPNSVoipSandboxChannel' smart constructor.
newtype DeleteAPNSVoipSandboxChannel = DeleteAPNSVoipSandboxChannel'
  { _davscApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPNSVoipSandboxChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'davscApplicationId' - Undocumented member.
deleteAPNSVoipSandboxChannel
    :: Text -- ^ 'davscApplicationId'
    -> DeleteAPNSVoipSandboxChannel
deleteAPNSVoipSandboxChannel pApplicationId_ =
  DeleteAPNSVoipSandboxChannel' {_davscApplicationId = pApplicationId_}


-- | Undocumented member.
davscApplicationId :: Lens' DeleteAPNSVoipSandboxChannel Text
davscApplicationId = lens _davscApplicationId (\ s a -> s{_davscApplicationId = a})

instance AWSRequest DeleteAPNSVoipSandboxChannel
         where
        type Rs DeleteAPNSVoipSandboxChannel =
             DeleteAPNSVoipSandboxChannelResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteAPNSVoipSandboxChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteAPNSVoipSandboxChannel where

instance NFData DeleteAPNSVoipSandboxChannel where

instance ToHeaders DeleteAPNSVoipSandboxChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteAPNSVoipSandboxChannel where
        toPath DeleteAPNSVoipSandboxChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _davscApplicationId,
               "/channels/apns_voip_sandbox"]

instance ToQuery DeleteAPNSVoipSandboxChannel where
        toQuery = const mempty

-- | /See:/ 'deleteAPNSVoipSandboxChannelResponse' smart constructor.
data DeleteAPNSVoipSandboxChannelResponse = DeleteAPNSVoipSandboxChannelResponse'
  { _davscrsResponseStatus                 :: !Int
  , _davscrsAPNSVoipSandboxChannelResponse :: !APNSVoipSandboxChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPNSVoipSandboxChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'davscrsResponseStatus' - -- | The response status code.
--
-- * 'davscrsAPNSVoipSandboxChannelResponse' - Undocumented member.
deleteAPNSVoipSandboxChannelResponse
    :: Int -- ^ 'davscrsResponseStatus'
    -> APNSVoipSandboxChannelResponse -- ^ 'davscrsAPNSVoipSandboxChannelResponse'
    -> DeleteAPNSVoipSandboxChannelResponse
deleteAPNSVoipSandboxChannelResponse pResponseStatus_ pAPNSVoipSandboxChannelResponse_ =
  DeleteAPNSVoipSandboxChannelResponse'
    { _davscrsResponseStatus = pResponseStatus_
    , _davscrsAPNSVoipSandboxChannelResponse = pAPNSVoipSandboxChannelResponse_
    }


-- | -- | The response status code.
davscrsResponseStatus :: Lens' DeleteAPNSVoipSandboxChannelResponse Int
davscrsResponseStatus = lens _davscrsResponseStatus (\ s a -> s{_davscrsResponseStatus = a})

-- | Undocumented member.
davscrsAPNSVoipSandboxChannelResponse :: Lens' DeleteAPNSVoipSandboxChannelResponse APNSVoipSandboxChannelResponse
davscrsAPNSVoipSandboxChannelResponse = lens _davscrsAPNSVoipSandboxChannelResponse (\ s a -> s{_davscrsAPNSVoipSandboxChannelResponse = a})

instance NFData DeleteAPNSVoipSandboxChannelResponse
         where
