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
-- Module      : Network.AWS.Pinpoint.DeleteVoiceChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an Voice channel
module Network.AWS.Pinpoint.DeleteVoiceChannel
    (
    -- * Creating a Request
      deleteVoiceChannel
    , DeleteVoiceChannel
    -- * Request Lenses
    , dvcApplicationId

    -- * Destructuring the Response
    , deleteVoiceChannelResponse
    , DeleteVoiceChannelResponse
    -- * Response Lenses
    , dvcrsResponseStatus
    , dvcrsVoiceChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteVoiceChannel' smart constructor.
newtype DeleteVoiceChannel = DeleteVoiceChannel'
  { _dvcApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVoiceChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvcApplicationId' - The unique ID of your Amazon Pinpoint application.
deleteVoiceChannel
    :: Text -- ^ 'dvcApplicationId'
    -> DeleteVoiceChannel
deleteVoiceChannel pApplicationId_ =
  DeleteVoiceChannel' {_dvcApplicationId = pApplicationId_}


-- | The unique ID of your Amazon Pinpoint application.
dvcApplicationId :: Lens' DeleteVoiceChannel Text
dvcApplicationId = lens _dvcApplicationId (\ s a -> s{_dvcApplicationId = a})

instance AWSRequest DeleteVoiceChannel where
        type Rs DeleteVoiceChannel =
             DeleteVoiceChannelResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteVoiceChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteVoiceChannel where

instance NFData DeleteVoiceChannel where

instance ToHeaders DeleteVoiceChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteVoiceChannel where
        toPath DeleteVoiceChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _dvcApplicationId,
               "/channels/voice"]

instance ToQuery DeleteVoiceChannel where
        toQuery = const mempty

-- | /See:/ 'deleteVoiceChannelResponse' smart constructor.
data DeleteVoiceChannelResponse = DeleteVoiceChannelResponse'
  { _dvcrsResponseStatus       :: !Int
  , _dvcrsVoiceChannelResponse :: !VoiceChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVoiceChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvcrsResponseStatus' - -- | The response status code.
--
-- * 'dvcrsVoiceChannelResponse' - Undocumented member.
deleteVoiceChannelResponse
    :: Int -- ^ 'dvcrsResponseStatus'
    -> VoiceChannelResponse -- ^ 'dvcrsVoiceChannelResponse'
    -> DeleteVoiceChannelResponse
deleteVoiceChannelResponse pResponseStatus_ pVoiceChannelResponse_ =
  DeleteVoiceChannelResponse'
    { _dvcrsResponseStatus = pResponseStatus_
    , _dvcrsVoiceChannelResponse = pVoiceChannelResponse_
    }


-- | -- | The response status code.
dvcrsResponseStatus :: Lens' DeleteVoiceChannelResponse Int
dvcrsResponseStatus = lens _dvcrsResponseStatus (\ s a -> s{_dvcrsResponseStatus = a})

-- | Undocumented member.
dvcrsVoiceChannelResponse :: Lens' DeleteVoiceChannelResponse VoiceChannelResponse
dvcrsVoiceChannelResponse = lens _dvcrsVoiceChannelResponse (\ s a -> s{_dvcrsVoiceChannelResponse = a})

instance NFData DeleteVoiceChannelResponse where
