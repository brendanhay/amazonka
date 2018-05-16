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
-- Module      : Network.AWS.Pinpoint.DeleteADMChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an ADM channel
module Network.AWS.Pinpoint.DeleteADMChannel
    (
    -- * Creating a Request
      deleteADMChannel
    , DeleteADMChannel
    -- * Request Lenses
    , dadmcApplicationId

    -- * Destructuring the Response
    , deleteADMChannelResponse
    , DeleteADMChannelResponse
    -- * Response Lenses
    , dadmcrsResponseStatus
    , dadmcrsADMChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteADMChannel' smart constructor.
newtype DeleteADMChannel = DeleteADMChannel'
  { _dadmcApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteADMChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dadmcApplicationId' - Undocumented member.
deleteADMChannel
    :: Text -- ^ 'dadmcApplicationId'
    -> DeleteADMChannel
deleteADMChannel pApplicationId_ =
  DeleteADMChannel' {_dadmcApplicationId = pApplicationId_}


-- | Undocumented member.
dadmcApplicationId :: Lens' DeleteADMChannel Text
dadmcApplicationId = lens _dadmcApplicationId (\ s a -> s{_dadmcApplicationId = a})

instance AWSRequest DeleteADMChannel where
        type Rs DeleteADMChannel = DeleteADMChannelResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteADMChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteADMChannel where

instance NFData DeleteADMChannel where

instance ToHeaders DeleteADMChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteADMChannel where
        toPath DeleteADMChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _dadmcApplicationId,
               "/channels/adm"]

instance ToQuery DeleteADMChannel where
        toQuery = const mempty

-- | /See:/ 'deleteADMChannelResponse' smart constructor.
data DeleteADMChannelResponse = DeleteADMChannelResponse'
  { _dadmcrsResponseStatus     :: !Int
  , _dadmcrsADMChannelResponse :: !ADMChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteADMChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dadmcrsResponseStatus' - -- | The response status code.
--
-- * 'dadmcrsADMChannelResponse' - Undocumented member.
deleteADMChannelResponse
    :: Int -- ^ 'dadmcrsResponseStatus'
    -> ADMChannelResponse -- ^ 'dadmcrsADMChannelResponse'
    -> DeleteADMChannelResponse
deleteADMChannelResponse pResponseStatus_ pADMChannelResponse_ =
  DeleteADMChannelResponse'
    { _dadmcrsResponseStatus = pResponseStatus_
    , _dadmcrsADMChannelResponse = pADMChannelResponse_
    }


-- | -- | The response status code.
dadmcrsResponseStatus :: Lens' DeleteADMChannelResponse Int
dadmcrsResponseStatus = lens _dadmcrsResponseStatus (\ s a -> s{_dadmcrsResponseStatus = a})

-- | Undocumented member.
dadmcrsADMChannelResponse :: Lens' DeleteADMChannelResponse ADMChannelResponse
dadmcrsADMChannelResponse = lens _dadmcrsADMChannelResponse (\ s a -> s{_dadmcrsADMChannelResponse = a})

instance NFData DeleteADMChannelResponse where
