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
-- Module      : Network.AWS.Pinpoint.UpdateADMChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an ADM channel
module Network.AWS.Pinpoint.UpdateADMChannel
    (
    -- * Creating a Request
      updateADMChannel
    , UpdateADMChannel
    -- * Request Lenses
    , uadmcApplicationId
    , uadmcADMChannelRequest

    -- * Destructuring the Response
    , updateADMChannelResponse
    , UpdateADMChannelResponse
    -- * Response Lenses
    , uadmcrsResponseStatus
    , uadmcrsADMChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateADMChannel' smart constructor.
data UpdateADMChannel = UpdateADMChannel'
  { _uadmcApplicationId     :: !Text
  , _uadmcADMChannelRequest :: !ADMChannelRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateADMChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uadmcApplicationId' - Undocumented member.
--
-- * 'uadmcADMChannelRequest' - Undocumented member.
updateADMChannel
    :: Text -- ^ 'uadmcApplicationId'
    -> ADMChannelRequest -- ^ 'uadmcADMChannelRequest'
    -> UpdateADMChannel
updateADMChannel pApplicationId_ pADMChannelRequest_ =
  UpdateADMChannel'
    { _uadmcApplicationId = pApplicationId_
    , _uadmcADMChannelRequest = pADMChannelRequest_
    }


-- | Undocumented member.
uadmcApplicationId :: Lens' UpdateADMChannel Text
uadmcApplicationId = lens _uadmcApplicationId (\ s a -> s{_uadmcApplicationId = a})

-- | Undocumented member.
uadmcADMChannelRequest :: Lens' UpdateADMChannel ADMChannelRequest
uadmcADMChannelRequest = lens _uadmcADMChannelRequest (\ s a -> s{_uadmcADMChannelRequest = a})

instance AWSRequest UpdateADMChannel where
        type Rs UpdateADMChannel = UpdateADMChannelResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateADMChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateADMChannel where

instance NFData UpdateADMChannel where

instance ToHeaders UpdateADMChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateADMChannel where
        toJSON UpdateADMChannel'{..}
          = object
              (catMaybes
                 [Just
                    ("ADMChannelRequest" .= _uadmcADMChannelRequest)])

instance ToPath UpdateADMChannel where
        toPath UpdateADMChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _uadmcApplicationId,
               "/channels/adm"]

instance ToQuery UpdateADMChannel where
        toQuery = const mempty

-- | /See:/ 'updateADMChannelResponse' smart constructor.
data UpdateADMChannelResponse = UpdateADMChannelResponse'
  { _uadmcrsResponseStatus     :: !Int
  , _uadmcrsADMChannelResponse :: !ADMChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateADMChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uadmcrsResponseStatus' - -- | The response status code.
--
-- * 'uadmcrsADMChannelResponse' - Undocumented member.
updateADMChannelResponse
    :: Int -- ^ 'uadmcrsResponseStatus'
    -> ADMChannelResponse -- ^ 'uadmcrsADMChannelResponse'
    -> UpdateADMChannelResponse
updateADMChannelResponse pResponseStatus_ pADMChannelResponse_ =
  UpdateADMChannelResponse'
    { _uadmcrsResponseStatus = pResponseStatus_
    , _uadmcrsADMChannelResponse = pADMChannelResponse_
    }


-- | -- | The response status code.
uadmcrsResponseStatus :: Lens' UpdateADMChannelResponse Int
uadmcrsResponseStatus = lens _uadmcrsResponseStatus (\ s a -> s{_uadmcrsResponseStatus = a})

-- | Undocumented member.
uadmcrsADMChannelResponse :: Lens' UpdateADMChannelResponse ADMChannelResponse
uadmcrsADMChannelResponse = lens _uadmcrsADMChannelResponse (\ s a -> s{_uadmcrsADMChannelResponse = a})

instance NFData UpdateADMChannelResponse where
