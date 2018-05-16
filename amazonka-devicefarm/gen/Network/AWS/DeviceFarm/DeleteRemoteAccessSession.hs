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
-- Module      : Network.AWS.DeviceFarm.DeleteRemoteAccessSession
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a completed remote access session and its results.
--
--
module Network.AWS.DeviceFarm.DeleteRemoteAccessSession
    (
    -- * Creating a Request
      deleteRemoteAccessSession
    , DeleteRemoteAccessSession
    -- * Request Lenses
    , drasArn

    -- * Destructuring the Response
    , deleteRemoteAccessSessionResponse
    , DeleteRemoteAccessSessionResponse
    -- * Response Lenses
    , drasrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to delete the specified remote access session.
--
--
--
-- /See:/ 'deleteRemoteAccessSession' smart constructor.
newtype DeleteRemoteAccessSession = DeleteRemoteAccessSession'
  { _drasArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRemoteAccessSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drasArn' - The Amazon Resource Name (ARN) of the sesssion for which you want to delete remote access.
deleteRemoteAccessSession
    :: Text -- ^ 'drasArn'
    -> DeleteRemoteAccessSession
deleteRemoteAccessSession pArn_ = DeleteRemoteAccessSession' {_drasArn = pArn_}


-- | The Amazon Resource Name (ARN) of the sesssion for which you want to delete remote access.
drasArn :: Lens' DeleteRemoteAccessSession Text
drasArn = lens _drasArn (\ s a -> s{_drasArn = a})

instance AWSRequest DeleteRemoteAccessSession where
        type Rs DeleteRemoteAccessSession =
             DeleteRemoteAccessSessionResponse
        request = postJSON deviceFarm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteRemoteAccessSessionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteRemoteAccessSession where

instance NFData DeleteRemoteAccessSession where

instance ToHeaders DeleteRemoteAccessSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.DeleteRemoteAccessSession" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRemoteAccessSession where
        toJSON DeleteRemoteAccessSession'{..}
          = object (catMaybes [Just ("arn" .= _drasArn)])

instance ToPath DeleteRemoteAccessSession where
        toPath = const "/"

instance ToQuery DeleteRemoteAccessSession where
        toQuery = const mempty

-- | The response from the server when a request is made to delete the remote access session.
--
--
--
-- /See:/ 'deleteRemoteAccessSessionResponse' smart constructor.
newtype DeleteRemoteAccessSessionResponse = DeleteRemoteAccessSessionResponse'
  { _drasrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRemoteAccessSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drasrsResponseStatus' - -- | The response status code.
deleteRemoteAccessSessionResponse
    :: Int -- ^ 'drasrsResponseStatus'
    -> DeleteRemoteAccessSessionResponse
deleteRemoteAccessSessionResponse pResponseStatus_ =
  DeleteRemoteAccessSessionResponse' {_drasrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drasrsResponseStatus :: Lens' DeleteRemoteAccessSessionResponse Int
drasrsResponseStatus = lens _drasrsResponseStatus (\ s a -> s{_drasrsResponseStatus = a})

instance NFData DeleteRemoteAccessSessionResponse
         where
