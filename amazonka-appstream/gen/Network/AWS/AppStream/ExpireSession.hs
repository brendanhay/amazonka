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
-- Module      : Network.AWS.AppStream.ExpireSession
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified streaming session.
--
--
module Network.AWS.AppStream.ExpireSession
    (
    -- * Creating a Request
      expireSession
    , ExpireSession
    -- * Request Lenses
    , esSessionId

    -- * Destructuring the Response
    , expireSessionResponse
    , ExpireSessionResponse
    -- * Response Lenses
    , esrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'expireSession' smart constructor.
newtype ExpireSession = ExpireSession'
  { _esSessionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExpireSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esSessionId' - The ID of the streaming session.
expireSession
    :: Text -- ^ 'esSessionId'
    -> ExpireSession
expireSession pSessionId_ = ExpireSession' {_esSessionId = pSessionId_}


-- | The ID of the streaming session.
esSessionId :: Lens' ExpireSession Text
esSessionId = lens _esSessionId (\ s a -> s{_esSessionId = a})

instance AWSRequest ExpireSession where
        type Rs ExpireSession = ExpireSessionResponse
        request = postJSON appStream
        response
          = receiveEmpty
              (\ s h x ->
                 ExpireSessionResponse' <$> (pure (fromEnum s)))

instance Hashable ExpireSession where

instance NFData ExpireSession where

instance ToHeaders ExpireSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.ExpireSession" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ExpireSession where
        toJSON ExpireSession'{..}
          = object
              (catMaybes [Just ("SessionId" .= _esSessionId)])

instance ToPath ExpireSession where
        toPath = const "/"

instance ToQuery ExpireSession where
        toQuery = const mempty

-- | /See:/ 'expireSessionResponse' smart constructor.
newtype ExpireSessionResponse = ExpireSessionResponse'
  { _esrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExpireSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esrsResponseStatus' - -- | The response status code.
expireSessionResponse
    :: Int -- ^ 'esrsResponseStatus'
    -> ExpireSessionResponse
expireSessionResponse pResponseStatus_ =
  ExpireSessionResponse' {_esrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
esrsResponseStatus :: Lens' ExpireSessionResponse Int
esrsResponseStatus = lens _esrsResponseStatus (\ s a -> s{_esrsResponseStatus = a})

instance NFData ExpireSessionResponse where
