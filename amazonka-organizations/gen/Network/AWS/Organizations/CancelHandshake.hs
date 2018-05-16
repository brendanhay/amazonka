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
-- Module      : Network.AWS.Organizations.CancelHandshake
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a handshake. Canceling a handshake sets the handshake state to @CANCELED@ .
--
--
-- This operation can be called only from the account that originated the handshake. The recipient of the handshake can't cancel it, but can use 'DeclineHandshake' instead. After a handshake is canceled, the recipient can no longer respond to that handshake.
--
-- After you cancel a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that it is deleted.
--
module Network.AWS.Organizations.CancelHandshake
    (
    -- * Creating a Request
      cancelHandshake
    , CancelHandshake
    -- * Request Lenses
    , chHandshakeId

    -- * Destructuring the Response
    , cancelHandshakeResponse
    , CancelHandshakeResponse
    -- * Response Lenses
    , chrsHandshake
    , chrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelHandshake' smart constructor.
newtype CancelHandshake = CancelHandshake'
  { _chHandshakeId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelHandshake' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chHandshakeId' - The unique identifier (ID) of the handshake that you want to cancel. You can get the ID from the 'ListHandshakesForOrganization' operation. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
cancelHandshake
    :: Text -- ^ 'chHandshakeId'
    -> CancelHandshake
cancelHandshake pHandshakeId_ =
  CancelHandshake' {_chHandshakeId = pHandshakeId_}


-- | The unique identifier (ID) of the handshake that you want to cancel. You can get the ID from the 'ListHandshakesForOrganization' operation. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
chHandshakeId :: Lens' CancelHandshake Text
chHandshakeId = lens _chHandshakeId (\ s a -> s{_chHandshakeId = a})

instance AWSRequest CancelHandshake where
        type Rs CancelHandshake = CancelHandshakeResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 CancelHandshakeResponse' <$>
                   (x .?> "Handshake") <*> (pure (fromEnum s)))

instance Hashable CancelHandshake where

instance NFData CancelHandshake where

instance ToHeaders CancelHandshake where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.CancelHandshake" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelHandshake where
        toJSON CancelHandshake'{..}
          = object
              (catMaybes [Just ("HandshakeId" .= _chHandshakeId)])

instance ToPath CancelHandshake where
        toPath = const "/"

instance ToQuery CancelHandshake where
        toQuery = const mempty

-- | /See:/ 'cancelHandshakeResponse' smart constructor.
data CancelHandshakeResponse = CancelHandshakeResponse'
  { _chrsHandshake      :: !(Maybe Handshake)
  , _chrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelHandshakeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chrsHandshake' - A structure that contains details about the handshake that you canceled.
--
-- * 'chrsResponseStatus' - -- | The response status code.
cancelHandshakeResponse
    :: Int -- ^ 'chrsResponseStatus'
    -> CancelHandshakeResponse
cancelHandshakeResponse pResponseStatus_ =
  CancelHandshakeResponse'
    {_chrsHandshake = Nothing, _chrsResponseStatus = pResponseStatus_}


-- | A structure that contains details about the handshake that you canceled.
chrsHandshake :: Lens' CancelHandshakeResponse (Maybe Handshake)
chrsHandshake = lens _chrsHandshake (\ s a -> s{_chrsHandshake = a})

-- | -- | The response status code.
chrsResponseStatus :: Lens' CancelHandshakeResponse Int
chrsResponseStatus = lens _chrsResponseStatus (\ s a -> s{_chrsResponseStatus = a})

instance NFData CancelHandshakeResponse where
