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
-- Module      : Network.AWS.Organizations.DeclineHandshake
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Declines a handshake request. This sets the handshake state to @DECLINED@ and effectively deactivates the request.
--
--
-- This operation can be called only from the account that received the handshake. The originator of the handshake can use 'CancelHandshake' instead. The originator can't reactivate a declined request, but can re-initiate the process with a new handshake request.
--
-- After you decline a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that it is deleted.
--
module Network.AWS.Organizations.DeclineHandshake
    (
    -- * Creating a Request
      declineHandshake
    , DeclineHandshake
    -- * Request Lenses
    , dHandshakeId

    -- * Destructuring the Response
    , declineHandshakeResponse
    , DeclineHandshakeResponse
    -- * Response Lenses
    , drsHandshake
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'declineHandshake' smart constructor.
newtype DeclineHandshake = DeclineHandshake'
  { _dHandshakeId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeclineHandshake' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dHandshakeId' - The unique identifier (ID) of the handshake that you want to decline. You can get the ID from the 'ListHandshakesForAccount' operation. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
declineHandshake
    :: Text -- ^ 'dHandshakeId'
    -> DeclineHandshake
declineHandshake pHandshakeId_ =
  DeclineHandshake' {_dHandshakeId = pHandshakeId_}


-- | The unique identifier (ID) of the handshake that you want to decline. You can get the ID from the 'ListHandshakesForAccount' operation. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
dHandshakeId :: Lens' DeclineHandshake Text
dHandshakeId = lens _dHandshakeId (\ s a -> s{_dHandshakeId = a})

instance AWSRequest DeclineHandshake where
        type Rs DeclineHandshake = DeclineHandshakeResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 DeclineHandshakeResponse' <$>
                   (x .?> "Handshake") <*> (pure (fromEnum s)))

instance Hashable DeclineHandshake where

instance NFData DeclineHandshake where

instance ToHeaders DeclineHandshake where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DeclineHandshake" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeclineHandshake where
        toJSON DeclineHandshake'{..}
          = object
              (catMaybes [Just ("HandshakeId" .= _dHandshakeId)])

instance ToPath DeclineHandshake where
        toPath = const "/"

instance ToQuery DeclineHandshake where
        toQuery = const mempty

-- | /See:/ 'declineHandshakeResponse' smart constructor.
data DeclineHandshakeResponse = DeclineHandshakeResponse'
  { _drsHandshake      :: !(Maybe Handshake)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeclineHandshakeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsHandshake' - A structure that contains details about the declined handshake. The state is updated to show the value @DECLINED@ .
--
-- * 'drsResponseStatus' - -- | The response status code.
declineHandshakeResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeclineHandshakeResponse
declineHandshakeResponse pResponseStatus_ =
  DeclineHandshakeResponse'
    {_drsHandshake = Nothing, _drsResponseStatus = pResponseStatus_}


-- | A structure that contains details about the declined handshake. The state is updated to show the value @DECLINED@ .
drsHandshake :: Lens' DeclineHandshakeResponse (Maybe Handshake)
drsHandshake = lens _drsHandshake (\ s a -> s{_drsHandshake = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeclineHandshakeResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeclineHandshakeResponse where
