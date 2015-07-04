{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.DirectConnect.ConfirmConnection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Confirm the creation of a hosted connection on an interconnect.
--
-- Upon creation, the hosted connection is initially in the \'Ordering\'
-- state, and will remain in this state until the owner calls
-- ConfirmConnection to confirm creation of the hosted connection.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_ConfirmConnection.html>
module Network.AWS.DirectConnect.ConfirmConnection
    (
    -- * Request
      ConfirmConnection
    -- ** Request constructor
    , confirmConnection
    -- ** Request lenses
    , ccConnectionId

    -- * Response
    , ConfirmConnectionResponse
    -- ** Response constructor
    , confirmConnectionResponse
    -- ** Response lenses
    , ccrConnectionState
    , ccrStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the ConfirmConnection operation.
--
-- /See:/ 'confirmConnection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccConnectionId'
newtype ConfirmConnection = ConfirmConnection'
    { _ccConnectionId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfirmConnection' smart constructor.
confirmConnection :: Text -> ConfirmConnection
confirmConnection pConnectionId =
    ConfirmConnection'
    { _ccConnectionId = pConnectionId
    }

-- | FIXME: Undocumented member.
ccConnectionId :: Lens' ConfirmConnection Text
ccConnectionId = lens _ccConnectionId (\ s a -> s{_ccConnectionId = a});

instance AWSRequest ConfirmConnection where
        type Sv ConfirmConnection = DirectConnect
        type Rs ConfirmConnection = ConfirmConnectionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ConfirmConnectionResponse' <$>
                   (x .?> "connectionState") <*> (pure (fromEnum s)))

instance ToHeaders ConfirmConnection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.ConfirmConnection" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ConfirmConnection where
        toJSON ConfirmConnection'{..}
          = object ["connectionId" .= _ccConnectionId]

instance ToPath ConfirmConnection where
        toPath = const "/"

instance ToQuery ConfirmConnection where
        toQuery = const mempty

-- | The response received when ConfirmConnection is called.
--
-- /See:/ 'confirmConnectionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrConnectionState'
--
-- * 'ccrStatus'
data ConfirmConnectionResponse = ConfirmConnectionResponse'
    { _ccrConnectionState :: !(Maybe ConnectionState)
    , _ccrStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfirmConnectionResponse' smart constructor.
confirmConnectionResponse :: Int -> ConfirmConnectionResponse
confirmConnectionResponse pStatus =
    ConfirmConnectionResponse'
    { _ccrConnectionState = Nothing
    , _ccrStatus = pStatus
    }

-- | FIXME: Undocumented member.
ccrConnectionState :: Lens' ConfirmConnectionResponse (Maybe ConnectionState)
ccrConnectionState = lens _ccrConnectionState (\ s a -> s{_ccrConnectionState = a});

-- | FIXME: Undocumented member.
ccrStatus :: Lens' ConfirmConnectionResponse Int
ccrStatus = lens _ccrStatus (\ s a -> s{_ccrStatus = a});
