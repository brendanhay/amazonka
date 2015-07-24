{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.ConfirmConnection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Confirm the creation of a hosted connection on an interconnect.
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
    , ccrsConnectionState
    , ccrsStatus
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
confirmConnection pConnectionId_ =
    ConfirmConnection'
    { _ccConnectionId = pConnectionId_
    }

-- | FIXME: Undocumented member.
ccConnectionId :: Lens' ConfirmConnection Text
ccConnectionId = lens _ccConnectionId (\ s a -> s{_ccConnectionId = a});

instance AWSRequest ConfirmConnection where
        type Sv ConfirmConnection = DirectConnect
        type Rs ConfirmConnection = ConfirmConnectionResponse
        request = postJSON "ConfirmConnection"
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
-- * 'ccrsConnectionState'
--
-- * 'ccrsStatus'
data ConfirmConnectionResponse = ConfirmConnectionResponse'
    { _ccrsConnectionState :: !(Maybe ConnectionState)
    , _ccrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfirmConnectionResponse' smart constructor.
confirmConnectionResponse :: Int -> ConfirmConnectionResponse
confirmConnectionResponse pStatus_ =
    ConfirmConnectionResponse'
    { _ccrsConnectionState = Nothing
    , _ccrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
ccrsConnectionState :: Lens' ConfirmConnectionResponse (Maybe ConnectionState)
ccrsConnectionState = lens _ccrsConnectionState (\ s a -> s{_ccrsConnectionState = a});

-- | FIXME: Undocumented member.
ccrsStatus :: Lens' ConfirmConnectionResponse Int
ccrsStatus = lens _ccrsStatus (\ s a -> s{_ccrsStatus = a});
