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
-- Module      : Network.AWS.DirectConnect.ConfirmConnection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirm the creation of a hosted connection on an interconnect.
--
-- Upon creation, the hosted connection is initially in the \'Ordering\'
-- state, and will remain in this state until the owner calls
-- ConfirmConnection to confirm creation of the hosted connection.
--
-- /See:/ <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_ConfirmConnection.html AWS API Reference> for ConfirmConnection.
module Network.AWS.DirectConnect.ConfirmConnection
    (
    -- * Creating a Request
      confirmConnection
    , ConfirmConnection
    -- * Request Lenses
    , ccConnectionId

    -- * Destructuring the Response
    , confirmConnectionResponse
    , ConfirmConnectionResponse
    -- * Response Lenses
    , ccrsConnectionState
    , ccrsStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the ConfirmConnection operation.
--
-- /See:/ 'confirmConnection' smart constructor.
newtype ConfirmConnection = ConfirmConnection'
    { _ccConnectionId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfirmConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccConnectionId'
confirmConnection
    :: Text -- ^ 'ccConnectionId'
    -> ConfirmConnection
confirmConnection pConnectionId_ =
    ConfirmConnection'
    { _ccConnectionId = pConnectionId_
    }

-- | Undocumented member.
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
data ConfirmConnectionResponse = ConfirmConnectionResponse'
    { _ccrsConnectionState :: !(Maybe ConnectionState)
    , _ccrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfirmConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsConnectionState'
--
-- * 'ccrsStatus'
confirmConnectionResponse
    :: Int -- ^ 'ccrsStatus'
    -> ConfirmConnectionResponse
confirmConnectionResponse pStatus_ =
    ConfirmConnectionResponse'
    { _ccrsConnectionState = Nothing
    , _ccrsStatus = pStatus_
    }

-- | Undocumented member.
ccrsConnectionState :: Lens' ConfirmConnectionResponse (Maybe ConnectionState)
ccrsConnectionState = lens _ccrsConnectionState (\ s a -> s{_ccrsConnectionState = a});

-- | The response status code.
ccrsStatus :: Lens' ConfirmConnectionResponse Int
ccrsStatus = lens _ccrsStatus (\ s a -> s{_ccrsStatus = a});
