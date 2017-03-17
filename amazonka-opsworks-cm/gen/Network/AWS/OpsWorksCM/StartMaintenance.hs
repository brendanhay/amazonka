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
-- Module      : Network.AWS.OpsWorksCM.StartMaintenance
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Manually starts server maintenance. This command can be useful if an earlier maintenance attempt failed, and the underlying cause of maintenance failure has been resolved. The server is in an @UNDER_MAINTENANCE@ state while maintenance is in progress.
--
--
-- Maintenance can only be started on servers in @HEALTHY@ and @UNHEALTHY@ states. Otherwise, an @InvalidStateException@ is thrown. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
module Network.AWS.OpsWorksCM.StartMaintenance
    (
    -- * Creating a Request
      startMaintenance
    , StartMaintenance
    -- * Request Lenses
    , smServerName

    -- * Destructuring the Response
    , startMaintenanceResponse
    , StartMaintenanceResponse
    -- * Response Lenses
    , smrsServer
    , smrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.OpsWorksCM.Types
import           Network.AWS.OpsWorksCM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startMaintenance' smart constructor.
newtype StartMaintenance = StartMaintenance'
    { _smServerName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartMaintenance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smServerName' - The name of the server on which to run maintenance.
startMaintenance
    :: Text -- ^ 'smServerName'
    -> StartMaintenance
startMaintenance pServerName_ =
    StartMaintenance'
    { _smServerName = pServerName_
    }

-- | The name of the server on which to run maintenance.
smServerName :: Lens' StartMaintenance Text
smServerName = lens _smServerName (\ s a -> s{_smServerName = a});

instance AWSRequest StartMaintenance where
        type Rs StartMaintenance = StartMaintenanceResponse
        request = postJSON opsWorksCM
        response
          = receiveJSON
              (\ s h x ->
                 StartMaintenanceResponse' <$>
                   (x .?> "Server") <*> (pure (fromEnum s)))

instance Hashable StartMaintenance

instance NFData StartMaintenance

instance ToHeaders StartMaintenance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorksCM_V2016_11_01.StartMaintenance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartMaintenance where
        toJSON StartMaintenance'{..}
          = object
              (catMaybes [Just ("ServerName" .= _smServerName)])

instance ToPath StartMaintenance where
        toPath = const "/"

instance ToQuery StartMaintenance where
        toQuery = const mempty

-- | /See:/ 'startMaintenanceResponse' smart constructor.
data StartMaintenanceResponse = StartMaintenanceResponse'
    { _smrsServer         :: !(Maybe Server)
    , _smrsResponseStatus :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartMaintenanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smrsServer' - Contains the response to a @StartMaintenance@ request.
--
-- * 'smrsResponseStatus' - -- | The response status code.
startMaintenanceResponse
    :: Int -- ^ 'smrsResponseStatus'
    -> StartMaintenanceResponse
startMaintenanceResponse pResponseStatus_ =
    StartMaintenanceResponse'
    { _smrsServer = Nothing
    , _smrsResponseStatus = pResponseStatus_
    }

-- | Contains the response to a @StartMaintenance@ request.
smrsServer :: Lens' StartMaintenanceResponse (Maybe Server)
smrsServer = lens _smrsServer (\ s a -> s{_smrsServer = a});

-- | -- | The response status code.
smrsResponseStatus :: Lens' StartMaintenanceResponse Int
smrsResponseStatus = lens _smrsResponseStatus (\ s a -> s{_smrsResponseStatus = a});

instance NFData StartMaintenanceResponse
