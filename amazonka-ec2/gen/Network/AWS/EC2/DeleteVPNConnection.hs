{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPNConnection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPN connection.
--
-- If you\'re deleting the VPC and its associated components, we recommend
-- that you detach the virtual private gateway from the VPC and delete the
-- VPC before deleting the VPN connection. If you believe that the tunnel
-- credentials for your VPN connection have been compromised, you can
-- delete the VPN connection and create a new one that has new keys,
-- without needing to delete the VPC or virtual private gateway. If you
-- create a new VPN connection, you must reconfigure the customer gateway
-- using the new configuration information returned with the new VPN
-- connection ID.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVPNConnection.html>
module Network.AWS.EC2.DeleteVPNConnection
    (
    -- * Request
      DeleteVPNConnection
    -- ** Request constructor
    , deleteVPNConnection
    -- ** Request lenses
    , dvcDryRun
    , dvcVPNConnectionId

    -- * Response
    , DeleteVPNConnectionResponse
    -- ** Response constructor
    , deleteVPNConnectionResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteVPNConnection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvcDryRun'
--
-- * 'dvcVPNConnectionId'
data DeleteVPNConnection = DeleteVPNConnection'
    { _dvcDryRun          :: !(Maybe Bool)
    , _dvcVPNConnectionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVPNConnection' smart constructor.
deleteVPNConnection :: Text -> DeleteVPNConnection
deleteVPNConnection pVPNConnectionId_ =
    DeleteVPNConnection'
    { _dvcDryRun = Nothing
    , _dvcVPNConnectionId = pVPNConnectionId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvcDryRun :: Lens' DeleteVPNConnection (Maybe Bool)
dvcDryRun = lens _dvcDryRun (\ s a -> s{_dvcDryRun = a});

-- | The ID of the VPN connection.
dvcVPNConnectionId :: Lens' DeleteVPNConnection Text
dvcVPNConnectionId = lens _dvcVPNConnectionId (\ s a -> s{_dvcVPNConnectionId = a});

instance AWSRequest DeleteVPNConnection where
        type Sv DeleteVPNConnection = EC2
        type Rs DeleteVPNConnection =
             DeleteVPNConnectionResponse
        request = post "DeleteVPNConnection"
        response = receiveNull DeleteVPNConnectionResponse'

instance ToHeaders DeleteVPNConnection where
        toHeaders = const mempty

instance ToPath DeleteVPNConnection where
        toPath = const "/"

instance ToQuery DeleteVPNConnection where
        toQuery DeleteVPNConnection'{..}
          = mconcat
              ["Action" =: ("DeleteVPNConnection" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dvcDryRun,
               "VpnConnectionId" =: _dvcVPNConnectionId]

-- | /See:/ 'deleteVPNConnectionResponse' smart constructor.
data DeleteVPNConnectionResponse =
    DeleteVPNConnectionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVPNConnectionResponse' smart constructor.
deleteVPNConnectionResponse :: DeleteVPNConnectionResponse
deleteVPNConnectionResponse = DeleteVPNConnectionResponse'
