{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DeleteVPCPeeringConnection
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a VPC peering connection. Either the owner of the requester VPC
-- or the owner of the peer VPC can delete the VPC peering connection if
-- it\'s in the @active@ state. The owner of the requester VPC can delete a
-- VPC peering connection in the @pending-acceptance@ state.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVPCPeeringConnection.html>
module Network.AWS.EC2.DeleteVPCPeeringConnection
    (
    -- * Request
      DeleteVPCPeeringConnection
    -- ** Request constructor
    , deleteVPCPeeringConnection
    -- ** Request lenses
    , dvpcDryRun
    , dvpcVPCPeeringConnectionId

    -- * Response
    , DeleteVPCPeeringConnectionResponse
    -- ** Response constructor
    , deleteVPCPeeringConnectionResponse
    -- ** Response lenses
    , dvpcrReturn
    , dvpcrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteVPCPeeringConnection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcDryRun'
--
-- * 'dvpcVPCPeeringConnectionId'
data DeleteVPCPeeringConnection = DeleteVPCPeeringConnection'
    { _dvpcDryRun                 :: !(Maybe Bool)
    , _dvpcVPCPeeringConnectionId :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeleteVPCPeeringConnection' smart constructor.
deleteVPCPeeringConnection :: Text -> DeleteVPCPeeringConnection
deleteVPCPeeringConnection pVPCPeeringConnectionId =
    DeleteVPCPeeringConnection'
    { _dvpcDryRun = Nothing
    , _dvpcVPCPeeringConnectionId = pVPCPeeringConnectionId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvpcDryRun :: Lens' DeleteVPCPeeringConnection (Maybe Bool)
dvpcDryRun = lens _dvpcDryRun (\ s a -> s{_dvpcDryRun = a});

-- | The ID of the VPC peering connection.
dvpcVPCPeeringConnectionId :: Lens' DeleteVPCPeeringConnection Text
dvpcVPCPeeringConnectionId = lens _dvpcVPCPeeringConnectionId (\ s a -> s{_dvpcVPCPeeringConnectionId = a});

instance AWSRequest DeleteVPCPeeringConnection where
        type Sv DeleteVPCPeeringConnection = EC2
        type Rs DeleteVPCPeeringConnection =
             DeleteVPCPeeringConnectionResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DeleteVPCPeeringConnectionResponse' <$>
                   (x .@? "return") <*> (pure s))

instance ToHeaders DeleteVPCPeeringConnection where
        toHeaders = const mempty

instance ToPath DeleteVPCPeeringConnection where
        toPath = const "/"

instance ToQuery DeleteVPCPeeringConnection where
        toQuery DeleteVPCPeeringConnection'{..}
          = mconcat
              ["Action" =:
                 ("DeleteVPCPeeringConnection" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dvpcDryRun,
               "VpcPeeringConnectionId" =:
                 _dvpcVPCPeeringConnectionId]

-- | /See:/ 'deleteVPCPeeringConnectionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcrReturn'
--
-- * 'dvpcrStatus'
data DeleteVPCPeeringConnectionResponse = DeleteVPCPeeringConnectionResponse'
    { _dvpcrReturn :: !(Maybe Bool)
    , _dvpcrStatus :: !Status
    } deriving (Eq,Read,Show)

-- | 'DeleteVPCPeeringConnectionResponse' smart constructor.
deleteVPCPeeringConnectionResponse :: Status -> DeleteVPCPeeringConnectionResponse
deleteVPCPeeringConnectionResponse pStatus =
    DeleteVPCPeeringConnectionResponse'
    { _dvpcrReturn = Nothing
    , _dvpcrStatus = pStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
dvpcrReturn :: Lens' DeleteVPCPeeringConnectionResponse (Maybe Bool)
dvpcrReturn = lens _dvpcrReturn (\ s a -> s{_dvpcrReturn = a});

-- | FIXME: Undocumented member.
dvpcrStatus :: Lens' DeleteVPCPeeringConnectionResponse Status
dvpcrStatus = lens _dvpcrStatus (\ s a -> s{_dvpcrStatus = a});
