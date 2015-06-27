{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DeleteRouteTable
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

-- | Deletes the specified route table. You must disassociate the route table
-- from any subnets before you can delete it. You can\'t delete the main
-- route table.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteRouteTable.html>
module Network.AWS.EC2.DeleteRouteTable
    (
    -- * Request
      DeleteRouteTable
    -- ** Request constructor
    , deleteRouteTable
    -- ** Request lenses
    , drt1DryRun
    , drt1RouteTableId

    -- * Response
    , DeleteRouteTableResponse
    -- ** Response constructor
    , deleteRouteTableResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteRouteTable' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drt1DryRun'
--
-- * 'drt1RouteTableId'
data DeleteRouteTable = DeleteRouteTable'
    { _drt1DryRun       :: Maybe Bool
    , _drt1RouteTableId :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteRouteTable' smart constructor.
deleteRouteTable :: Text -> DeleteRouteTable
deleteRouteTable pRouteTableId =
    DeleteRouteTable'
    { _drt1DryRun = Nothing
    , _drt1RouteTableId = pRouteTableId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
drt1DryRun :: Lens' DeleteRouteTable (Maybe Bool)
drt1DryRun = lens _drt1DryRun (\ s a -> s{_drt1DryRun = a});

-- | The ID of the route table.
drt1RouteTableId :: Lens' DeleteRouteTable Text
drt1RouteTableId = lens _drt1RouteTableId (\ s a -> s{_drt1RouteTableId = a});

instance AWSRequest DeleteRouteTable where
        type Sv DeleteRouteTable = EC2
        type Rs DeleteRouteTable = DeleteRouteTableResponse
        request = post
        response = receiveNull DeleteRouteTableResponse'

instance ToHeaders DeleteRouteTable where
        toHeaders = const mempty

instance ToPath DeleteRouteTable where
        toPath = const "/"

instance ToQuery DeleteRouteTable where
        toQuery DeleteRouteTable'{..}
          = mconcat
              ["Action" =: ("DeleteRouteTable" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _drt1DryRun,
               "RouteTableId" =: _drt1RouteTableId]

-- | /See:/ 'deleteRouteTableResponse' smart constructor.
data DeleteRouteTableResponse =
    DeleteRouteTableResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteRouteTableResponse' smart constructor.
deleteRouteTableResponse :: DeleteRouteTableResponse
deleteRouteTableResponse = DeleteRouteTableResponse'
