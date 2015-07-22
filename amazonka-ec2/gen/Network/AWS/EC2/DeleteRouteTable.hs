{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteRouteTable
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route table. You must disassociate the route table
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
    , drttDryRun
    , drttRouteTableId

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
-- * 'drttDryRun'
--
-- * 'drttRouteTableId'
data DeleteRouteTable = DeleteRouteTable'
    { _drttDryRun       :: !(Maybe Bool)
    , _drttRouteTableId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRouteTable' smart constructor.
deleteRouteTable :: Text -> DeleteRouteTable
deleteRouteTable pRouteTableId =
    DeleteRouteTable'
    { _drttDryRun = Nothing
    , _drttRouteTableId = pRouteTableId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
drttDryRun :: Lens' DeleteRouteTable (Maybe Bool)
drttDryRun = lens _drttDryRun (\ s a -> s{_drttDryRun = a});

-- | The ID of the route table.
drttRouteTableId :: Lens' DeleteRouteTable Text
drttRouteTableId = lens _drttRouteTableId (\ s a -> s{_drttRouteTableId = a});

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
               "DryRun" =: _drttDryRun,
               "RouteTableId" =: _drttRouteTableId]

-- | /See:/ 'deleteRouteTableResponse' smart constructor.
data DeleteRouteTableResponse =
    DeleteRouteTableResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRouteTableResponse' smart constructor.
deleteRouteTableResponse :: DeleteRouteTableResponse
deleteRouteTableResponse = DeleteRouteTableResponse'
