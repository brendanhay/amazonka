{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DeleteVPC
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

-- | Deletes the specified VPC. You must detach or delete all gateways and
-- resources that are associated with the VPC before you can delete it. For
-- example, you must terminate all instances running in the VPC, delete all
-- security groups associated with the VPC (except the default one), delete
-- all route tables associated with the VPC (except the default one), and
-- so on.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVPC.html>
module Network.AWS.EC2.DeleteVPC
    (
    -- * Request
      DeleteVPC
    -- ** Request constructor
    , deleteVPC
    -- ** Request lenses
    , deleteDryRun
    , deleteVPCId

    -- * Response
    , DeleteVPCResponse
    -- ** Response constructor
    , deleteVPCResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'deleteVPC' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deleteDryRun'
--
-- * 'deleteVPCId'
data DeleteVPC = DeleteVPC'{_deleteDryRun :: Maybe Bool, _deleteVPCId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteVPC' smart constructor.
deleteVPC :: Text -> DeleteVPC
deleteVPC pVPCId = DeleteVPC'{_deleteDryRun = Nothing, _deleteVPCId = pVPCId};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteDryRun :: Lens' DeleteVPC (Maybe Bool)
deleteDryRun = lens _deleteDryRun (\ s a -> s{_deleteDryRun = a});

-- | The ID of the VPC.
deleteVPCId :: Lens' DeleteVPC Text
deleteVPCId = lens _deleteVPCId (\ s a -> s{_deleteVPCId = a});

instance AWSRequest DeleteVPC where
        type Sv DeleteVPC = EC2
        type Rs DeleteVPC = DeleteVPCResponse
        request = post
        response = receiveNull DeleteVPCResponse'

instance ToHeaders DeleteVPC where
        toHeaders = const mempty

instance ToPath DeleteVPC where
        toPath = const "/"

instance ToQuery DeleteVPC where
        toQuery DeleteVPC'{..}
          = mconcat
              ["Action" =: ("DeleteVPC" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _deleteDryRun, "VpcId" =: _deleteVPCId]

-- | /See:/ 'deleteVPCResponse' smart constructor.
data DeleteVPCResponse = DeleteVPCResponse' deriving (Eq, Read, Show)

-- | 'DeleteVPCResponse' smart constructor.
deleteVPCResponse :: DeleteVPCResponse
deleteVPCResponse = DeleteVPCResponse';
