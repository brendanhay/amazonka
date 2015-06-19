{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticFileSystem.DeleteMountTarget
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

-- | Deletes the specified mount target.
--
-- This operation forcibly breaks any mounts of the file system via the
-- mount target being deleted, which might disrupt instances or
-- applications using those mounts. To avoid applications getting cut off
-- abruptly, you might consider unmounting any mounts of the mount target,
-- if feasible. The operation also deletes the associated network
-- interface. Uncommitted writes may be lost, but breaking a mount target
-- using this operation does not corrupt the file system itself. The file
-- system you created remains. You can mount an EC2 instance in your VPC
-- using another mount target.
--
-- This operation requires permission for the following action on the file
-- system:
--
-- -   @elasticfilesystem:DeleteMountTarget@
--
-- The @DeleteMountTarget@ call returns while the mount target state is
-- still \"deleting\". You can check the mount target deletion by calling
-- the DescribeMountTargets API, which returns a list of mount target
-- descriptions for the given file system.
--
-- The operation also requires permission for the following Amazon EC2
-- action on the mount target\'s network interface:
--
-- -   @ec2:DeleteNetworkInterface@
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DeleteMountTarget.html>
module Network.AWS.ElasticFileSystem.DeleteMountTarget
    (
    -- * Request
      DeleteMountTarget
    -- ** Request constructor
    , deleteMountTarget
    -- ** Request lenses
    , dmtMountTargetId

    -- * Response
    , DeleteMountTargetResponse
    -- ** Response constructor
    , deleteMountTargetResponse
    ) where

import Network.AWS.ElasticFileSystem.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteMountTarget' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmtMountTargetId'
newtype DeleteMountTarget = DeleteMountTarget'{_dmtMountTargetId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteMountTarget' smart constructor.
deleteMountTarget :: Text -> DeleteMountTarget
deleteMountTarget pMountTargetId = DeleteMountTarget'{_dmtMountTargetId = pMountTargetId};

-- | String. The ID of the mount target to delete.
dmtMountTargetId :: Lens' DeleteMountTarget Text
dmtMountTargetId = lens _dmtMountTargetId (\ s a -> s{_dmtMountTargetId = a});

instance AWSRequest DeleteMountTarget where
        type Sv DeleteMountTarget = ElasticFileSystem
        type Rs DeleteMountTarget = DeleteMountTargetResponse
        request = delete
        response = receiveNull DeleteMountTargetResponse'

instance ToHeaders DeleteMountTarget where
        toHeaders = const mempty

instance ToPath DeleteMountTarget where
        toPath DeleteMountTarget'{..}
          = mconcat
              ["/2015-02-01/mount-targets/",
               toText _dmtMountTargetId]

instance ToQuery DeleteMountTarget where
        toQuery = const mempty

-- | /See:/ 'deleteMountTargetResponse' smart constructor.
data DeleteMountTargetResponse = DeleteMountTargetResponse' deriving (Eq, Read, Show)

-- | 'DeleteMountTargetResponse' smart constructor.
deleteMountTargetResponse :: DeleteMountTargetResponse
deleteMountTargetResponse = DeleteMountTargetResponse';
