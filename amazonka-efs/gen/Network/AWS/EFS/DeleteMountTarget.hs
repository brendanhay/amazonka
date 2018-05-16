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
-- Module      : Network.AWS.EFS.DeleteMountTarget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified mount target.
--
--
-- This operation forcibly breaks any mounts of the file system via the mount target that is being deleted, which might disrupt instances or applications using those mounts. To avoid applications getting cut off abruptly, you might consider unmounting any mounts of the mount target, if feasible. The operation also deletes the associated network interface. Uncommitted writes may be lost, but breaking a mount target using this operation does not corrupt the file system itself. The file system you created remains. You can mount an EC2 instance in your VPC via another mount target.
--
-- This operation requires permissions for the following action on the file system:
--
--     * @elasticfilesystem:DeleteMountTarget@
--
--
--
-- The operation also requires permissions for the following Amazon EC2 action on the mount target's network interface:
--
--     * @ec2:DeleteNetworkInterface@
--
--
--
module Network.AWS.EFS.DeleteMountTarget
    (
    -- * Creating a Request
      deleteMountTarget
    , DeleteMountTarget
    -- * Request Lenses
    , dMountTargetId

    -- * Destructuring the Response
    , deleteMountTargetResponse
    , DeleteMountTargetResponse
    ) where

import Network.AWS.EFS.Types
import Network.AWS.EFS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteMountTarget' smart constructor.
newtype DeleteMountTarget = DeleteMountTarget'
  { _dMountTargetId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMountTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dMountTargetId' - ID of the mount target to delete (String).
deleteMountTarget
    :: Text -- ^ 'dMountTargetId'
    -> DeleteMountTarget
deleteMountTarget pMountTargetId_ =
  DeleteMountTarget' {_dMountTargetId = pMountTargetId_}


-- | ID of the mount target to delete (String).
dMountTargetId :: Lens' DeleteMountTarget Text
dMountTargetId = lens _dMountTargetId (\ s a -> s{_dMountTargetId = a})

instance AWSRequest DeleteMountTarget where
        type Rs DeleteMountTarget = DeleteMountTargetResponse
        request = delete efs
        response = receiveNull DeleteMountTargetResponse'

instance Hashable DeleteMountTarget where

instance NFData DeleteMountTarget where

instance ToHeaders DeleteMountTarget where
        toHeaders = const mempty

instance ToPath DeleteMountTarget where
        toPath DeleteMountTarget'{..}
          = mconcat
              ["/2015-02-01/mount-targets/", toBS _dMountTargetId]

instance ToQuery DeleteMountTarget where
        toQuery = const mempty

-- | /See:/ 'deleteMountTargetResponse' smart constructor.
data DeleteMountTargetResponse =
  DeleteMountTargetResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMountTargetResponse' with the minimum fields required to make a request.
--
deleteMountTargetResponse
    :: DeleteMountTargetResponse
deleteMountTargetResponse = DeleteMountTargetResponse'


instance NFData DeleteMountTargetResponse where
