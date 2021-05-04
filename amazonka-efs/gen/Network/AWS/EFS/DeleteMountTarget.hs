{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DeleteMountTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified mount target.
--
-- This operation forcibly breaks any mounts of the file system by using
-- the mount target that is being deleted, which might disrupt instances or
-- applications using those mounts. To avoid applications getting cut off
-- abruptly, you might consider unmounting any mounts of the mount target,
-- if feasible. The operation also deletes the associated network
-- interface. Uncommitted writes might be lost, but breaking a mount target
-- using this operation does not corrupt the file system itself. The file
-- system you created remains. You can mount an EC2 instance in your VPC by
-- using another mount target.
--
-- This operation requires permissions for the following action on the file
-- system:
--
-- -   @elasticfilesystem:DeleteMountTarget@
--
-- The @DeleteMountTarget@ call returns while the mount target state is
-- still @deleting@. You can check the mount target deletion by calling the
-- DescribeMountTargets operation, which returns a list of mount target
-- descriptions for the given file system.
--
-- The operation also requires permissions for the following Amazon EC2
-- action on the mount target\'s network interface:
--
-- -   @ec2:DeleteNetworkInterface@
module Network.AWS.EFS.DeleteMountTarget
  ( -- * Creating a Request
    DeleteMountTarget (..),
    newDeleteMountTarget,

    -- * Request Lenses
    deleteMountTarget_mountTargetId,

    -- * Destructuring the Response
    DeleteMountTargetResponse (..),
    newDeleteMountTargetResponse,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteMountTarget' smart constructor.
data DeleteMountTarget = DeleteMountTarget'
  { -- | The ID of the mount target to delete (String).
    mountTargetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteMountTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mountTargetId', 'deleteMountTarget_mountTargetId' - The ID of the mount target to delete (String).
newDeleteMountTarget ::
  -- | 'mountTargetId'
  Prelude.Text ->
  DeleteMountTarget
newDeleteMountTarget pMountTargetId_ =
  DeleteMountTarget' {mountTargetId = pMountTargetId_}

-- | The ID of the mount target to delete (String).
deleteMountTarget_mountTargetId :: Lens.Lens' DeleteMountTarget Prelude.Text
deleteMountTarget_mountTargetId = Lens.lens (\DeleteMountTarget' {mountTargetId} -> mountTargetId) (\s@DeleteMountTarget' {} a -> s {mountTargetId = a} :: DeleteMountTarget)

instance Prelude.AWSRequest DeleteMountTarget where
  type Rs DeleteMountTarget = DeleteMountTargetResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteMountTargetResponse'

instance Prelude.Hashable DeleteMountTarget

instance Prelude.NFData DeleteMountTarget

instance Prelude.ToHeaders DeleteMountTarget where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteMountTarget where
  toPath DeleteMountTarget' {..} =
    Prelude.mconcat
      [ "/2015-02-01/mount-targets/",
        Prelude.toBS mountTargetId
      ]

instance Prelude.ToQuery DeleteMountTarget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMountTargetResponse' smart constructor.
data DeleteMountTargetResponse = DeleteMountTargetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteMountTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMountTargetResponse ::
  DeleteMountTargetResponse
newDeleteMountTargetResponse =
  DeleteMountTargetResponse'

instance Prelude.NFData DeleteMountTargetResponse
