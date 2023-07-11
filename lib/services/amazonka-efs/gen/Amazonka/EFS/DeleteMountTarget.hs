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
-- Module      : Amazonka.EFS.DeleteMountTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EFS.DeleteMountTarget
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteMountTarget' smart constructor.
data DeleteMountTarget = DeleteMountTarget'
  { -- | The ID of the mount target to delete (String).
    mountTargetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteMountTarget where
  type
    AWSResponse DeleteMountTarget =
      DeleteMountTargetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteMountTargetResponse'

instance Prelude.Hashable DeleteMountTarget where
  hashWithSalt _salt DeleteMountTarget' {..} =
    _salt `Prelude.hashWithSalt` mountTargetId

instance Prelude.NFData DeleteMountTarget where
  rnf DeleteMountTarget' {..} =
    Prelude.rnf mountTargetId

instance Data.ToHeaders DeleteMountTarget where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteMountTarget where
  toPath DeleteMountTarget' {..} =
    Prelude.mconcat
      [ "/2015-02-01/mount-targets/",
        Data.toBS mountTargetId
      ]

instance Data.ToQuery DeleteMountTarget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMountTargetResponse' smart constructor.
data DeleteMountTargetResponse = DeleteMountTargetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMountTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMountTargetResponse ::
  DeleteMountTargetResponse
newDeleteMountTargetResponse =
  DeleteMountTargetResponse'

instance Prelude.NFData DeleteMountTargetResponse where
  rnf _ = ()
