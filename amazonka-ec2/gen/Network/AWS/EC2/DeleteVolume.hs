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
-- Module      : Network.AWS.EC2.DeleteVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified EBS volume. The volume must be in the @available@
-- state (not attached to an instance).
--
-- The volume can remain in the @deleting@ state for several minutes.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-volume.html Deleting an Amazon EBS volume>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.DeleteVolume
  ( -- * Creating a Request
    DeleteVolume (..),
    newDeleteVolume,

    -- * Request Lenses
    deleteVolume_dryRun,
    deleteVolume_volumeId,

    -- * Destructuring the Response
    DeleteVolumeResponse (..),
    newDeleteVolumeResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteVolume' smart constructor.
data DeleteVolume = DeleteVolume'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the volume.
    volumeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteVolume_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'volumeId', 'deleteVolume_volumeId' - The ID of the volume.
newDeleteVolume ::
  -- | 'volumeId'
  Core.Text ->
  DeleteVolume
newDeleteVolume pVolumeId_ =
  DeleteVolume'
    { dryRun = Core.Nothing,
      volumeId = pVolumeId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVolume_dryRun :: Lens.Lens' DeleteVolume (Core.Maybe Core.Bool)
deleteVolume_dryRun = Lens.lens (\DeleteVolume' {dryRun} -> dryRun) (\s@DeleteVolume' {} a -> s {dryRun = a} :: DeleteVolume)

-- | The ID of the volume.
deleteVolume_volumeId :: Lens.Lens' DeleteVolume Core.Text
deleteVolume_volumeId = Lens.lens (\DeleteVolume' {volumeId} -> volumeId) (\s@DeleteVolume' {} a -> s {volumeId = a} :: DeleteVolume)

instance Core.AWSRequest DeleteVolume where
  type AWSResponse DeleteVolume = DeleteVolumeResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteVolumeResponse'

instance Core.Hashable DeleteVolume

instance Core.NFData DeleteVolume

instance Core.ToHeaders DeleteVolume where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteVolume where
  toPath = Core.const "/"

instance Core.ToQuery DeleteVolume where
  toQuery DeleteVolume' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteVolume" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "VolumeId" Core.=: volumeId
      ]

-- | /See:/ 'newDeleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVolumeResponse ::
  DeleteVolumeResponse
newDeleteVolumeResponse = DeleteVolumeResponse'

instance Core.NFData DeleteVolumeResponse
