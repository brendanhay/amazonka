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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteVolume' smart constructor.
data DeleteVolume = DeleteVolume'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the volume.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteVolume
newDeleteVolume pVolumeId_ =
  DeleteVolume'
    { dryRun = Prelude.Nothing,
      volumeId = pVolumeId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVolume_dryRun :: Lens.Lens' DeleteVolume (Prelude.Maybe Prelude.Bool)
deleteVolume_dryRun = Lens.lens (\DeleteVolume' {dryRun} -> dryRun) (\s@DeleteVolume' {} a -> s {dryRun = a} :: DeleteVolume)

-- | The ID of the volume.
deleteVolume_volumeId :: Lens.Lens' DeleteVolume Prelude.Text
deleteVolume_volumeId = Lens.lens (\DeleteVolume' {volumeId} -> volumeId) (\s@DeleteVolume' {} a -> s {volumeId = a} :: DeleteVolume)

instance Prelude.AWSRequest DeleteVolume where
  type Rs DeleteVolume = DeleteVolumeResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteVolumeResponse'

instance Prelude.Hashable DeleteVolume

instance Prelude.NFData DeleteVolume

instance Prelude.ToHeaders DeleteVolume where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteVolume where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteVolume where
  toQuery DeleteVolume' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteVolume" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "VolumeId" Prelude.=: volumeId
      ]

-- | /See:/ 'newDeleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVolumeResponse ::
  DeleteVolumeResponse
newDeleteVolumeResponse = DeleteVolumeResponse'

instance Prelude.NFData DeleteVolumeResponse
