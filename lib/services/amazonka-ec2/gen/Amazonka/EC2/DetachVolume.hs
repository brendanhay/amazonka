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
-- Module      : Amazonka.EC2.DetachVolume
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an EBS volume from an instance. Make sure to unmount any file
-- systems on the device within your operating system before detaching the
-- volume. Failure to do so can result in the volume becoming stuck in the
-- @busy@ state while detaching. If this happens, detachment can be delayed
-- indefinitely until you unmount the volume, force detachment, reboot the
-- instance, or all three. If an EBS volume is the root device of an
-- instance, it can\'t be detached while the instance is running. To detach
-- the root volume, stop the instance first.
--
-- When a volume with an Amazon Web Services Marketplace product code is
-- detached from an instance, the product code is no longer associated with
-- the instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-detaching-volume.html Detach an Amazon EBS volume>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.DetachVolume
  ( -- * Creating a Request
    DetachVolume (..),
    newDetachVolume,

    -- * Request Lenses
    detachVolume_device,
    detachVolume_dryRun,
    detachVolume_instanceId,
    detachVolume_force,
    detachVolume_volumeId,

    -- * Destructuring the Response
    VolumeAttachment (..),
    newVolumeAttachment,

    -- * Response Lenses
    volumeAttachment_deleteOnTermination,
    volumeAttachment_device,
    volumeAttachment_state,
    volumeAttachment_instanceId,
    volumeAttachment_attachTime,
    volumeAttachment_volumeId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachVolume' smart constructor.
data DetachVolume = DetachVolume'
  { -- | The device name.
    device :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance. If you are detaching a Multi-Attach enabled
    -- volume, you must specify an instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Forces detachment if the previous detachment attempt did not occur
    -- cleanly (for example, logging into an instance, unmounting the volume,
    -- and detaching normally). This option can lead to data loss or a
    -- corrupted file system. Use this option only as a last resort to detach a
    -- volume from a failed instance. The instance won\'t have an opportunity
    -- to flush file system caches or file system metadata. If you use this
    -- option, you must perform file system check and repair procedures.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the volume.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'device', 'detachVolume_device' - The device name.
--
-- 'dryRun', 'detachVolume_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'detachVolume_instanceId' - The ID of the instance. If you are detaching a Multi-Attach enabled
-- volume, you must specify an instance ID.
--
-- 'force', 'detachVolume_force' - Forces detachment if the previous detachment attempt did not occur
-- cleanly (for example, logging into an instance, unmounting the volume,
-- and detaching normally). This option can lead to data loss or a
-- corrupted file system. Use this option only as a last resort to detach a
-- volume from a failed instance. The instance won\'t have an opportunity
-- to flush file system caches or file system metadata. If you use this
-- option, you must perform file system check and repair procedures.
--
-- 'volumeId', 'detachVolume_volumeId' - The ID of the volume.
newDetachVolume ::
  -- | 'volumeId'
  Prelude.Text ->
  DetachVolume
newDetachVolume pVolumeId_ =
  DetachVolume'
    { device = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      force = Prelude.Nothing,
      volumeId = pVolumeId_
    }

-- | The device name.
detachVolume_device :: Lens.Lens' DetachVolume (Prelude.Maybe Prelude.Text)
detachVolume_device = Lens.lens (\DetachVolume' {device} -> device) (\s@DetachVolume' {} a -> s {device = a} :: DetachVolume)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
detachVolume_dryRun :: Lens.Lens' DetachVolume (Prelude.Maybe Prelude.Bool)
detachVolume_dryRun = Lens.lens (\DetachVolume' {dryRun} -> dryRun) (\s@DetachVolume' {} a -> s {dryRun = a} :: DetachVolume)

-- | The ID of the instance. If you are detaching a Multi-Attach enabled
-- volume, you must specify an instance ID.
detachVolume_instanceId :: Lens.Lens' DetachVolume (Prelude.Maybe Prelude.Text)
detachVolume_instanceId = Lens.lens (\DetachVolume' {instanceId} -> instanceId) (\s@DetachVolume' {} a -> s {instanceId = a} :: DetachVolume)

-- | Forces detachment if the previous detachment attempt did not occur
-- cleanly (for example, logging into an instance, unmounting the volume,
-- and detaching normally). This option can lead to data loss or a
-- corrupted file system. Use this option only as a last resort to detach a
-- volume from a failed instance. The instance won\'t have an opportunity
-- to flush file system caches or file system metadata. If you use this
-- option, you must perform file system check and repair procedures.
detachVolume_force :: Lens.Lens' DetachVolume (Prelude.Maybe Prelude.Bool)
detachVolume_force = Lens.lens (\DetachVolume' {force} -> force) (\s@DetachVolume' {} a -> s {force = a} :: DetachVolume)

-- | The ID of the volume.
detachVolume_volumeId :: Lens.Lens' DetachVolume Prelude.Text
detachVolume_volumeId = Lens.lens (\DetachVolume' {volumeId} -> volumeId) (\s@DetachVolume' {} a -> s {volumeId = a} :: DetachVolume)

instance Core.AWSRequest DetachVolume where
  type AWSResponse DetachVolume = VolumeAttachment
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML (\s h x -> Core.parseXML x)

instance Prelude.Hashable DetachVolume where
  hashWithSalt _salt DetachVolume' {..} =
    _salt `Prelude.hashWithSalt` device
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData DetachVolume where
  rnf DetachVolume' {..} =
    Prelude.rnf device
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf force
      `Prelude.seq` Prelude.rnf volumeId

instance Core.ToHeaders DetachVolume where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DetachVolume where
  toPath = Prelude.const "/"

instance Core.ToQuery DetachVolume where
  toQuery DetachVolume' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DetachVolume" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "Device" Core.=: device,
        "DryRun" Core.=: dryRun,
        "InstanceId" Core.=: instanceId,
        "Force" Core.=: force,
        "VolumeId" Core.=: volumeId
      ]
