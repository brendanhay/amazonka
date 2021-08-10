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
-- Module      : Network.AWS.EC2.DetachVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- When a volume with an AWS Marketplace product code is detached from an
-- instance, the product code is no longer associated with the instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-detaching-volume.html Detaching an Amazon EBS volume>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.DetachVolume
  ( -- * Creating a Request
    DetachVolume (..),
    newDetachVolume,

    -- * Request Lenses
    detachVolume_instanceId,
    detachVolume_dryRun,
    detachVolume_device,
    detachVolume_force,
    detachVolume_volumeId,

    -- * Destructuring the Response
    VolumeAttachment (..),
    newVolumeAttachment,

    -- * Response Lenses
    volumeAttachment_instanceId,
    volumeAttachment_attachTime,
    volumeAttachment_device,
    volumeAttachment_volumeId,
    volumeAttachment_state,
    volumeAttachment_deleteOnTermination,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachVolume' smart constructor.
data DetachVolume = DetachVolume'
  { -- | The ID of the instance. If you are detaching a Multi-Attach enabled
    -- volume, you must specify an instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The device name.
    device :: Prelude.Maybe Prelude.Text,
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
-- 'instanceId', 'detachVolume_instanceId' - The ID of the instance. If you are detaching a Multi-Attach enabled
-- volume, you must specify an instance ID.
--
-- 'dryRun', 'detachVolume_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'device', 'detachVolume_device' - The device name.
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
    { instanceId = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      device = Prelude.Nothing,
      force = Prelude.Nothing,
      volumeId = pVolumeId_
    }

-- | The ID of the instance. If you are detaching a Multi-Attach enabled
-- volume, you must specify an instance ID.
detachVolume_instanceId :: Lens.Lens' DetachVolume (Prelude.Maybe Prelude.Text)
detachVolume_instanceId = Lens.lens (\DetachVolume' {instanceId} -> instanceId) (\s@DetachVolume' {} a -> s {instanceId = a} :: DetachVolume)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
detachVolume_dryRun :: Lens.Lens' DetachVolume (Prelude.Maybe Prelude.Bool)
detachVolume_dryRun = Lens.lens (\DetachVolume' {dryRun} -> dryRun) (\s@DetachVolume' {} a -> s {dryRun = a} :: DetachVolume)

-- | The device name.
detachVolume_device :: Lens.Lens' DetachVolume (Prelude.Maybe Prelude.Text)
detachVolume_device = Lens.lens (\DetachVolume' {device} -> device) (\s@DetachVolume' {} a -> s {device = a} :: DetachVolume)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML (\s h x -> Core.parseXML x)

instance Prelude.Hashable DetachVolume

instance Prelude.NFData DetachVolume

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
        "InstanceId" Core.=: instanceId,
        "DryRun" Core.=: dryRun,
        "Device" Core.=: device,
        "Force" Core.=: force,
        "VolumeId" Core.=: volumeId
      ]
