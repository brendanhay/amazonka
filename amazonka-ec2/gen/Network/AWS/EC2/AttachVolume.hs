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
-- Module      : Network.AWS.EC2.AttachVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an EBS volume to a running or stopped instance and exposes it
-- to the instance with the specified device name.
--
-- Encrypted EBS volumes must be attached to instances that support Amazon
-- EBS encryption. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- After you attach an EBS volume, you must make it available. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-using-volumes.html Making an EBS volume available for use>.
--
-- If a volume has an AWS Marketplace product code:
--
-- -   The volume can be attached only to a stopped instance.
--
-- -   AWS Marketplace product codes are copied from the volume to the
--     instance.
--
-- -   You must be subscribed to the product.
--
-- -   The instance type and operating system of the instance must support
--     the product. For example, you can\'t detach a volume from a Windows
--     instance and attach it to a Linux instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-attaching-volume.html Attaching Amazon EBS volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.AttachVolume
  ( -- * Creating a Request
    AttachVolume (..),
    newAttachVolume,

    -- * Request Lenses
    attachVolume_dryRun,
    attachVolume_device,
    attachVolume_instanceId,
    attachVolume_volumeId,

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

-- | /See:/ 'newAttachVolume' smart constructor.
data AttachVolume = AttachVolume'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The device name (for example, @\/dev\/sdh@ or @xvdh@).
    device :: Prelude.Text,
    -- | The ID of the instance.
    instanceId :: Prelude.Text,
    -- | The ID of the EBS volume. The volume and instance must be within the
    -- same Availability Zone.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'attachVolume_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'device', 'attachVolume_device' - The device name (for example, @\/dev\/sdh@ or @xvdh@).
--
-- 'instanceId', 'attachVolume_instanceId' - The ID of the instance.
--
-- 'volumeId', 'attachVolume_volumeId' - The ID of the EBS volume. The volume and instance must be within the
-- same Availability Zone.
newAttachVolume ::
  -- | 'device'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'volumeId'
  Prelude.Text ->
  AttachVolume
newAttachVolume pDevice_ pInstanceId_ pVolumeId_ =
  AttachVolume'
    { dryRun = Prelude.Nothing,
      device = pDevice_,
      instanceId = pInstanceId_,
      volumeId = pVolumeId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
attachVolume_dryRun :: Lens.Lens' AttachVolume (Prelude.Maybe Prelude.Bool)
attachVolume_dryRun = Lens.lens (\AttachVolume' {dryRun} -> dryRun) (\s@AttachVolume' {} a -> s {dryRun = a} :: AttachVolume)

-- | The device name (for example, @\/dev\/sdh@ or @xvdh@).
attachVolume_device :: Lens.Lens' AttachVolume Prelude.Text
attachVolume_device = Lens.lens (\AttachVolume' {device} -> device) (\s@AttachVolume' {} a -> s {device = a} :: AttachVolume)

-- | The ID of the instance.
attachVolume_instanceId :: Lens.Lens' AttachVolume Prelude.Text
attachVolume_instanceId = Lens.lens (\AttachVolume' {instanceId} -> instanceId) (\s@AttachVolume' {} a -> s {instanceId = a} :: AttachVolume)

-- | The ID of the EBS volume. The volume and instance must be within the
-- same Availability Zone.
attachVolume_volumeId :: Lens.Lens' AttachVolume Prelude.Text
attachVolume_volumeId = Lens.lens (\AttachVolume' {volumeId} -> volumeId) (\s@AttachVolume' {} a -> s {volumeId = a} :: AttachVolume)

instance Core.AWSRequest AttachVolume where
  type AWSResponse AttachVolume = VolumeAttachment
  request = Request.postQuery defaultService
  response =
    Response.receiveXML (\s h x -> Core.parseXML x)

instance Prelude.Hashable AttachVolume

instance Prelude.NFData AttachVolume

instance Core.ToHeaders AttachVolume where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AttachVolume where
  toPath = Prelude.const "/"

instance Core.ToQuery AttachVolume where
  toQuery AttachVolume' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AttachVolume" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "Device" Core.=: device,
        "InstanceId" Core.=: instanceId,
        "VolumeId" Core.=: volumeId
      ]
