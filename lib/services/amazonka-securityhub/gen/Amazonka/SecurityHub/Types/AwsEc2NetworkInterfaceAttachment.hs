{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the network interface attachment.
--
-- /See:/ 'newAwsEc2NetworkInterfaceAttachment' smart constructor.
data AwsEc2NetworkInterfaceAttachment = AwsEc2NetworkInterfaceAttachment'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The attachment state.
    --
    -- Valid values: @attaching@ | @attached@ | @detaching@ | @detached@
    status :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the network interface attachment
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the owner of the instance.
    instanceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the attachment initiated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    attachTime :: Prelude.Maybe Prelude.Text,
    -- | The device index of the network interface attachment on the instance.
    deviceIndex :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2NetworkInterfaceAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'awsEc2NetworkInterfaceAttachment_instanceId' - The ID of the instance.
--
-- 'status', 'awsEc2NetworkInterfaceAttachment_status' - The attachment state.
--
-- Valid values: @attaching@ | @attached@ | @detaching@ | @detached@
--
-- 'deleteOnTermination', 'awsEc2NetworkInterfaceAttachment_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'attachmentId', 'awsEc2NetworkInterfaceAttachment_attachmentId' - The identifier of the network interface attachment
--
-- 'instanceOwnerId', 'awsEc2NetworkInterfaceAttachment_instanceOwnerId' - The Amazon Web Services account ID of the owner of the instance.
--
-- 'attachTime', 'awsEc2NetworkInterfaceAttachment_attachTime' - Indicates when the attachment initiated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'deviceIndex', 'awsEc2NetworkInterfaceAttachment_deviceIndex' - The device index of the network interface attachment on the instance.
newAwsEc2NetworkInterfaceAttachment ::
  AwsEc2NetworkInterfaceAttachment
newAwsEc2NetworkInterfaceAttachment =
  AwsEc2NetworkInterfaceAttachment'
    { instanceId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      attachmentId = Prelude.Nothing,
      instanceOwnerId = Prelude.Nothing,
      attachTime = Prelude.Nothing,
      deviceIndex = Prelude.Nothing
    }

-- | The ID of the instance.
awsEc2NetworkInterfaceAttachment_instanceId :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceAttachment_instanceId = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {instanceId} -> instanceId) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {instanceId = a} :: AwsEc2NetworkInterfaceAttachment)

-- | The attachment state.
--
-- Valid values: @attaching@ | @attached@ | @detaching@ | @detached@
awsEc2NetworkInterfaceAttachment_status :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceAttachment_status = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {status} -> status) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {status = a} :: AwsEc2NetworkInterfaceAttachment)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
awsEc2NetworkInterfaceAttachment_deleteOnTermination :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Bool)
awsEc2NetworkInterfaceAttachment_deleteOnTermination = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {deleteOnTermination = a} :: AwsEc2NetworkInterfaceAttachment)

-- | The identifier of the network interface attachment
awsEc2NetworkInterfaceAttachment_attachmentId :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceAttachment_attachmentId = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {attachmentId} -> attachmentId) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {attachmentId = a} :: AwsEc2NetworkInterfaceAttachment)

-- | The Amazon Web Services account ID of the owner of the instance.
awsEc2NetworkInterfaceAttachment_instanceOwnerId :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceAttachment_instanceOwnerId = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {instanceOwnerId} -> instanceOwnerId) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {instanceOwnerId = a} :: AwsEc2NetworkInterfaceAttachment)

-- | Indicates when the attachment initiated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsEc2NetworkInterfaceAttachment_attachTime :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceAttachment_attachTime = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {attachTime} -> attachTime) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {attachTime = a} :: AwsEc2NetworkInterfaceAttachment)

-- | The device index of the network interface attachment on the instance.
awsEc2NetworkInterfaceAttachment_deviceIndex :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
awsEc2NetworkInterfaceAttachment_deviceIndex = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {deviceIndex} -> deviceIndex) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {deviceIndex = a} :: AwsEc2NetworkInterfaceAttachment)

instance
  Core.FromJSON
    AwsEc2NetworkInterfaceAttachment
  where
  parseJSON =
    Core.withObject
      "AwsEc2NetworkInterfaceAttachment"
      ( \x ->
          AwsEc2NetworkInterfaceAttachment'
            Prelude.<$> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "DeleteOnTermination")
            Prelude.<*> (x Core..:? "AttachmentId")
            Prelude.<*> (x Core..:? "InstanceOwnerId")
            Prelude.<*> (x Core..:? "AttachTime")
            Prelude.<*> (x Core..:? "DeviceIndex")
      )

instance
  Prelude.Hashable
    AwsEc2NetworkInterfaceAttachment
  where
  hashWithSalt
    salt'
    AwsEc2NetworkInterfaceAttachment' {..} =
      salt' `Prelude.hashWithSalt` deviceIndex
        `Prelude.hashWithSalt` attachTime
        `Prelude.hashWithSalt` instanceOwnerId
        `Prelude.hashWithSalt` attachmentId
        `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` instanceId

instance
  Prelude.NFData
    AwsEc2NetworkInterfaceAttachment
  where
  rnf AwsEc2NetworkInterfaceAttachment' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf deviceIndex
      `Prelude.seq` Prelude.rnf attachTime
      `Prelude.seq` Prelude.rnf instanceOwnerId
      `Prelude.seq` Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf status

instance Core.ToJSON AwsEc2NetworkInterfaceAttachment where
  toJSON AwsEc2NetworkInterfaceAttachment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InstanceId" Core..=) Prelude.<$> instanceId,
            ("Status" Core..=) Prelude.<$> status,
            ("DeleteOnTermination" Core..=)
              Prelude.<$> deleteOnTermination,
            ("AttachmentId" Core..=) Prelude.<$> attachmentId,
            ("InstanceOwnerId" Core..=)
              Prelude.<$> instanceOwnerId,
            ("AttachTime" Core..=) Prelude.<$> attachTime,
            ("DeviceIndex" Core..=) Prelude.<$> deviceIndex
          ]
      )
