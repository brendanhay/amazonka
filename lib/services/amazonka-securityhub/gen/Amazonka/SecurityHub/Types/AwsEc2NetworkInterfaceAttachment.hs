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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the network interface attachment.
--
-- /See:/ 'newAwsEc2NetworkInterfaceAttachment' smart constructor.
data AwsEc2NetworkInterfaceAttachment = AwsEc2NetworkInterfaceAttachment'
  { -- | Indicates when the attachment initiated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    attachTime :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the network interface attachment
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The device index of the network interface attachment on the instance.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the owner of the instance.
    instanceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The attachment state.
    --
    -- Valid values: @attaching@ | @attached@ | @detaching@ | @detached@
    status :: Prelude.Maybe Prelude.Text
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
-- 'attachTime', 'awsEc2NetworkInterfaceAttachment_attachTime' - Indicates when the attachment initiated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'attachmentId', 'awsEc2NetworkInterfaceAttachment_attachmentId' - The identifier of the network interface attachment
--
-- 'deleteOnTermination', 'awsEc2NetworkInterfaceAttachment_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'deviceIndex', 'awsEc2NetworkInterfaceAttachment_deviceIndex' - The device index of the network interface attachment on the instance.
--
-- 'instanceId', 'awsEc2NetworkInterfaceAttachment_instanceId' - The ID of the instance.
--
-- 'instanceOwnerId', 'awsEc2NetworkInterfaceAttachment_instanceOwnerId' - The Amazon Web Services account ID of the owner of the instance.
--
-- 'status', 'awsEc2NetworkInterfaceAttachment_status' - The attachment state.
--
-- Valid values: @attaching@ | @attached@ | @detaching@ | @detached@
newAwsEc2NetworkInterfaceAttachment ::
  AwsEc2NetworkInterfaceAttachment
newAwsEc2NetworkInterfaceAttachment =
  AwsEc2NetworkInterfaceAttachment'
    { attachTime =
        Prelude.Nothing,
      attachmentId = Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      deviceIndex = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceOwnerId = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Indicates when the attachment initiated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsEc2NetworkInterfaceAttachment_attachTime :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceAttachment_attachTime = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {attachTime} -> attachTime) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {attachTime = a} :: AwsEc2NetworkInterfaceAttachment)

-- | The identifier of the network interface attachment
awsEc2NetworkInterfaceAttachment_attachmentId :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceAttachment_attachmentId = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {attachmentId} -> attachmentId) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {attachmentId = a} :: AwsEc2NetworkInterfaceAttachment)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
awsEc2NetworkInterfaceAttachment_deleteOnTermination :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Bool)
awsEc2NetworkInterfaceAttachment_deleteOnTermination = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {deleteOnTermination = a} :: AwsEc2NetworkInterfaceAttachment)

-- | The device index of the network interface attachment on the instance.
awsEc2NetworkInterfaceAttachment_deviceIndex :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
awsEc2NetworkInterfaceAttachment_deviceIndex = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {deviceIndex} -> deviceIndex) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {deviceIndex = a} :: AwsEc2NetworkInterfaceAttachment)

-- | The ID of the instance.
awsEc2NetworkInterfaceAttachment_instanceId :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceAttachment_instanceId = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {instanceId} -> instanceId) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {instanceId = a} :: AwsEc2NetworkInterfaceAttachment)

-- | The Amazon Web Services account ID of the owner of the instance.
awsEc2NetworkInterfaceAttachment_instanceOwnerId :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceAttachment_instanceOwnerId = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {instanceOwnerId} -> instanceOwnerId) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {instanceOwnerId = a} :: AwsEc2NetworkInterfaceAttachment)

-- | The attachment state.
--
-- Valid values: @attaching@ | @attached@ | @detaching@ | @detached@
awsEc2NetworkInterfaceAttachment_status :: Lens.Lens' AwsEc2NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceAttachment_status = Lens.lens (\AwsEc2NetworkInterfaceAttachment' {status} -> status) (\s@AwsEc2NetworkInterfaceAttachment' {} a -> s {status = a} :: AwsEc2NetworkInterfaceAttachment)

instance
  Data.FromJSON
    AwsEc2NetworkInterfaceAttachment
  where
  parseJSON =
    Data.withObject
      "AwsEc2NetworkInterfaceAttachment"
      ( \x ->
          AwsEc2NetworkInterfaceAttachment'
            Prelude.<$> (x Data..:? "AttachTime")
            Prelude.<*> (x Data..:? "AttachmentId")
            Prelude.<*> (x Data..:? "DeleteOnTermination")
            Prelude.<*> (x Data..:? "DeviceIndex")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "InstanceOwnerId")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsEc2NetworkInterfaceAttachment
  where
  hashWithSalt
    _salt
    AwsEc2NetworkInterfaceAttachment' {..} =
      _salt `Prelude.hashWithSalt` attachTime
        `Prelude.hashWithSalt` attachmentId
        `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` deviceIndex
        `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` instanceOwnerId
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsEc2NetworkInterfaceAttachment
  where
  rnf AwsEc2NetworkInterfaceAttachment' {..} =
    Prelude.rnf attachTime
      `Prelude.seq` Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf deviceIndex
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf instanceOwnerId
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON AwsEc2NetworkInterfaceAttachment where
  toJSON AwsEc2NetworkInterfaceAttachment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttachTime" Data..=) Prelude.<$> attachTime,
            ("AttachmentId" Data..=) Prelude.<$> attachmentId,
            ("DeleteOnTermination" Data..=)
              Prelude.<$> deleteOnTermination,
            ("DeviceIndex" Data..=) Prelude.<$> deviceIndex,
            ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("InstanceOwnerId" Data..=)
              Prelude.<$> instanceOwnerId,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
