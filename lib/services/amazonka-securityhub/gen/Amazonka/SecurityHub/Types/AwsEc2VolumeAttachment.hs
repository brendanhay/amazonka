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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VolumeAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VolumeAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An attachment to an Amazon EC2 volume.
--
-- /See:/ 'newAwsEc2VolumeAttachment' smart constructor.
data AwsEc2VolumeAttachment = AwsEc2VolumeAttachment'
  { -- | Whether the EBS volume is deleted when the EC2 instance is terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The attachment state of the volume. Valid values are as follows:
    --
    -- -   @attaching@
    --
    -- -   @attached@
    --
    -- -   @busy@
    --
    -- -   @detaching@
    --
    -- -   @detached@
    status :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the EC2 instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The datetime when the attachment initiated.
    attachTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VolumeAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteOnTermination', 'awsEc2VolumeAttachment_deleteOnTermination' - Whether the EBS volume is deleted when the EC2 instance is terminated.
--
-- 'status', 'awsEc2VolumeAttachment_status' - The attachment state of the volume. Valid values are as follows:
--
-- -   @attaching@
--
-- -   @attached@
--
-- -   @busy@
--
-- -   @detaching@
--
-- -   @detached@
--
-- 'instanceId', 'awsEc2VolumeAttachment_instanceId' - The identifier of the EC2 instance.
--
-- 'attachTime', 'awsEc2VolumeAttachment_attachTime' - The datetime when the attachment initiated.
newAwsEc2VolumeAttachment ::
  AwsEc2VolumeAttachment
newAwsEc2VolumeAttachment =
  AwsEc2VolumeAttachment'
    { deleteOnTermination =
        Prelude.Nothing,
      status = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      attachTime = Prelude.Nothing
    }

-- | Whether the EBS volume is deleted when the EC2 instance is terminated.
awsEc2VolumeAttachment_deleteOnTermination :: Lens.Lens' AwsEc2VolumeAttachment (Prelude.Maybe Prelude.Bool)
awsEc2VolumeAttachment_deleteOnTermination = Lens.lens (\AwsEc2VolumeAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@AwsEc2VolumeAttachment' {} a -> s {deleteOnTermination = a} :: AwsEc2VolumeAttachment)

-- | The attachment state of the volume. Valid values are as follows:
--
-- -   @attaching@
--
-- -   @attached@
--
-- -   @busy@
--
-- -   @detaching@
--
-- -   @detached@
awsEc2VolumeAttachment_status :: Lens.Lens' AwsEc2VolumeAttachment (Prelude.Maybe Prelude.Text)
awsEc2VolumeAttachment_status = Lens.lens (\AwsEc2VolumeAttachment' {status} -> status) (\s@AwsEc2VolumeAttachment' {} a -> s {status = a} :: AwsEc2VolumeAttachment)

-- | The identifier of the EC2 instance.
awsEc2VolumeAttachment_instanceId :: Lens.Lens' AwsEc2VolumeAttachment (Prelude.Maybe Prelude.Text)
awsEc2VolumeAttachment_instanceId = Lens.lens (\AwsEc2VolumeAttachment' {instanceId} -> instanceId) (\s@AwsEc2VolumeAttachment' {} a -> s {instanceId = a} :: AwsEc2VolumeAttachment)

-- | The datetime when the attachment initiated.
awsEc2VolumeAttachment_attachTime :: Lens.Lens' AwsEc2VolumeAttachment (Prelude.Maybe Prelude.Text)
awsEc2VolumeAttachment_attachTime = Lens.lens (\AwsEc2VolumeAttachment' {attachTime} -> attachTime) (\s@AwsEc2VolumeAttachment' {} a -> s {attachTime = a} :: AwsEc2VolumeAttachment)

instance Data.FromJSON AwsEc2VolumeAttachment where
  parseJSON =
    Data.withObject
      "AwsEc2VolumeAttachment"
      ( \x ->
          AwsEc2VolumeAttachment'
            Prelude.<$> (x Data..:? "DeleteOnTermination")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "AttachTime")
      )

instance Prelude.Hashable AwsEc2VolumeAttachment where
  hashWithSalt _salt AwsEc2VolumeAttachment' {..} =
    _salt `Prelude.hashWithSalt` deleteOnTermination
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` attachTime

instance Prelude.NFData AwsEc2VolumeAttachment where
  rnf AwsEc2VolumeAttachment' {..} =
    Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf attachTime

instance Data.ToJSON AwsEc2VolumeAttachment where
  toJSON AwsEc2VolumeAttachment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeleteOnTermination" Data..=)
              Prelude.<$> deleteOnTermination,
            ("Status" Data..=) Prelude.<$> status,
            ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("AttachTime" Data..=) Prelude.<$> attachTime
          ]
      )
