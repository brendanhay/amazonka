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
-- Module      : Network.AWS.SecurityHub.Types.AwsEc2VolumeAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsEc2VolumeAttachment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An attachment to an Amazon EC2 volume.
--
-- /See:/ 'newAwsEc2VolumeAttachment' smart constructor.
data AwsEc2VolumeAttachment = AwsEc2VolumeAttachment'
  { -- | The identifier of the EC2 instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The attachment state of the volume.
    status :: Prelude.Maybe Prelude.Text,
    -- | Whether the EBS volume is deleted when the EC2 instance is terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
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
-- 'instanceId', 'awsEc2VolumeAttachment_instanceId' - The identifier of the EC2 instance.
--
-- 'status', 'awsEc2VolumeAttachment_status' - The attachment state of the volume.
--
-- 'deleteOnTermination', 'awsEc2VolumeAttachment_deleteOnTermination' - Whether the EBS volume is deleted when the EC2 instance is terminated.
--
-- 'attachTime', 'awsEc2VolumeAttachment_attachTime' - The datetime when the attachment initiated.
newAwsEc2VolumeAttachment ::
  AwsEc2VolumeAttachment
newAwsEc2VolumeAttachment =
  AwsEc2VolumeAttachment'
    { instanceId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      attachTime = Prelude.Nothing
    }

-- | The identifier of the EC2 instance.
awsEc2VolumeAttachment_instanceId :: Lens.Lens' AwsEc2VolumeAttachment (Prelude.Maybe Prelude.Text)
awsEc2VolumeAttachment_instanceId = Lens.lens (\AwsEc2VolumeAttachment' {instanceId} -> instanceId) (\s@AwsEc2VolumeAttachment' {} a -> s {instanceId = a} :: AwsEc2VolumeAttachment)

-- | The attachment state of the volume.
awsEc2VolumeAttachment_status :: Lens.Lens' AwsEc2VolumeAttachment (Prelude.Maybe Prelude.Text)
awsEc2VolumeAttachment_status = Lens.lens (\AwsEc2VolumeAttachment' {status} -> status) (\s@AwsEc2VolumeAttachment' {} a -> s {status = a} :: AwsEc2VolumeAttachment)

-- | Whether the EBS volume is deleted when the EC2 instance is terminated.
awsEc2VolumeAttachment_deleteOnTermination :: Lens.Lens' AwsEc2VolumeAttachment (Prelude.Maybe Prelude.Bool)
awsEc2VolumeAttachment_deleteOnTermination = Lens.lens (\AwsEc2VolumeAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@AwsEc2VolumeAttachment' {} a -> s {deleteOnTermination = a} :: AwsEc2VolumeAttachment)

-- | The datetime when the attachment initiated.
awsEc2VolumeAttachment_attachTime :: Lens.Lens' AwsEc2VolumeAttachment (Prelude.Maybe Prelude.Text)
awsEc2VolumeAttachment_attachTime = Lens.lens (\AwsEc2VolumeAttachment' {attachTime} -> attachTime) (\s@AwsEc2VolumeAttachment' {} a -> s {attachTime = a} :: AwsEc2VolumeAttachment)

instance Core.FromJSON AwsEc2VolumeAttachment where
  parseJSON =
    Core.withObject
      "AwsEc2VolumeAttachment"
      ( \x ->
          AwsEc2VolumeAttachment'
            Prelude.<$> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "DeleteOnTermination")
            Prelude.<*> (x Core..:? "AttachTime")
      )

instance Prelude.Hashable AwsEc2VolumeAttachment

instance Prelude.NFData AwsEc2VolumeAttachment

instance Core.ToJSON AwsEc2VolumeAttachment where
  toJSON AwsEc2VolumeAttachment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InstanceId" Core..=) Prelude.<$> instanceId,
            ("Status" Core..=) Prelude.<$> status,
            ("DeleteOnTermination" Core..=)
              Prelude.<$> deleteOnTermination,
            ("AttachTime" Core..=) Prelude.<$> attachTime
          ]
      )
