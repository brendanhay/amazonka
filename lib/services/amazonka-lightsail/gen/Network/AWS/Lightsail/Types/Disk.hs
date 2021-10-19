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
-- Module      : Network.AWS.Lightsail.Types.Disk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Disk where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AddOn
import Network.AWS.Lightsail.Types.DiskState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes a block storage disk.
--
-- /See:/ 'newDisk' smart constructor.
data Disk = Disk'
  { -- | Describes the status of the disk.
    state :: Prelude.Maybe DiskState,
    -- | The Lightsail resource type (e.g., @Disk@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The Amazon Resource Name (ARN) of the disk.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The disk path.
    path :: Prelude.Maybe Prelude.Text,
    -- | The date when the disk was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The AWS Region and Availability Zone where the disk is located.
    location :: Prelude.Maybe ResourceLocation,
    -- | The input\/output operations per second (IOPS) of the disk.
    iops :: Prelude.Maybe Prelude.Int,
    -- | A Boolean value indicating whether the disk is attached.
    isAttached :: Prelude.Maybe Prelude.Bool,
    -- | An array of objects representing the add-ons enabled on the disk.
    addOns :: Prelude.Maybe [AddOn],
    -- | (Deprecated) The attachment state of the disk.
    --
    -- In releases prior to November 14, 2017, this parameter returned
    -- @attached@ for system disks in the API response. It is now deprecated,
    -- but still included in the response. Use @isAttached@ instead.
    attachmentState :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the disk.
    name :: Prelude.Maybe Prelude.Text,
    -- | The size of the disk in GB.
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether this disk is a system disk (has an
    -- operating system loaded on it).
    isSystemDisk :: Prelude.Maybe Prelude.Bool,
    -- | The resources to which the disk is attached.
    attachedTo :: Prelude.Maybe Prelude.Text,
    -- | (Deprecated) The number of GB in use by the disk.
    --
    -- In releases prior to November 14, 2017, this parameter was not included
    -- in the API response. It is now deprecated.
    gbInUse :: Prelude.Maybe Prelude.Int,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Disk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'disk_state' - Describes the status of the disk.
--
-- 'resourceType', 'disk_resourceType' - The Lightsail resource type (e.g., @Disk@).
--
-- 'arn', 'disk_arn' - The Amazon Resource Name (ARN) of the disk.
--
-- 'path', 'disk_path' - The disk path.
--
-- 'createdAt', 'disk_createdAt' - The date when the disk was created.
--
-- 'location', 'disk_location' - The AWS Region and Availability Zone where the disk is located.
--
-- 'iops', 'disk_iops' - The input\/output operations per second (IOPS) of the disk.
--
-- 'isAttached', 'disk_isAttached' - A Boolean value indicating whether the disk is attached.
--
-- 'addOns', 'disk_addOns' - An array of objects representing the add-ons enabled on the disk.
--
-- 'attachmentState', 'disk_attachmentState' - (Deprecated) The attachment state of the disk.
--
-- In releases prior to November 14, 2017, this parameter returned
-- @attached@ for system disks in the API response. It is now deprecated,
-- but still included in the response. Use @isAttached@ instead.
--
-- 'name', 'disk_name' - The unique name of the disk.
--
-- 'sizeInGb', 'disk_sizeInGb' - The size of the disk in GB.
--
-- 'supportCode', 'disk_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'isSystemDisk', 'disk_isSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an
-- operating system loaded on it).
--
-- 'attachedTo', 'disk_attachedTo' - The resources to which the disk is attached.
--
-- 'gbInUse', 'disk_gbInUse' - (Deprecated) The number of GB in use by the disk.
--
-- In releases prior to November 14, 2017, this parameter was not included
-- in the API response. It is now deprecated.
--
-- 'tags', 'disk_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
newDisk ::
  Disk
newDisk =
  Disk'
    { state = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      arn = Prelude.Nothing,
      path = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      location = Prelude.Nothing,
      iops = Prelude.Nothing,
      isAttached = Prelude.Nothing,
      addOns = Prelude.Nothing,
      attachmentState = Prelude.Nothing,
      name = Prelude.Nothing,
      sizeInGb = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      isSystemDisk = Prelude.Nothing,
      attachedTo = Prelude.Nothing,
      gbInUse = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Describes the status of the disk.
disk_state :: Lens.Lens' Disk (Prelude.Maybe DiskState)
disk_state = Lens.lens (\Disk' {state} -> state) (\s@Disk' {} a -> s {state = a} :: Disk)

-- | The Lightsail resource type (e.g., @Disk@).
disk_resourceType :: Lens.Lens' Disk (Prelude.Maybe ResourceType)
disk_resourceType = Lens.lens (\Disk' {resourceType} -> resourceType) (\s@Disk' {} a -> s {resourceType = a} :: Disk)

-- | The Amazon Resource Name (ARN) of the disk.
disk_arn :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_arn = Lens.lens (\Disk' {arn} -> arn) (\s@Disk' {} a -> s {arn = a} :: Disk)

-- | The disk path.
disk_path :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_path = Lens.lens (\Disk' {path} -> path) (\s@Disk' {} a -> s {path = a} :: Disk)

-- | The date when the disk was created.
disk_createdAt :: Lens.Lens' Disk (Prelude.Maybe Prelude.UTCTime)
disk_createdAt = Lens.lens (\Disk' {createdAt} -> createdAt) (\s@Disk' {} a -> s {createdAt = a} :: Disk) Prelude.. Lens.mapping Core._Time

-- | The AWS Region and Availability Zone where the disk is located.
disk_location :: Lens.Lens' Disk (Prelude.Maybe ResourceLocation)
disk_location = Lens.lens (\Disk' {location} -> location) (\s@Disk' {} a -> s {location = a} :: Disk)

-- | The input\/output operations per second (IOPS) of the disk.
disk_iops :: Lens.Lens' Disk (Prelude.Maybe Prelude.Int)
disk_iops = Lens.lens (\Disk' {iops} -> iops) (\s@Disk' {} a -> s {iops = a} :: Disk)

-- | A Boolean value indicating whether the disk is attached.
disk_isAttached :: Lens.Lens' Disk (Prelude.Maybe Prelude.Bool)
disk_isAttached = Lens.lens (\Disk' {isAttached} -> isAttached) (\s@Disk' {} a -> s {isAttached = a} :: Disk)

-- | An array of objects representing the add-ons enabled on the disk.
disk_addOns :: Lens.Lens' Disk (Prelude.Maybe [AddOn])
disk_addOns = Lens.lens (\Disk' {addOns} -> addOns) (\s@Disk' {} a -> s {addOns = a} :: Disk) Prelude.. Lens.mapping Lens.coerced

-- | (Deprecated) The attachment state of the disk.
--
-- In releases prior to November 14, 2017, this parameter returned
-- @attached@ for system disks in the API response. It is now deprecated,
-- but still included in the response. Use @isAttached@ instead.
disk_attachmentState :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_attachmentState = Lens.lens (\Disk' {attachmentState} -> attachmentState) (\s@Disk' {} a -> s {attachmentState = a} :: Disk)

-- | The unique name of the disk.
disk_name :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_name = Lens.lens (\Disk' {name} -> name) (\s@Disk' {} a -> s {name = a} :: Disk)

-- | The size of the disk in GB.
disk_sizeInGb :: Lens.Lens' Disk (Prelude.Maybe Prelude.Int)
disk_sizeInGb = Lens.lens (\Disk' {sizeInGb} -> sizeInGb) (\s@Disk' {} a -> s {sizeInGb = a} :: Disk)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
disk_supportCode :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_supportCode = Lens.lens (\Disk' {supportCode} -> supportCode) (\s@Disk' {} a -> s {supportCode = a} :: Disk)

-- | A Boolean value indicating whether this disk is a system disk (has an
-- operating system loaded on it).
disk_isSystemDisk :: Lens.Lens' Disk (Prelude.Maybe Prelude.Bool)
disk_isSystemDisk = Lens.lens (\Disk' {isSystemDisk} -> isSystemDisk) (\s@Disk' {} a -> s {isSystemDisk = a} :: Disk)

-- | The resources to which the disk is attached.
disk_attachedTo :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_attachedTo = Lens.lens (\Disk' {attachedTo} -> attachedTo) (\s@Disk' {} a -> s {attachedTo = a} :: Disk)

-- | (Deprecated) The number of GB in use by the disk.
--
-- In releases prior to November 14, 2017, this parameter was not included
-- in the API response. It is now deprecated.
disk_gbInUse :: Lens.Lens' Disk (Prelude.Maybe Prelude.Int)
disk_gbInUse = Lens.lens (\Disk' {gbInUse} -> gbInUse) (\s@Disk' {} a -> s {gbInUse = a} :: Disk)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
disk_tags :: Lens.Lens' Disk (Prelude.Maybe [Tag])
disk_tags = Lens.lens (\Disk' {tags} -> tags) (\s@Disk' {} a -> s {tags = a} :: Disk) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Disk where
  parseJSON =
    Core.withObject
      "Disk"
      ( \x ->
          Disk'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "path")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "iops")
            Prelude.<*> (x Core..:? "isAttached")
            Prelude.<*> (x Core..:? "addOns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "attachmentState")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "sizeInGb")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "isSystemDisk")
            Prelude.<*> (x Core..:? "attachedTo")
            Prelude.<*> (x Core..:? "gbInUse")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Disk

instance Prelude.NFData Disk
