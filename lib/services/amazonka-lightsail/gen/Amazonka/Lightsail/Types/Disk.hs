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
-- Module      : Amazonka.Lightsail.Types.Disk
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Disk where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.AddOn
import Amazonka.Lightsail.Types.DiskState
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a block storage disk.
--
-- /See:/ 'newDisk' smart constructor.
data Disk = Disk'
  { -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The Lightsail resource type (e.g., @Disk@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The unique name of the disk.
    name :: Prelude.Maybe Prelude.Text,
    -- | (Deprecated) The number of GB in use by the disk.
    --
    -- In releases prior to November 14, 2017, this parameter was not included
    -- in the API response. It is now deprecated.
    gbInUse :: Prelude.Maybe Prelude.Int,
    -- | The size of the disk in GB.
    sizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The resources to which the disk is attached.
    attachedTo :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the disk.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Describes the status of the disk.
    state :: Prelude.Maybe DiskState,
    -- | The disk path.
    path :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region and Availability Zone where the disk is located.
    location :: Prelude.Maybe ResourceLocation,
    -- | A Boolean value indicating whether the disk is attached.
    isAttached :: Prelude.Maybe Prelude.Bool,
    -- | An array of objects representing the add-ons enabled on the disk.
    addOns :: Prelude.Maybe [AddOn],
    -- | A Boolean value indicating whether this disk is a system disk (has an
    -- operating system loaded on it).
    isSystemDisk :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) The attachment state of the disk.
    --
    -- In releases prior to November 14, 2017, this parameter returned
    -- @attached@ for system disks in the API response. It is now deprecated,
    -- but still included in the response. Use @isAttached@ instead.
    attachmentState :: Prelude.Maybe Prelude.Text,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The input\/output operations per second (IOPS) of the disk.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The date when the disk was created.
    createdAt :: Prelude.Maybe Data.POSIX
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
-- 'tags', 'disk_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'resourceType', 'disk_resourceType' - The Lightsail resource type (e.g., @Disk@).
--
-- 'name', 'disk_name' - The unique name of the disk.
--
-- 'gbInUse', 'disk_gbInUse' - (Deprecated) The number of GB in use by the disk.
--
-- In releases prior to November 14, 2017, this parameter was not included
-- in the API response. It is now deprecated.
--
-- 'sizeInGb', 'disk_sizeInGb' - The size of the disk in GB.
--
-- 'attachedTo', 'disk_attachedTo' - The resources to which the disk is attached.
--
-- 'arn', 'disk_arn' - The Amazon Resource Name (ARN) of the disk.
--
-- 'state', 'disk_state' - Describes the status of the disk.
--
-- 'path', 'disk_path' - The disk path.
--
-- 'location', 'disk_location' - The AWS Region and Availability Zone where the disk is located.
--
-- 'isAttached', 'disk_isAttached' - A Boolean value indicating whether the disk is attached.
--
-- 'addOns', 'disk_addOns' - An array of objects representing the add-ons enabled on the disk.
--
-- 'isSystemDisk', 'disk_isSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an
-- operating system loaded on it).
--
-- 'attachmentState', 'disk_attachmentState' - (Deprecated) The attachment state of the disk.
--
-- In releases prior to November 14, 2017, this parameter returned
-- @attached@ for system disks in the API response. It is now deprecated,
-- but still included in the response. Use @isAttached@ instead.
--
-- 'supportCode', 'disk_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'iops', 'disk_iops' - The input\/output operations per second (IOPS) of the disk.
--
-- 'createdAt', 'disk_createdAt' - The date when the disk was created.
newDisk ::
  Disk
newDisk =
  Disk'
    { tags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      gbInUse = Prelude.Nothing,
      sizeInGb = Prelude.Nothing,
      attachedTo = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      path = Prelude.Nothing,
      location = Prelude.Nothing,
      isAttached = Prelude.Nothing,
      addOns = Prelude.Nothing,
      isSystemDisk = Prelude.Nothing,
      attachmentState = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      iops = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
disk_tags :: Lens.Lens' Disk (Prelude.Maybe [Tag])
disk_tags = Lens.lens (\Disk' {tags} -> tags) (\s@Disk' {} a -> s {tags = a} :: Disk) Prelude.. Lens.mapping Lens.coerced

-- | The Lightsail resource type (e.g., @Disk@).
disk_resourceType :: Lens.Lens' Disk (Prelude.Maybe ResourceType)
disk_resourceType = Lens.lens (\Disk' {resourceType} -> resourceType) (\s@Disk' {} a -> s {resourceType = a} :: Disk)

-- | The unique name of the disk.
disk_name :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_name = Lens.lens (\Disk' {name} -> name) (\s@Disk' {} a -> s {name = a} :: Disk)

-- | (Deprecated) The number of GB in use by the disk.
--
-- In releases prior to November 14, 2017, this parameter was not included
-- in the API response. It is now deprecated.
disk_gbInUse :: Lens.Lens' Disk (Prelude.Maybe Prelude.Int)
disk_gbInUse = Lens.lens (\Disk' {gbInUse} -> gbInUse) (\s@Disk' {} a -> s {gbInUse = a} :: Disk)

-- | The size of the disk in GB.
disk_sizeInGb :: Lens.Lens' Disk (Prelude.Maybe Prelude.Int)
disk_sizeInGb = Lens.lens (\Disk' {sizeInGb} -> sizeInGb) (\s@Disk' {} a -> s {sizeInGb = a} :: Disk)

-- | The resources to which the disk is attached.
disk_attachedTo :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_attachedTo = Lens.lens (\Disk' {attachedTo} -> attachedTo) (\s@Disk' {} a -> s {attachedTo = a} :: Disk)

-- | The Amazon Resource Name (ARN) of the disk.
disk_arn :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_arn = Lens.lens (\Disk' {arn} -> arn) (\s@Disk' {} a -> s {arn = a} :: Disk)

-- | Describes the status of the disk.
disk_state :: Lens.Lens' Disk (Prelude.Maybe DiskState)
disk_state = Lens.lens (\Disk' {state} -> state) (\s@Disk' {} a -> s {state = a} :: Disk)

-- | The disk path.
disk_path :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_path = Lens.lens (\Disk' {path} -> path) (\s@Disk' {} a -> s {path = a} :: Disk)

-- | The AWS Region and Availability Zone where the disk is located.
disk_location :: Lens.Lens' Disk (Prelude.Maybe ResourceLocation)
disk_location = Lens.lens (\Disk' {location} -> location) (\s@Disk' {} a -> s {location = a} :: Disk)

-- | A Boolean value indicating whether the disk is attached.
disk_isAttached :: Lens.Lens' Disk (Prelude.Maybe Prelude.Bool)
disk_isAttached = Lens.lens (\Disk' {isAttached} -> isAttached) (\s@Disk' {} a -> s {isAttached = a} :: Disk)

-- | An array of objects representing the add-ons enabled on the disk.
disk_addOns :: Lens.Lens' Disk (Prelude.Maybe [AddOn])
disk_addOns = Lens.lens (\Disk' {addOns} -> addOns) (\s@Disk' {} a -> s {addOns = a} :: Disk) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value indicating whether this disk is a system disk (has an
-- operating system loaded on it).
disk_isSystemDisk :: Lens.Lens' Disk (Prelude.Maybe Prelude.Bool)
disk_isSystemDisk = Lens.lens (\Disk' {isSystemDisk} -> isSystemDisk) (\s@Disk' {} a -> s {isSystemDisk = a} :: Disk)

-- | (Deprecated) The attachment state of the disk.
--
-- In releases prior to November 14, 2017, this parameter returned
-- @attached@ for system disks in the API response. It is now deprecated,
-- but still included in the response. Use @isAttached@ instead.
disk_attachmentState :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_attachmentState = Lens.lens (\Disk' {attachmentState} -> attachmentState) (\s@Disk' {} a -> s {attachmentState = a} :: Disk)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
disk_supportCode :: Lens.Lens' Disk (Prelude.Maybe Prelude.Text)
disk_supportCode = Lens.lens (\Disk' {supportCode} -> supportCode) (\s@Disk' {} a -> s {supportCode = a} :: Disk)

-- | The input\/output operations per second (IOPS) of the disk.
disk_iops :: Lens.Lens' Disk (Prelude.Maybe Prelude.Int)
disk_iops = Lens.lens (\Disk' {iops} -> iops) (\s@Disk' {} a -> s {iops = a} :: Disk)

-- | The date when the disk was created.
disk_createdAt :: Lens.Lens' Disk (Prelude.Maybe Prelude.UTCTime)
disk_createdAt = Lens.lens (\Disk' {createdAt} -> createdAt) (\s@Disk' {} a -> s {createdAt = a} :: Disk) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Disk where
  parseJSON =
    Data.withObject
      "Disk"
      ( \x ->
          Disk'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "gbInUse")
            Prelude.<*> (x Data..:? "sizeInGb")
            Prelude.<*> (x Data..:? "attachedTo")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "path")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "isAttached")
            Prelude.<*> (x Data..:? "addOns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "isSystemDisk")
            Prelude.<*> (x Data..:? "attachmentState")
            Prelude.<*> (x Data..:? "supportCode")
            Prelude.<*> (x Data..:? "iops")
            Prelude.<*> (x Data..:? "createdAt")
      )

instance Prelude.Hashable Disk where
  hashWithSalt _salt Disk' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` gbInUse
      `Prelude.hashWithSalt` sizeInGb
      `Prelude.hashWithSalt` attachedTo
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` isAttached
      `Prelude.hashWithSalt` addOns
      `Prelude.hashWithSalt` isSystemDisk
      `Prelude.hashWithSalt` attachmentState
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData Disk where
  rnf Disk' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf gbInUse
      `Prelude.seq` Prelude.rnf sizeInGb
      `Prelude.seq` Prelude.rnf attachedTo
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf isAttached
      `Prelude.seq` Prelude.rnf addOns
      `Prelude.seq` Prelude.rnf isSystemDisk
      `Prelude.seq` Prelude.rnf attachmentState
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf createdAt
