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
-- Module      : Amazonka.EMR.Types.SupportedInstanceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.SupportedInstanceType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An instance type that the specified Amazon EMR release supports.
--
-- /See:/ 'newSupportedInstanceType' smart constructor.
data SupportedInstanceType = SupportedInstanceType'
  { -- | The CPU architecture, for example @X86_64@ or @AARCH64@.
    architecture :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the @SupportedInstanceType@ supports Amazon EBS
    -- optimization.
    ebsOptimizedAvailable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the @SupportedInstanceType@ uses Amazon EBS
    -- optimization by default.
    ebsOptimizedByDefault :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the @SupportedInstanceType@ only supports Amazon EBS.
    ebsStorageOnly :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon EC2 family and generation for the @SupportedInstanceType@.
    instanceFamilyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the @SupportedInstanceType@ only supports 64-bit
    -- architecture.
    is64BitsOnly :: Prelude.Maybe Prelude.Bool,
    -- | The amount of memory that is available to Amazon EMR from the
    -- @SupportedInstanceType@. The kernel and hypervisor software consume some
    -- memory, so this value might be lower than the overall memory for the
    -- instance type.
    memoryGB :: Prelude.Maybe Prelude.Double,
    -- | Number of disks for the @SupportedInstanceType@. This value is @0@ for
    -- Amazon EBS-only instance types.
    numberOfDisks :: Prelude.Maybe Prelude.Int,
    -- | @StorageGB@ represents the storage capacity of the
    -- @SupportedInstanceType@. This value is @0@ for Amazon EBS-only instance
    -- types.
    storageGB :: Prelude.Maybe Prelude.Int,
    -- | The
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 instance type>,
    -- for example @m5.xlarge@, of the @SupportedInstanceType@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The number of vCPUs available for the @SupportedInstanceType@.
    vcpu :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SupportedInstanceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'supportedInstanceType_architecture' - The CPU architecture, for example @X86_64@ or @AARCH64@.
--
-- 'ebsOptimizedAvailable', 'supportedInstanceType_ebsOptimizedAvailable' - Indicates whether the @SupportedInstanceType@ supports Amazon EBS
-- optimization.
--
-- 'ebsOptimizedByDefault', 'supportedInstanceType_ebsOptimizedByDefault' - Indicates whether the @SupportedInstanceType@ uses Amazon EBS
-- optimization by default.
--
-- 'ebsStorageOnly', 'supportedInstanceType_ebsStorageOnly' - Indicates whether the @SupportedInstanceType@ only supports Amazon EBS.
--
-- 'instanceFamilyId', 'supportedInstanceType_instanceFamilyId' - The Amazon EC2 family and generation for the @SupportedInstanceType@.
--
-- 'is64BitsOnly', 'supportedInstanceType_is64BitsOnly' - Indicates whether the @SupportedInstanceType@ only supports 64-bit
-- architecture.
--
-- 'memoryGB', 'supportedInstanceType_memoryGB' - The amount of memory that is available to Amazon EMR from the
-- @SupportedInstanceType@. The kernel and hypervisor software consume some
-- memory, so this value might be lower than the overall memory for the
-- instance type.
--
-- 'numberOfDisks', 'supportedInstanceType_numberOfDisks' - Number of disks for the @SupportedInstanceType@. This value is @0@ for
-- Amazon EBS-only instance types.
--
-- 'storageGB', 'supportedInstanceType_storageGB' - @StorageGB@ represents the storage capacity of the
-- @SupportedInstanceType@. This value is @0@ for Amazon EBS-only instance
-- types.
--
-- 'type'', 'supportedInstanceType_type' - The
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 instance type>,
-- for example @m5.xlarge@, of the @SupportedInstanceType@.
--
-- 'vcpu', 'supportedInstanceType_vcpu' - The number of vCPUs available for the @SupportedInstanceType@.
newSupportedInstanceType ::
  SupportedInstanceType
newSupportedInstanceType =
  SupportedInstanceType'
    { architecture =
        Prelude.Nothing,
      ebsOptimizedAvailable = Prelude.Nothing,
      ebsOptimizedByDefault = Prelude.Nothing,
      ebsStorageOnly = Prelude.Nothing,
      instanceFamilyId = Prelude.Nothing,
      is64BitsOnly = Prelude.Nothing,
      memoryGB = Prelude.Nothing,
      numberOfDisks = Prelude.Nothing,
      storageGB = Prelude.Nothing,
      type' = Prelude.Nothing,
      vcpu = Prelude.Nothing
    }

-- | The CPU architecture, for example @X86_64@ or @AARCH64@.
supportedInstanceType_architecture :: Lens.Lens' SupportedInstanceType (Prelude.Maybe Prelude.Text)
supportedInstanceType_architecture = Lens.lens (\SupportedInstanceType' {architecture} -> architecture) (\s@SupportedInstanceType' {} a -> s {architecture = a} :: SupportedInstanceType)

-- | Indicates whether the @SupportedInstanceType@ supports Amazon EBS
-- optimization.
supportedInstanceType_ebsOptimizedAvailable :: Lens.Lens' SupportedInstanceType (Prelude.Maybe Prelude.Bool)
supportedInstanceType_ebsOptimizedAvailable = Lens.lens (\SupportedInstanceType' {ebsOptimizedAvailable} -> ebsOptimizedAvailable) (\s@SupportedInstanceType' {} a -> s {ebsOptimizedAvailable = a} :: SupportedInstanceType)

-- | Indicates whether the @SupportedInstanceType@ uses Amazon EBS
-- optimization by default.
supportedInstanceType_ebsOptimizedByDefault :: Lens.Lens' SupportedInstanceType (Prelude.Maybe Prelude.Bool)
supportedInstanceType_ebsOptimizedByDefault = Lens.lens (\SupportedInstanceType' {ebsOptimizedByDefault} -> ebsOptimizedByDefault) (\s@SupportedInstanceType' {} a -> s {ebsOptimizedByDefault = a} :: SupportedInstanceType)

-- | Indicates whether the @SupportedInstanceType@ only supports Amazon EBS.
supportedInstanceType_ebsStorageOnly :: Lens.Lens' SupportedInstanceType (Prelude.Maybe Prelude.Bool)
supportedInstanceType_ebsStorageOnly = Lens.lens (\SupportedInstanceType' {ebsStorageOnly} -> ebsStorageOnly) (\s@SupportedInstanceType' {} a -> s {ebsStorageOnly = a} :: SupportedInstanceType)

-- | The Amazon EC2 family and generation for the @SupportedInstanceType@.
supportedInstanceType_instanceFamilyId :: Lens.Lens' SupportedInstanceType (Prelude.Maybe Prelude.Text)
supportedInstanceType_instanceFamilyId = Lens.lens (\SupportedInstanceType' {instanceFamilyId} -> instanceFamilyId) (\s@SupportedInstanceType' {} a -> s {instanceFamilyId = a} :: SupportedInstanceType)

-- | Indicates whether the @SupportedInstanceType@ only supports 64-bit
-- architecture.
supportedInstanceType_is64BitsOnly :: Lens.Lens' SupportedInstanceType (Prelude.Maybe Prelude.Bool)
supportedInstanceType_is64BitsOnly = Lens.lens (\SupportedInstanceType' {is64BitsOnly} -> is64BitsOnly) (\s@SupportedInstanceType' {} a -> s {is64BitsOnly = a} :: SupportedInstanceType)

-- | The amount of memory that is available to Amazon EMR from the
-- @SupportedInstanceType@. The kernel and hypervisor software consume some
-- memory, so this value might be lower than the overall memory for the
-- instance type.
supportedInstanceType_memoryGB :: Lens.Lens' SupportedInstanceType (Prelude.Maybe Prelude.Double)
supportedInstanceType_memoryGB = Lens.lens (\SupportedInstanceType' {memoryGB} -> memoryGB) (\s@SupportedInstanceType' {} a -> s {memoryGB = a} :: SupportedInstanceType)

-- | Number of disks for the @SupportedInstanceType@. This value is @0@ for
-- Amazon EBS-only instance types.
supportedInstanceType_numberOfDisks :: Lens.Lens' SupportedInstanceType (Prelude.Maybe Prelude.Int)
supportedInstanceType_numberOfDisks = Lens.lens (\SupportedInstanceType' {numberOfDisks} -> numberOfDisks) (\s@SupportedInstanceType' {} a -> s {numberOfDisks = a} :: SupportedInstanceType)

-- | @StorageGB@ represents the storage capacity of the
-- @SupportedInstanceType@. This value is @0@ for Amazon EBS-only instance
-- types.
supportedInstanceType_storageGB :: Lens.Lens' SupportedInstanceType (Prelude.Maybe Prelude.Int)
supportedInstanceType_storageGB = Lens.lens (\SupportedInstanceType' {storageGB} -> storageGB) (\s@SupportedInstanceType' {} a -> s {storageGB = a} :: SupportedInstanceType)

-- | The
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 instance type>,
-- for example @m5.xlarge@, of the @SupportedInstanceType@.
supportedInstanceType_type :: Lens.Lens' SupportedInstanceType (Prelude.Maybe Prelude.Text)
supportedInstanceType_type = Lens.lens (\SupportedInstanceType' {type'} -> type') (\s@SupportedInstanceType' {} a -> s {type' = a} :: SupportedInstanceType)

-- | The number of vCPUs available for the @SupportedInstanceType@.
supportedInstanceType_vcpu :: Lens.Lens' SupportedInstanceType (Prelude.Maybe Prelude.Int)
supportedInstanceType_vcpu = Lens.lens (\SupportedInstanceType' {vcpu} -> vcpu) (\s@SupportedInstanceType' {} a -> s {vcpu = a} :: SupportedInstanceType)

instance Data.FromJSON SupportedInstanceType where
  parseJSON =
    Data.withObject
      "SupportedInstanceType"
      ( \x ->
          SupportedInstanceType'
            Prelude.<$> (x Data..:? "Architecture")
            Prelude.<*> (x Data..:? "EbsOptimizedAvailable")
            Prelude.<*> (x Data..:? "EbsOptimizedByDefault")
            Prelude.<*> (x Data..:? "EbsStorageOnly")
            Prelude.<*> (x Data..:? "InstanceFamilyId")
            Prelude.<*> (x Data..:? "Is64BitsOnly")
            Prelude.<*> (x Data..:? "MemoryGB")
            Prelude.<*> (x Data..:? "NumberOfDisks")
            Prelude.<*> (x Data..:? "StorageGB")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "VCPU")
      )

instance Prelude.Hashable SupportedInstanceType where
  hashWithSalt _salt SupportedInstanceType' {..} =
    _salt
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` ebsOptimizedAvailable
      `Prelude.hashWithSalt` ebsOptimizedByDefault
      `Prelude.hashWithSalt` ebsStorageOnly
      `Prelude.hashWithSalt` instanceFamilyId
      `Prelude.hashWithSalt` is64BitsOnly
      `Prelude.hashWithSalt` memoryGB
      `Prelude.hashWithSalt` numberOfDisks
      `Prelude.hashWithSalt` storageGB
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` vcpu

instance Prelude.NFData SupportedInstanceType where
  rnf SupportedInstanceType' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf ebsOptimizedAvailable
      `Prelude.seq` Prelude.rnf ebsOptimizedByDefault
      `Prelude.seq` Prelude.rnf ebsStorageOnly
      `Prelude.seq` Prelude.rnf instanceFamilyId
      `Prelude.seq` Prelude.rnf is64BitsOnly
      `Prelude.seq` Prelude.rnf memoryGB
      `Prelude.seq` Prelude.rnf numberOfDisks
      `Prelude.seq` Prelude.rnf storageGB
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf vcpu
