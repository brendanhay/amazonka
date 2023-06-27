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
-- Module      : Amazonka.RDS.Types.ValidStorageOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.ValidStorageOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.DoubleRange
import Amazonka.RDS.Types.Range

-- | Information about valid modifications that you can make to your DB
-- instance. Contains the result of a successful call to the
-- @DescribeValidDBInstanceModifications@ action.
--
-- /See:/ 'newValidStorageOptions' smart constructor.
data ValidStorageOptions = ValidStorageOptions'
  { -- | The valid range of Provisioned IOPS to gibibytes of storage multiplier.
    -- For example, 3-10, which means that provisioned IOPS can be between 3
    -- and 10 times storage.
    iopsToStorageRatio :: Prelude.Maybe [DoubleRange],
    -- | The valid range of provisioned IOPS. For example, 1000-256,000.
    provisionedIops :: Prelude.Maybe [Range],
    -- | The valid range of provisioned storage throughput. For example,
    -- 500-4,000 mebibytes per second (MiBps).
    provisionedStorageThroughput :: Prelude.Maybe [Range],
    -- | The valid range of storage in gibibytes (GiB). For example, 100 to
    -- 16,384.
    storageSize :: Prelude.Maybe [Range],
    -- | The valid range of storage throughput to provisioned IOPS ratios. For
    -- example, 0-0.25.
    storageThroughputToIopsRatio :: Prelude.Maybe [DoubleRange],
    -- | The valid storage types for your DB instance. For example: gp2, gp3,
    -- io1.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | Whether or not Amazon RDS can automatically scale storage for DB
    -- instances that use the new instance class.
    supportsStorageAutoscaling :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidStorageOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iopsToStorageRatio', 'validStorageOptions_iopsToStorageRatio' - The valid range of Provisioned IOPS to gibibytes of storage multiplier.
-- For example, 3-10, which means that provisioned IOPS can be between 3
-- and 10 times storage.
--
-- 'provisionedIops', 'validStorageOptions_provisionedIops' - The valid range of provisioned IOPS. For example, 1000-256,000.
--
-- 'provisionedStorageThroughput', 'validStorageOptions_provisionedStorageThroughput' - The valid range of provisioned storage throughput. For example,
-- 500-4,000 mebibytes per second (MiBps).
--
-- 'storageSize', 'validStorageOptions_storageSize' - The valid range of storage in gibibytes (GiB). For example, 100 to
-- 16,384.
--
-- 'storageThroughputToIopsRatio', 'validStorageOptions_storageThroughputToIopsRatio' - The valid range of storage throughput to provisioned IOPS ratios. For
-- example, 0-0.25.
--
-- 'storageType', 'validStorageOptions_storageType' - The valid storage types for your DB instance. For example: gp2, gp3,
-- io1.
--
-- 'supportsStorageAutoscaling', 'validStorageOptions_supportsStorageAutoscaling' - Whether or not Amazon RDS can automatically scale storage for DB
-- instances that use the new instance class.
newValidStorageOptions ::
  ValidStorageOptions
newValidStorageOptions =
  ValidStorageOptions'
    { iopsToStorageRatio =
        Prelude.Nothing,
      provisionedIops = Prelude.Nothing,
      provisionedStorageThroughput = Prelude.Nothing,
      storageSize = Prelude.Nothing,
      storageThroughputToIopsRatio = Prelude.Nothing,
      storageType = Prelude.Nothing,
      supportsStorageAutoscaling = Prelude.Nothing
    }

-- | The valid range of Provisioned IOPS to gibibytes of storage multiplier.
-- For example, 3-10, which means that provisioned IOPS can be between 3
-- and 10 times storage.
validStorageOptions_iopsToStorageRatio :: Lens.Lens' ValidStorageOptions (Prelude.Maybe [DoubleRange])
validStorageOptions_iopsToStorageRatio = Lens.lens (\ValidStorageOptions' {iopsToStorageRatio} -> iopsToStorageRatio) (\s@ValidStorageOptions' {} a -> s {iopsToStorageRatio = a} :: ValidStorageOptions) Prelude.. Lens.mapping Lens.coerced

-- | The valid range of provisioned IOPS. For example, 1000-256,000.
validStorageOptions_provisionedIops :: Lens.Lens' ValidStorageOptions (Prelude.Maybe [Range])
validStorageOptions_provisionedIops = Lens.lens (\ValidStorageOptions' {provisionedIops} -> provisionedIops) (\s@ValidStorageOptions' {} a -> s {provisionedIops = a} :: ValidStorageOptions) Prelude.. Lens.mapping Lens.coerced

-- | The valid range of provisioned storage throughput. For example,
-- 500-4,000 mebibytes per second (MiBps).
validStorageOptions_provisionedStorageThroughput :: Lens.Lens' ValidStorageOptions (Prelude.Maybe [Range])
validStorageOptions_provisionedStorageThroughput = Lens.lens (\ValidStorageOptions' {provisionedStorageThroughput} -> provisionedStorageThroughput) (\s@ValidStorageOptions' {} a -> s {provisionedStorageThroughput = a} :: ValidStorageOptions) Prelude.. Lens.mapping Lens.coerced

-- | The valid range of storage in gibibytes (GiB). For example, 100 to
-- 16,384.
validStorageOptions_storageSize :: Lens.Lens' ValidStorageOptions (Prelude.Maybe [Range])
validStorageOptions_storageSize = Lens.lens (\ValidStorageOptions' {storageSize} -> storageSize) (\s@ValidStorageOptions' {} a -> s {storageSize = a} :: ValidStorageOptions) Prelude.. Lens.mapping Lens.coerced

-- | The valid range of storage throughput to provisioned IOPS ratios. For
-- example, 0-0.25.
validStorageOptions_storageThroughputToIopsRatio :: Lens.Lens' ValidStorageOptions (Prelude.Maybe [DoubleRange])
validStorageOptions_storageThroughputToIopsRatio = Lens.lens (\ValidStorageOptions' {storageThroughputToIopsRatio} -> storageThroughputToIopsRatio) (\s@ValidStorageOptions' {} a -> s {storageThroughputToIopsRatio = a} :: ValidStorageOptions) Prelude.. Lens.mapping Lens.coerced

-- | The valid storage types for your DB instance. For example: gp2, gp3,
-- io1.
validStorageOptions_storageType :: Lens.Lens' ValidStorageOptions (Prelude.Maybe Prelude.Text)
validStorageOptions_storageType = Lens.lens (\ValidStorageOptions' {storageType} -> storageType) (\s@ValidStorageOptions' {} a -> s {storageType = a} :: ValidStorageOptions)

-- | Whether or not Amazon RDS can automatically scale storage for DB
-- instances that use the new instance class.
validStorageOptions_supportsStorageAutoscaling :: Lens.Lens' ValidStorageOptions (Prelude.Maybe Prelude.Bool)
validStorageOptions_supportsStorageAutoscaling = Lens.lens (\ValidStorageOptions' {supportsStorageAutoscaling} -> supportsStorageAutoscaling) (\s@ValidStorageOptions' {} a -> s {supportsStorageAutoscaling = a} :: ValidStorageOptions)

instance Data.FromXML ValidStorageOptions where
  parseXML x =
    ValidStorageOptions'
      Prelude.<$> ( x
                      Data..@? "IopsToStorageRatio"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DoubleRange")
                  )
      Prelude.<*> ( x
                      Data..@? "ProvisionedIops"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Range")
                  )
      Prelude.<*> ( x
                      Data..@? "ProvisionedStorageThroughput"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Range")
                  )
      Prelude.<*> ( x
                      Data..@? "StorageSize"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Range")
                  )
      Prelude.<*> ( x
                      Data..@? "StorageThroughputToIopsRatio"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DoubleRange")
                  )
      Prelude.<*> (x Data..@? "StorageType")
      Prelude.<*> (x Data..@? "SupportsStorageAutoscaling")

instance Prelude.Hashable ValidStorageOptions where
  hashWithSalt _salt ValidStorageOptions' {..} =
    _salt
      `Prelude.hashWithSalt` iopsToStorageRatio
      `Prelude.hashWithSalt` provisionedIops
      `Prelude.hashWithSalt` provisionedStorageThroughput
      `Prelude.hashWithSalt` storageSize
      `Prelude.hashWithSalt` storageThroughputToIopsRatio
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` supportsStorageAutoscaling

instance Prelude.NFData ValidStorageOptions where
  rnf ValidStorageOptions' {..} =
    Prelude.rnf iopsToStorageRatio
      `Prelude.seq` Prelude.rnf provisionedIops
      `Prelude.seq` Prelude.rnf provisionedStorageThroughput
      `Prelude.seq` Prelude.rnf storageSize
      `Prelude.seq` Prelude.rnf storageThroughputToIopsRatio
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf supportsStorageAutoscaling
