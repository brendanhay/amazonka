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
-- Module      : Amazonka.Lightsail.Types.BucketBundle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.BucketBundle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the specifications of a bundle that can be applied to an
-- Amazon Lightsail bucket.
--
-- A bucket bundle specifies the monthly cost, storage space, and data
-- transfer quota for a bucket.
--
-- /See:/ 'newBucketBundle' smart constructor.
data BucketBundle = BucketBundle'
  { -- | The storage size of the bundle, in GB.
    storagePerMonthInGb :: Prelude.Maybe Prelude.Int,
    -- | The monthly network transfer quota of the bundle.
    transferPerMonthInGb :: Prelude.Maybe Prelude.Int,
    -- | The ID of the bundle.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The name of the bundle.
    name :: Prelude.Maybe Prelude.Text,
    -- | The monthly price of the bundle, in US dollars.
    price :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether the bundle is active. Use for a new or existing
    -- bucket.
    isActive :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storagePerMonthInGb', 'bucketBundle_storagePerMonthInGb' - The storage size of the bundle, in GB.
--
-- 'transferPerMonthInGb', 'bucketBundle_transferPerMonthInGb' - The monthly network transfer quota of the bundle.
--
-- 'bundleId', 'bucketBundle_bundleId' - The ID of the bundle.
--
-- 'name', 'bucketBundle_name' - The name of the bundle.
--
-- 'price', 'bucketBundle_price' - The monthly price of the bundle, in US dollars.
--
-- 'isActive', 'bucketBundle_isActive' - Indicates whether the bundle is active. Use for a new or existing
-- bucket.
newBucketBundle ::
  BucketBundle
newBucketBundle =
  BucketBundle'
    { storagePerMonthInGb =
        Prelude.Nothing,
      transferPerMonthInGb = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      name = Prelude.Nothing,
      price = Prelude.Nothing,
      isActive = Prelude.Nothing
    }

-- | The storage size of the bundle, in GB.
bucketBundle_storagePerMonthInGb :: Lens.Lens' BucketBundle (Prelude.Maybe Prelude.Int)
bucketBundle_storagePerMonthInGb = Lens.lens (\BucketBundle' {storagePerMonthInGb} -> storagePerMonthInGb) (\s@BucketBundle' {} a -> s {storagePerMonthInGb = a} :: BucketBundle)

-- | The monthly network transfer quota of the bundle.
bucketBundle_transferPerMonthInGb :: Lens.Lens' BucketBundle (Prelude.Maybe Prelude.Int)
bucketBundle_transferPerMonthInGb = Lens.lens (\BucketBundle' {transferPerMonthInGb} -> transferPerMonthInGb) (\s@BucketBundle' {} a -> s {transferPerMonthInGb = a} :: BucketBundle)

-- | The ID of the bundle.
bucketBundle_bundleId :: Lens.Lens' BucketBundle (Prelude.Maybe Prelude.Text)
bucketBundle_bundleId = Lens.lens (\BucketBundle' {bundleId} -> bundleId) (\s@BucketBundle' {} a -> s {bundleId = a} :: BucketBundle)

-- | The name of the bundle.
bucketBundle_name :: Lens.Lens' BucketBundle (Prelude.Maybe Prelude.Text)
bucketBundle_name = Lens.lens (\BucketBundle' {name} -> name) (\s@BucketBundle' {} a -> s {name = a} :: BucketBundle)

-- | The monthly price of the bundle, in US dollars.
bucketBundle_price :: Lens.Lens' BucketBundle (Prelude.Maybe Prelude.Double)
bucketBundle_price = Lens.lens (\BucketBundle' {price} -> price) (\s@BucketBundle' {} a -> s {price = a} :: BucketBundle)

-- | Indicates whether the bundle is active. Use for a new or existing
-- bucket.
bucketBundle_isActive :: Lens.Lens' BucketBundle (Prelude.Maybe Prelude.Bool)
bucketBundle_isActive = Lens.lens (\BucketBundle' {isActive} -> isActive) (\s@BucketBundle' {} a -> s {isActive = a} :: BucketBundle)

instance Core.FromJSON BucketBundle where
  parseJSON =
    Core.withObject
      "BucketBundle"
      ( \x ->
          BucketBundle'
            Prelude.<$> (x Core..:? "storagePerMonthInGb")
            Prelude.<*> (x Core..:? "transferPerMonthInGb")
            Prelude.<*> (x Core..:? "bundleId")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "price")
            Prelude.<*> (x Core..:? "isActive")
      )

instance Prelude.Hashable BucketBundle where
  hashWithSalt _salt BucketBundle' {..} =
    _salt `Prelude.hashWithSalt` storagePerMonthInGb
      `Prelude.hashWithSalt` transferPerMonthInGb
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` price
      `Prelude.hashWithSalt` isActive

instance Prelude.NFData BucketBundle where
  rnf BucketBundle' {..} =
    Prelude.rnf storagePerMonthInGb
      `Prelude.seq` Prelude.rnf transferPerMonthInGb
      `Prelude.seq` Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf price
      `Prelude.seq` Prelude.rnf isActive
