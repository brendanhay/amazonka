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
-- Module      : Amazonka.Lightsail.Types.DistributionBundle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DistributionBundle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the specifications of a distribution bundle.
--
-- /See:/ 'newDistributionBundle' smart constructor.
data DistributionBundle = DistributionBundle'
  { -- | The monthly network transfer quota of the bundle.
    transferPerMonthInGb :: Prelude.Maybe Prelude.Int,
    -- | The ID of the bundle.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The name of the distribution bundle.
    name :: Prelude.Maybe Prelude.Text,
    -- | The monthly price, in US dollars, of the bundle.
    price :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether the bundle is active, and can be specified for a new
    -- or existing distribution.
    isActive :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DistributionBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transferPerMonthInGb', 'distributionBundle_transferPerMonthInGb' - The monthly network transfer quota of the bundle.
--
-- 'bundleId', 'distributionBundle_bundleId' - The ID of the bundle.
--
-- 'name', 'distributionBundle_name' - The name of the distribution bundle.
--
-- 'price', 'distributionBundle_price' - The monthly price, in US dollars, of the bundle.
--
-- 'isActive', 'distributionBundle_isActive' - Indicates whether the bundle is active, and can be specified for a new
-- or existing distribution.
newDistributionBundle ::
  DistributionBundle
newDistributionBundle =
  DistributionBundle'
    { transferPerMonthInGb =
        Prelude.Nothing,
      bundleId = Prelude.Nothing,
      name = Prelude.Nothing,
      price = Prelude.Nothing,
      isActive = Prelude.Nothing
    }

-- | The monthly network transfer quota of the bundle.
distributionBundle_transferPerMonthInGb :: Lens.Lens' DistributionBundle (Prelude.Maybe Prelude.Int)
distributionBundle_transferPerMonthInGb = Lens.lens (\DistributionBundle' {transferPerMonthInGb} -> transferPerMonthInGb) (\s@DistributionBundle' {} a -> s {transferPerMonthInGb = a} :: DistributionBundle)

-- | The ID of the bundle.
distributionBundle_bundleId :: Lens.Lens' DistributionBundle (Prelude.Maybe Prelude.Text)
distributionBundle_bundleId = Lens.lens (\DistributionBundle' {bundleId} -> bundleId) (\s@DistributionBundle' {} a -> s {bundleId = a} :: DistributionBundle)

-- | The name of the distribution bundle.
distributionBundle_name :: Lens.Lens' DistributionBundle (Prelude.Maybe Prelude.Text)
distributionBundle_name = Lens.lens (\DistributionBundle' {name} -> name) (\s@DistributionBundle' {} a -> s {name = a} :: DistributionBundle)

-- | The monthly price, in US dollars, of the bundle.
distributionBundle_price :: Lens.Lens' DistributionBundle (Prelude.Maybe Prelude.Double)
distributionBundle_price = Lens.lens (\DistributionBundle' {price} -> price) (\s@DistributionBundle' {} a -> s {price = a} :: DistributionBundle)

-- | Indicates whether the bundle is active, and can be specified for a new
-- or existing distribution.
distributionBundle_isActive :: Lens.Lens' DistributionBundle (Prelude.Maybe Prelude.Bool)
distributionBundle_isActive = Lens.lens (\DistributionBundle' {isActive} -> isActive) (\s@DistributionBundle' {} a -> s {isActive = a} :: DistributionBundle)

instance Core.FromJSON DistributionBundle where
  parseJSON =
    Core.withObject
      "DistributionBundle"
      ( \x ->
          DistributionBundle'
            Prelude.<$> (x Core..:? "transferPerMonthInGb")
            Prelude.<*> (x Core..:? "bundleId")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "price")
            Prelude.<*> (x Core..:? "isActive")
      )

instance Prelude.Hashable DistributionBundle where
  hashWithSalt salt' DistributionBundle' {..} =
    salt' `Prelude.hashWithSalt` isActive
      `Prelude.hashWithSalt` price
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` transferPerMonthInGb

instance Prelude.NFData DistributionBundle where
  rnf DistributionBundle' {..} =
    Prelude.rnf transferPerMonthInGb
      `Prelude.seq` Prelude.rnf isActive
      `Prelude.seq` Prelude.rnf price
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf bundleId
