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
-- Module      : Amazonka.IotTwinMaker.Types.BundleInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.BundleInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.PricingTier
import qualified Amazonka.Prelude as Prelude

-- | Information about pricing bundle.
--
-- /See:/ 'newBundleInformation' smart constructor.
data BundleInformation = BundleInformation'
  { -- | The pricing tier.
    pricingTier :: Prelude.Maybe PricingTier,
    -- | The bundle names.
    bundleNames :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BundleInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pricingTier', 'bundleInformation_pricingTier' - The pricing tier.
--
-- 'bundleNames', 'bundleInformation_bundleNames' - The bundle names.
newBundleInformation ::
  -- | 'bundleNames'
  Prelude.NonEmpty Prelude.Text ->
  BundleInformation
newBundleInformation pBundleNames_ =
  BundleInformation'
    { pricingTier = Prelude.Nothing,
      bundleNames = Lens.coerced Lens.# pBundleNames_
    }

-- | The pricing tier.
bundleInformation_pricingTier :: Lens.Lens' BundleInformation (Prelude.Maybe PricingTier)
bundleInformation_pricingTier = Lens.lens (\BundleInformation' {pricingTier} -> pricingTier) (\s@BundleInformation' {} a -> s {pricingTier = a} :: BundleInformation)

-- | The bundle names.
bundleInformation_bundleNames :: Lens.Lens' BundleInformation (Prelude.NonEmpty Prelude.Text)
bundleInformation_bundleNames = Lens.lens (\BundleInformation' {bundleNames} -> bundleNames) (\s@BundleInformation' {} a -> s {bundleNames = a} :: BundleInformation) Prelude.. Lens.coerced

instance Data.FromJSON BundleInformation where
  parseJSON =
    Data.withObject
      "BundleInformation"
      ( \x ->
          BundleInformation'
            Prelude.<$> (x Data..:? "pricingTier")
            Prelude.<*> (x Data..: "bundleNames")
      )

instance Prelude.Hashable BundleInformation where
  hashWithSalt _salt BundleInformation' {..} =
    _salt
      `Prelude.hashWithSalt` pricingTier
      `Prelude.hashWithSalt` bundleNames

instance Prelude.NFData BundleInformation where
  rnf BundleInformation' {..} =
    Prelude.rnf pricingTier
      `Prelude.seq` Prelude.rnf bundleNames
