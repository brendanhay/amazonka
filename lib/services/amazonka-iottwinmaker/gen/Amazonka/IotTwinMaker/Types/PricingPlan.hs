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
-- Module      : Amazonka.IotTwinMaker.Types.PricingPlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PricingPlan where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.BundleInformation
import Amazonka.IotTwinMaker.Types.PricingMode
import Amazonka.IotTwinMaker.Types.UpdateReason
import qualified Amazonka.Prelude as Prelude

-- | The pricing plan.
--
-- /See:/ 'newPricingPlan' smart constructor.
data PricingPlan = PricingPlan'
  { -- | The pricing plan\'s bundle information.
    bundleInformation :: Prelude.Maybe BundleInformation,
    -- | The billable entity count.
    billableEntityCount :: Prelude.Maybe Prelude.Integer,
    -- | The effective date and time of the pricing plan.
    effectiveDateTime :: Data.POSIX,
    -- | The pricing mode.
    pricingMode :: PricingMode,
    -- | The set date and time for updating a pricing plan.
    updateDateTime :: Data.POSIX,
    -- | The update reason, for changing a pricing plan.
    updateReason :: UpdateReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PricingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleInformation', 'pricingPlan_bundleInformation' - The pricing plan\'s bundle information.
--
-- 'billableEntityCount', 'pricingPlan_billableEntityCount' - The billable entity count.
--
-- 'effectiveDateTime', 'pricingPlan_effectiveDateTime' - The effective date and time of the pricing plan.
--
-- 'pricingMode', 'pricingPlan_pricingMode' - The pricing mode.
--
-- 'updateDateTime', 'pricingPlan_updateDateTime' - The set date and time for updating a pricing plan.
--
-- 'updateReason', 'pricingPlan_updateReason' - The update reason, for changing a pricing plan.
newPricingPlan ::
  -- | 'effectiveDateTime'
  Prelude.UTCTime ->
  -- | 'pricingMode'
  PricingMode ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  -- | 'updateReason'
  UpdateReason ->
  PricingPlan
newPricingPlan
  pEffectiveDateTime_
  pPricingMode_
  pUpdateDateTime_
  pUpdateReason_ =
    PricingPlan'
      { bundleInformation = Prelude.Nothing,
        billableEntityCount = Prelude.Nothing,
        effectiveDateTime =
          Data._Time Lens.# pEffectiveDateTime_,
        pricingMode = pPricingMode_,
        updateDateTime = Data._Time Lens.# pUpdateDateTime_,
        updateReason = pUpdateReason_
      }

-- | The pricing plan\'s bundle information.
pricingPlan_bundleInformation :: Lens.Lens' PricingPlan (Prelude.Maybe BundleInformation)
pricingPlan_bundleInformation = Lens.lens (\PricingPlan' {bundleInformation} -> bundleInformation) (\s@PricingPlan' {} a -> s {bundleInformation = a} :: PricingPlan)

-- | The billable entity count.
pricingPlan_billableEntityCount :: Lens.Lens' PricingPlan (Prelude.Maybe Prelude.Integer)
pricingPlan_billableEntityCount = Lens.lens (\PricingPlan' {billableEntityCount} -> billableEntityCount) (\s@PricingPlan' {} a -> s {billableEntityCount = a} :: PricingPlan)

-- | The effective date and time of the pricing plan.
pricingPlan_effectiveDateTime :: Lens.Lens' PricingPlan Prelude.UTCTime
pricingPlan_effectiveDateTime = Lens.lens (\PricingPlan' {effectiveDateTime} -> effectiveDateTime) (\s@PricingPlan' {} a -> s {effectiveDateTime = a} :: PricingPlan) Prelude.. Data._Time

-- | The pricing mode.
pricingPlan_pricingMode :: Lens.Lens' PricingPlan PricingMode
pricingPlan_pricingMode = Lens.lens (\PricingPlan' {pricingMode} -> pricingMode) (\s@PricingPlan' {} a -> s {pricingMode = a} :: PricingPlan)

-- | The set date and time for updating a pricing plan.
pricingPlan_updateDateTime :: Lens.Lens' PricingPlan Prelude.UTCTime
pricingPlan_updateDateTime = Lens.lens (\PricingPlan' {updateDateTime} -> updateDateTime) (\s@PricingPlan' {} a -> s {updateDateTime = a} :: PricingPlan) Prelude.. Data._Time

-- | The update reason, for changing a pricing plan.
pricingPlan_updateReason :: Lens.Lens' PricingPlan UpdateReason
pricingPlan_updateReason = Lens.lens (\PricingPlan' {updateReason} -> updateReason) (\s@PricingPlan' {} a -> s {updateReason = a} :: PricingPlan)

instance Data.FromJSON PricingPlan where
  parseJSON =
    Data.withObject
      "PricingPlan"
      ( \x ->
          PricingPlan'
            Prelude.<$> (x Data..:? "bundleInformation")
            Prelude.<*> (x Data..:? "billableEntityCount")
            Prelude.<*> (x Data..: "effectiveDateTime")
            Prelude.<*> (x Data..: "pricingMode")
            Prelude.<*> (x Data..: "updateDateTime")
            Prelude.<*> (x Data..: "updateReason")
      )

instance Prelude.Hashable PricingPlan where
  hashWithSalt _salt PricingPlan' {..} =
    _salt `Prelude.hashWithSalt` bundleInformation
      `Prelude.hashWithSalt` billableEntityCount
      `Prelude.hashWithSalt` effectiveDateTime
      `Prelude.hashWithSalt` pricingMode
      `Prelude.hashWithSalt` updateDateTime
      `Prelude.hashWithSalt` updateReason

instance Prelude.NFData PricingPlan where
  rnf PricingPlan' {..} =
    Prelude.rnf bundleInformation
      `Prelude.seq` Prelude.rnf billableEntityCount
      `Prelude.seq` Prelude.rnf effectiveDateTime
      `Prelude.seq` Prelude.rnf pricingMode
      `Prelude.seq` Prelude.rnf updateDateTime
      `Prelude.seq` Prelude.rnf updateReason
