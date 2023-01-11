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
-- Module      : Amazonka.SavingsPlans.Types.ParentSavingsPlanOffering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.ParentSavingsPlanOffering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.CurrencyCode
import Amazonka.SavingsPlans.Types.SavingsPlanPaymentOption
import Amazonka.SavingsPlans.Types.SavingsPlanType

-- | Information about a Savings Plan offering.
--
-- /See:/ 'newParentSavingsPlanOffering' smart constructor.
data ParentSavingsPlanOffering = ParentSavingsPlanOffering'
  { -- | The currency.
    currency :: Prelude.Maybe CurrencyCode,
    -- | The duration, in seconds.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the offering.
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | The payment option.
    paymentOption :: Prelude.Maybe SavingsPlanPaymentOption,
    -- | The description.
    planDescription :: Prelude.Maybe Prelude.Text,
    -- | The plan type.
    planType :: Prelude.Maybe SavingsPlanType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParentSavingsPlanOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currency', 'parentSavingsPlanOffering_currency' - The currency.
--
-- 'durationSeconds', 'parentSavingsPlanOffering_durationSeconds' - The duration, in seconds.
--
-- 'offeringId', 'parentSavingsPlanOffering_offeringId' - The ID of the offering.
--
-- 'paymentOption', 'parentSavingsPlanOffering_paymentOption' - The payment option.
--
-- 'planDescription', 'parentSavingsPlanOffering_planDescription' - The description.
--
-- 'planType', 'parentSavingsPlanOffering_planType' - The plan type.
newParentSavingsPlanOffering ::
  ParentSavingsPlanOffering
newParentSavingsPlanOffering =
  ParentSavingsPlanOffering'
    { currency =
        Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      planDescription = Prelude.Nothing,
      planType = Prelude.Nothing
    }

-- | The currency.
parentSavingsPlanOffering_currency :: Lens.Lens' ParentSavingsPlanOffering (Prelude.Maybe CurrencyCode)
parentSavingsPlanOffering_currency = Lens.lens (\ParentSavingsPlanOffering' {currency} -> currency) (\s@ParentSavingsPlanOffering' {} a -> s {currency = a} :: ParentSavingsPlanOffering)

-- | The duration, in seconds.
parentSavingsPlanOffering_durationSeconds :: Lens.Lens' ParentSavingsPlanOffering (Prelude.Maybe Prelude.Natural)
parentSavingsPlanOffering_durationSeconds = Lens.lens (\ParentSavingsPlanOffering' {durationSeconds} -> durationSeconds) (\s@ParentSavingsPlanOffering' {} a -> s {durationSeconds = a} :: ParentSavingsPlanOffering)

-- | The ID of the offering.
parentSavingsPlanOffering_offeringId :: Lens.Lens' ParentSavingsPlanOffering (Prelude.Maybe Prelude.Text)
parentSavingsPlanOffering_offeringId = Lens.lens (\ParentSavingsPlanOffering' {offeringId} -> offeringId) (\s@ParentSavingsPlanOffering' {} a -> s {offeringId = a} :: ParentSavingsPlanOffering)

-- | The payment option.
parentSavingsPlanOffering_paymentOption :: Lens.Lens' ParentSavingsPlanOffering (Prelude.Maybe SavingsPlanPaymentOption)
parentSavingsPlanOffering_paymentOption = Lens.lens (\ParentSavingsPlanOffering' {paymentOption} -> paymentOption) (\s@ParentSavingsPlanOffering' {} a -> s {paymentOption = a} :: ParentSavingsPlanOffering)

-- | The description.
parentSavingsPlanOffering_planDescription :: Lens.Lens' ParentSavingsPlanOffering (Prelude.Maybe Prelude.Text)
parentSavingsPlanOffering_planDescription = Lens.lens (\ParentSavingsPlanOffering' {planDescription} -> planDescription) (\s@ParentSavingsPlanOffering' {} a -> s {planDescription = a} :: ParentSavingsPlanOffering)

-- | The plan type.
parentSavingsPlanOffering_planType :: Lens.Lens' ParentSavingsPlanOffering (Prelude.Maybe SavingsPlanType)
parentSavingsPlanOffering_planType = Lens.lens (\ParentSavingsPlanOffering' {planType} -> planType) (\s@ParentSavingsPlanOffering' {} a -> s {planType = a} :: ParentSavingsPlanOffering)

instance Data.FromJSON ParentSavingsPlanOffering where
  parseJSON =
    Data.withObject
      "ParentSavingsPlanOffering"
      ( \x ->
          ParentSavingsPlanOffering'
            Prelude.<$> (x Data..:? "currency")
            Prelude.<*> (x Data..:? "durationSeconds")
            Prelude.<*> (x Data..:? "offeringId")
            Prelude.<*> (x Data..:? "paymentOption")
            Prelude.<*> (x Data..:? "planDescription")
            Prelude.<*> (x Data..:? "planType")
      )

instance Prelude.Hashable ParentSavingsPlanOffering where
  hashWithSalt _salt ParentSavingsPlanOffering' {..} =
    _salt `Prelude.hashWithSalt` currency
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` offeringId
      `Prelude.hashWithSalt` paymentOption
      `Prelude.hashWithSalt` planDescription
      `Prelude.hashWithSalt` planType

instance Prelude.NFData ParentSavingsPlanOffering where
  rnf ParentSavingsPlanOffering' {..} =
    Prelude.rnf currency
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf offeringId
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf planDescription
      `Prelude.seq` Prelude.rnf planType
