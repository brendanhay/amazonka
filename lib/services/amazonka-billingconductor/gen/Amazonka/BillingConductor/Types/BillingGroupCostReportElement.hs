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
-- Module      : Amazonka.BillingConductor.Types.BillingGroupCostReportElement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.BillingGroupCostReportElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary report of actual Amazon Web Services charges and calculated
-- Amazon Web Services charges, based on the associated pricing plan of a
-- billing group.
--
-- /See:/ 'newBillingGroupCostReportElement' smart constructor.
data BillingGroupCostReportElement = BillingGroupCostReportElement'
  { -- | The hypothetical Amazon Web Services charges based on the associated
    -- pricing plan of a billing group.
    proformaCost :: Prelude.Maybe Prelude.Text,
    -- | The percentage of billing group margin.
    marginPercentage :: Prelude.Maybe Prelude.Text,
    -- | The actual Amazon Web Services charges for the billing group.
    aWSCost :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a billing group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The displayed currency.
    currency :: Prelude.Maybe Prelude.Text,
    -- | The billing group margin.
    margin :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BillingGroupCostReportElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proformaCost', 'billingGroupCostReportElement_proformaCost' - The hypothetical Amazon Web Services charges based on the associated
-- pricing plan of a billing group.
--
-- 'marginPercentage', 'billingGroupCostReportElement_marginPercentage' - The percentage of billing group margin.
--
-- 'aWSCost', 'billingGroupCostReportElement_aWSCost' - The actual Amazon Web Services charges for the billing group.
--
-- 'arn', 'billingGroupCostReportElement_arn' - The Amazon Resource Name (ARN) of a billing group.
--
-- 'currency', 'billingGroupCostReportElement_currency' - The displayed currency.
--
-- 'margin', 'billingGroupCostReportElement_margin' - The billing group margin.
newBillingGroupCostReportElement ::
  BillingGroupCostReportElement
newBillingGroupCostReportElement =
  BillingGroupCostReportElement'
    { proformaCost =
        Prelude.Nothing,
      marginPercentage = Prelude.Nothing,
      aWSCost = Prelude.Nothing,
      arn = Prelude.Nothing,
      currency = Prelude.Nothing,
      margin = Prelude.Nothing
    }

-- | The hypothetical Amazon Web Services charges based on the associated
-- pricing plan of a billing group.
billingGroupCostReportElement_proformaCost :: Lens.Lens' BillingGroupCostReportElement (Prelude.Maybe Prelude.Text)
billingGroupCostReportElement_proformaCost = Lens.lens (\BillingGroupCostReportElement' {proformaCost} -> proformaCost) (\s@BillingGroupCostReportElement' {} a -> s {proformaCost = a} :: BillingGroupCostReportElement)

-- | The percentage of billing group margin.
billingGroupCostReportElement_marginPercentage :: Lens.Lens' BillingGroupCostReportElement (Prelude.Maybe Prelude.Text)
billingGroupCostReportElement_marginPercentage = Lens.lens (\BillingGroupCostReportElement' {marginPercentage} -> marginPercentage) (\s@BillingGroupCostReportElement' {} a -> s {marginPercentage = a} :: BillingGroupCostReportElement)

-- | The actual Amazon Web Services charges for the billing group.
billingGroupCostReportElement_aWSCost :: Lens.Lens' BillingGroupCostReportElement (Prelude.Maybe Prelude.Text)
billingGroupCostReportElement_aWSCost = Lens.lens (\BillingGroupCostReportElement' {aWSCost} -> aWSCost) (\s@BillingGroupCostReportElement' {} a -> s {aWSCost = a} :: BillingGroupCostReportElement)

-- | The Amazon Resource Name (ARN) of a billing group.
billingGroupCostReportElement_arn :: Lens.Lens' BillingGroupCostReportElement (Prelude.Maybe Prelude.Text)
billingGroupCostReportElement_arn = Lens.lens (\BillingGroupCostReportElement' {arn} -> arn) (\s@BillingGroupCostReportElement' {} a -> s {arn = a} :: BillingGroupCostReportElement)

-- | The displayed currency.
billingGroupCostReportElement_currency :: Lens.Lens' BillingGroupCostReportElement (Prelude.Maybe Prelude.Text)
billingGroupCostReportElement_currency = Lens.lens (\BillingGroupCostReportElement' {currency} -> currency) (\s@BillingGroupCostReportElement' {} a -> s {currency = a} :: BillingGroupCostReportElement)

-- | The billing group margin.
billingGroupCostReportElement_margin :: Lens.Lens' BillingGroupCostReportElement (Prelude.Maybe Prelude.Text)
billingGroupCostReportElement_margin = Lens.lens (\BillingGroupCostReportElement' {margin} -> margin) (\s@BillingGroupCostReportElement' {} a -> s {margin = a} :: BillingGroupCostReportElement)

instance Data.FromJSON BillingGroupCostReportElement where
  parseJSON =
    Data.withObject
      "BillingGroupCostReportElement"
      ( \x ->
          BillingGroupCostReportElement'
            Prelude.<$> (x Data..:? "ProformaCost")
            Prelude.<*> (x Data..:? "MarginPercentage")
            Prelude.<*> (x Data..:? "AWSCost")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Currency")
            Prelude.<*> (x Data..:? "Margin")
      )

instance
  Prelude.Hashable
    BillingGroupCostReportElement
  where
  hashWithSalt _salt BillingGroupCostReportElement' {..} =
    _salt `Prelude.hashWithSalt` proformaCost
      `Prelude.hashWithSalt` marginPercentage
      `Prelude.hashWithSalt` aWSCost
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` currency
      `Prelude.hashWithSalt` margin

instance Prelude.NFData BillingGroupCostReportElement where
  rnf BillingGroupCostReportElement' {..} =
    Prelude.rnf proformaCost
      `Prelude.seq` Prelude.rnf marginPercentage
      `Prelude.seq` Prelude.rnf aWSCost
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf currency
      `Prelude.seq` Prelude.rnf margin
