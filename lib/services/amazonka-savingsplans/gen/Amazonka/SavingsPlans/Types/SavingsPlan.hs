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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlan where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.CurrencyCode
import Amazonka.SavingsPlans.Types.SavingsPlanPaymentOption
import Amazonka.SavingsPlans.Types.SavingsPlanProductType
import Amazonka.SavingsPlans.Types.SavingsPlanState
import Amazonka.SavingsPlans.Types.SavingsPlanType

-- | Information about a Savings Plan.
--
-- /See:/ 'newSavingsPlan' smart constructor.
data SavingsPlan = SavingsPlan'
  { -- | The duration of the term, in seconds.
    termDurationInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The plan type.
    savingsPlanType :: Prelude.Maybe SavingsPlanType,
    -- | The state.
    state :: Prelude.Maybe SavingsPlanState,
    -- | The product types.
    productTypes :: Prelude.Maybe [SavingsPlanProductType],
    -- | The start time.
    start :: Prelude.Maybe Prelude.Text,
    -- | The EC2 instance family.
    ec2InstanceFamily :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Savings Plan.
    savingsPlanId :: Prelude.Maybe Prelude.Text,
    -- | The recurring payment amount.
    recurringPaymentAmount :: Prelude.Maybe Prelude.Text,
    -- | The currency.
    currency :: Prelude.Maybe CurrencyCode,
    -- | The end time.
    end :: Prelude.Maybe Prelude.Text,
    -- | The up-front payment amount.
    upfrontPaymentAmount :: Prelude.Maybe Prelude.Text,
    -- | The ID of the offering.
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region.
    region :: Prelude.Maybe Prelude.Text,
    -- | The hourly commitment, in USD.
    commitment :: Prelude.Maybe Prelude.Text,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The payment option.
    paymentOption :: Prelude.Maybe SavingsPlanPaymentOption,
    -- | The Amazon Resource Name (ARN) of the Savings Plan.
    savingsPlanArn :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'termDurationInSeconds', 'savingsPlan_termDurationInSeconds' - The duration of the term, in seconds.
--
-- 'savingsPlanType', 'savingsPlan_savingsPlanType' - The plan type.
--
-- 'state', 'savingsPlan_state' - The state.
--
-- 'productTypes', 'savingsPlan_productTypes' - The product types.
--
-- 'start', 'savingsPlan_start' - The start time.
--
-- 'ec2InstanceFamily', 'savingsPlan_ec2InstanceFamily' - The EC2 instance family.
--
-- 'savingsPlanId', 'savingsPlan_savingsPlanId' - The ID of the Savings Plan.
--
-- 'recurringPaymentAmount', 'savingsPlan_recurringPaymentAmount' - The recurring payment amount.
--
-- 'currency', 'savingsPlan_currency' - The currency.
--
-- 'end', 'savingsPlan_end' - The end time.
--
-- 'upfrontPaymentAmount', 'savingsPlan_upfrontPaymentAmount' - The up-front payment amount.
--
-- 'offeringId', 'savingsPlan_offeringId' - The ID of the offering.
--
-- 'region', 'savingsPlan_region' - The AWS Region.
--
-- 'commitment', 'savingsPlan_commitment' - The hourly commitment, in USD.
--
-- 'description', 'savingsPlan_description' - The description.
--
-- 'paymentOption', 'savingsPlan_paymentOption' - The payment option.
--
-- 'savingsPlanArn', 'savingsPlan_savingsPlanArn' - The Amazon Resource Name (ARN) of the Savings Plan.
--
-- 'tags', 'savingsPlan_tags' - One or more tags.
newSavingsPlan ::
  SavingsPlan
newSavingsPlan =
  SavingsPlan'
    { termDurationInSeconds =
        Prelude.Nothing,
      savingsPlanType = Prelude.Nothing,
      state = Prelude.Nothing,
      productTypes = Prelude.Nothing,
      start = Prelude.Nothing,
      ec2InstanceFamily = Prelude.Nothing,
      savingsPlanId = Prelude.Nothing,
      recurringPaymentAmount = Prelude.Nothing,
      currency = Prelude.Nothing,
      end = Prelude.Nothing,
      upfrontPaymentAmount = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      region = Prelude.Nothing,
      commitment = Prelude.Nothing,
      description = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      savingsPlanArn = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The duration of the term, in seconds.
savingsPlan_termDurationInSeconds :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Integer)
savingsPlan_termDurationInSeconds = Lens.lens (\SavingsPlan' {termDurationInSeconds} -> termDurationInSeconds) (\s@SavingsPlan' {} a -> s {termDurationInSeconds = a} :: SavingsPlan)

-- | The plan type.
savingsPlan_savingsPlanType :: Lens.Lens' SavingsPlan (Prelude.Maybe SavingsPlanType)
savingsPlan_savingsPlanType = Lens.lens (\SavingsPlan' {savingsPlanType} -> savingsPlanType) (\s@SavingsPlan' {} a -> s {savingsPlanType = a} :: SavingsPlan)

-- | The state.
savingsPlan_state :: Lens.Lens' SavingsPlan (Prelude.Maybe SavingsPlanState)
savingsPlan_state = Lens.lens (\SavingsPlan' {state} -> state) (\s@SavingsPlan' {} a -> s {state = a} :: SavingsPlan)

-- | The product types.
savingsPlan_productTypes :: Lens.Lens' SavingsPlan (Prelude.Maybe [SavingsPlanProductType])
savingsPlan_productTypes = Lens.lens (\SavingsPlan' {productTypes} -> productTypes) (\s@SavingsPlan' {} a -> s {productTypes = a} :: SavingsPlan) Prelude.. Lens.mapping Lens.coerced

-- | The start time.
savingsPlan_start :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_start = Lens.lens (\SavingsPlan' {start} -> start) (\s@SavingsPlan' {} a -> s {start = a} :: SavingsPlan)

-- | The EC2 instance family.
savingsPlan_ec2InstanceFamily :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_ec2InstanceFamily = Lens.lens (\SavingsPlan' {ec2InstanceFamily} -> ec2InstanceFamily) (\s@SavingsPlan' {} a -> s {ec2InstanceFamily = a} :: SavingsPlan)

-- | The ID of the Savings Plan.
savingsPlan_savingsPlanId :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_savingsPlanId = Lens.lens (\SavingsPlan' {savingsPlanId} -> savingsPlanId) (\s@SavingsPlan' {} a -> s {savingsPlanId = a} :: SavingsPlan)

-- | The recurring payment amount.
savingsPlan_recurringPaymentAmount :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_recurringPaymentAmount = Lens.lens (\SavingsPlan' {recurringPaymentAmount} -> recurringPaymentAmount) (\s@SavingsPlan' {} a -> s {recurringPaymentAmount = a} :: SavingsPlan)

-- | The currency.
savingsPlan_currency :: Lens.Lens' SavingsPlan (Prelude.Maybe CurrencyCode)
savingsPlan_currency = Lens.lens (\SavingsPlan' {currency} -> currency) (\s@SavingsPlan' {} a -> s {currency = a} :: SavingsPlan)

-- | The end time.
savingsPlan_end :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_end = Lens.lens (\SavingsPlan' {end} -> end) (\s@SavingsPlan' {} a -> s {end = a} :: SavingsPlan)

-- | The up-front payment amount.
savingsPlan_upfrontPaymentAmount :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_upfrontPaymentAmount = Lens.lens (\SavingsPlan' {upfrontPaymentAmount} -> upfrontPaymentAmount) (\s@SavingsPlan' {} a -> s {upfrontPaymentAmount = a} :: SavingsPlan)

-- | The ID of the offering.
savingsPlan_offeringId :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_offeringId = Lens.lens (\SavingsPlan' {offeringId} -> offeringId) (\s@SavingsPlan' {} a -> s {offeringId = a} :: SavingsPlan)

-- | The AWS Region.
savingsPlan_region :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_region = Lens.lens (\SavingsPlan' {region} -> region) (\s@SavingsPlan' {} a -> s {region = a} :: SavingsPlan)

-- | The hourly commitment, in USD.
savingsPlan_commitment :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_commitment = Lens.lens (\SavingsPlan' {commitment} -> commitment) (\s@SavingsPlan' {} a -> s {commitment = a} :: SavingsPlan)

-- | The description.
savingsPlan_description :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_description = Lens.lens (\SavingsPlan' {description} -> description) (\s@SavingsPlan' {} a -> s {description = a} :: SavingsPlan)

-- | The payment option.
savingsPlan_paymentOption :: Lens.Lens' SavingsPlan (Prelude.Maybe SavingsPlanPaymentOption)
savingsPlan_paymentOption = Lens.lens (\SavingsPlan' {paymentOption} -> paymentOption) (\s@SavingsPlan' {} a -> s {paymentOption = a} :: SavingsPlan)

-- | The Amazon Resource Name (ARN) of the Savings Plan.
savingsPlan_savingsPlanArn :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_savingsPlanArn = Lens.lens (\SavingsPlan' {savingsPlanArn} -> savingsPlanArn) (\s@SavingsPlan' {} a -> s {savingsPlanArn = a} :: SavingsPlan)

-- | One or more tags.
savingsPlan_tags :: Lens.Lens' SavingsPlan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
savingsPlan_tags = Lens.lens (\SavingsPlan' {tags} -> tags) (\s@SavingsPlan' {} a -> s {tags = a} :: SavingsPlan) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SavingsPlan where
  parseJSON =
    Core.withObject
      "SavingsPlan"
      ( \x ->
          SavingsPlan'
            Prelude.<$> (x Core..:? "termDurationInSeconds")
            Prelude.<*> (x Core..:? "savingsPlanType")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "productTypes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "start")
            Prelude.<*> (x Core..:? "ec2InstanceFamily")
            Prelude.<*> (x Core..:? "savingsPlanId")
            Prelude.<*> (x Core..:? "recurringPaymentAmount")
            Prelude.<*> (x Core..:? "currency")
            Prelude.<*> (x Core..:? "end")
            Prelude.<*> (x Core..:? "upfrontPaymentAmount")
            Prelude.<*> (x Core..:? "offeringId")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "commitment")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "paymentOption")
            Prelude.<*> (x Core..:? "savingsPlanArn")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SavingsPlan

instance Prelude.NFData SavingsPlan
