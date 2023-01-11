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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlan where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The hourly commitment, in USD.
    commitment :: Prelude.Maybe Prelude.Text,
    -- | The currency.
    currency :: Prelude.Maybe CurrencyCode,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The EC2 instance family.
    ec2InstanceFamily :: Prelude.Maybe Prelude.Text,
    -- | The end time.
    end :: Prelude.Maybe Prelude.Text,
    -- | The ID of the offering.
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | The payment option.
    paymentOption :: Prelude.Maybe SavingsPlanPaymentOption,
    -- | The product types.
    productTypes :: Prelude.Maybe [SavingsPlanProductType],
    -- | The recurring payment amount.
    recurringPaymentAmount :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region.
    region :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Savings Plan.
    savingsPlanArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Savings Plan.
    savingsPlanId :: Prelude.Maybe Prelude.Text,
    -- | The plan type.
    savingsPlanType :: Prelude.Maybe SavingsPlanType,
    -- | The start time.
    start :: Prelude.Maybe Prelude.Text,
    -- | The state.
    state :: Prelude.Maybe SavingsPlanState,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The duration of the term, in seconds.
    termDurationInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The up-front payment amount.
    upfrontPaymentAmount :: Prelude.Maybe Prelude.Text
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
-- 'commitment', 'savingsPlan_commitment' - The hourly commitment, in USD.
--
-- 'currency', 'savingsPlan_currency' - The currency.
--
-- 'description', 'savingsPlan_description' - The description.
--
-- 'ec2InstanceFamily', 'savingsPlan_ec2InstanceFamily' - The EC2 instance family.
--
-- 'end', 'savingsPlan_end' - The end time.
--
-- 'offeringId', 'savingsPlan_offeringId' - The ID of the offering.
--
-- 'paymentOption', 'savingsPlan_paymentOption' - The payment option.
--
-- 'productTypes', 'savingsPlan_productTypes' - The product types.
--
-- 'recurringPaymentAmount', 'savingsPlan_recurringPaymentAmount' - The recurring payment amount.
--
-- 'region', 'savingsPlan_region' - The AWS Region.
--
-- 'savingsPlanArn', 'savingsPlan_savingsPlanArn' - The Amazon Resource Name (ARN) of the Savings Plan.
--
-- 'savingsPlanId', 'savingsPlan_savingsPlanId' - The ID of the Savings Plan.
--
-- 'savingsPlanType', 'savingsPlan_savingsPlanType' - The plan type.
--
-- 'start', 'savingsPlan_start' - The start time.
--
-- 'state', 'savingsPlan_state' - The state.
--
-- 'tags', 'savingsPlan_tags' - One or more tags.
--
-- 'termDurationInSeconds', 'savingsPlan_termDurationInSeconds' - The duration of the term, in seconds.
--
-- 'upfrontPaymentAmount', 'savingsPlan_upfrontPaymentAmount' - The up-front payment amount.
newSavingsPlan ::
  SavingsPlan
newSavingsPlan =
  SavingsPlan'
    { commitment = Prelude.Nothing,
      currency = Prelude.Nothing,
      description = Prelude.Nothing,
      ec2InstanceFamily = Prelude.Nothing,
      end = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      productTypes = Prelude.Nothing,
      recurringPaymentAmount = Prelude.Nothing,
      region = Prelude.Nothing,
      savingsPlanArn = Prelude.Nothing,
      savingsPlanId = Prelude.Nothing,
      savingsPlanType = Prelude.Nothing,
      start = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      termDurationInSeconds = Prelude.Nothing,
      upfrontPaymentAmount = Prelude.Nothing
    }

-- | The hourly commitment, in USD.
savingsPlan_commitment :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_commitment = Lens.lens (\SavingsPlan' {commitment} -> commitment) (\s@SavingsPlan' {} a -> s {commitment = a} :: SavingsPlan)

-- | The currency.
savingsPlan_currency :: Lens.Lens' SavingsPlan (Prelude.Maybe CurrencyCode)
savingsPlan_currency = Lens.lens (\SavingsPlan' {currency} -> currency) (\s@SavingsPlan' {} a -> s {currency = a} :: SavingsPlan)

-- | The description.
savingsPlan_description :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_description = Lens.lens (\SavingsPlan' {description} -> description) (\s@SavingsPlan' {} a -> s {description = a} :: SavingsPlan)

-- | The EC2 instance family.
savingsPlan_ec2InstanceFamily :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_ec2InstanceFamily = Lens.lens (\SavingsPlan' {ec2InstanceFamily} -> ec2InstanceFamily) (\s@SavingsPlan' {} a -> s {ec2InstanceFamily = a} :: SavingsPlan)

-- | The end time.
savingsPlan_end :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_end = Lens.lens (\SavingsPlan' {end} -> end) (\s@SavingsPlan' {} a -> s {end = a} :: SavingsPlan)

-- | The ID of the offering.
savingsPlan_offeringId :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_offeringId = Lens.lens (\SavingsPlan' {offeringId} -> offeringId) (\s@SavingsPlan' {} a -> s {offeringId = a} :: SavingsPlan)

-- | The payment option.
savingsPlan_paymentOption :: Lens.Lens' SavingsPlan (Prelude.Maybe SavingsPlanPaymentOption)
savingsPlan_paymentOption = Lens.lens (\SavingsPlan' {paymentOption} -> paymentOption) (\s@SavingsPlan' {} a -> s {paymentOption = a} :: SavingsPlan)

-- | The product types.
savingsPlan_productTypes :: Lens.Lens' SavingsPlan (Prelude.Maybe [SavingsPlanProductType])
savingsPlan_productTypes = Lens.lens (\SavingsPlan' {productTypes} -> productTypes) (\s@SavingsPlan' {} a -> s {productTypes = a} :: SavingsPlan) Prelude.. Lens.mapping Lens.coerced

-- | The recurring payment amount.
savingsPlan_recurringPaymentAmount :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_recurringPaymentAmount = Lens.lens (\SavingsPlan' {recurringPaymentAmount} -> recurringPaymentAmount) (\s@SavingsPlan' {} a -> s {recurringPaymentAmount = a} :: SavingsPlan)

-- | The AWS Region.
savingsPlan_region :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_region = Lens.lens (\SavingsPlan' {region} -> region) (\s@SavingsPlan' {} a -> s {region = a} :: SavingsPlan)

-- | The Amazon Resource Name (ARN) of the Savings Plan.
savingsPlan_savingsPlanArn :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_savingsPlanArn = Lens.lens (\SavingsPlan' {savingsPlanArn} -> savingsPlanArn) (\s@SavingsPlan' {} a -> s {savingsPlanArn = a} :: SavingsPlan)

-- | The ID of the Savings Plan.
savingsPlan_savingsPlanId :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_savingsPlanId = Lens.lens (\SavingsPlan' {savingsPlanId} -> savingsPlanId) (\s@SavingsPlan' {} a -> s {savingsPlanId = a} :: SavingsPlan)

-- | The plan type.
savingsPlan_savingsPlanType :: Lens.Lens' SavingsPlan (Prelude.Maybe SavingsPlanType)
savingsPlan_savingsPlanType = Lens.lens (\SavingsPlan' {savingsPlanType} -> savingsPlanType) (\s@SavingsPlan' {} a -> s {savingsPlanType = a} :: SavingsPlan)

-- | The start time.
savingsPlan_start :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_start = Lens.lens (\SavingsPlan' {start} -> start) (\s@SavingsPlan' {} a -> s {start = a} :: SavingsPlan)

-- | The state.
savingsPlan_state :: Lens.Lens' SavingsPlan (Prelude.Maybe SavingsPlanState)
savingsPlan_state = Lens.lens (\SavingsPlan' {state} -> state) (\s@SavingsPlan' {} a -> s {state = a} :: SavingsPlan)

-- | One or more tags.
savingsPlan_tags :: Lens.Lens' SavingsPlan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
savingsPlan_tags = Lens.lens (\SavingsPlan' {tags} -> tags) (\s@SavingsPlan' {} a -> s {tags = a} :: SavingsPlan) Prelude.. Lens.mapping Lens.coerced

-- | The duration of the term, in seconds.
savingsPlan_termDurationInSeconds :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Integer)
savingsPlan_termDurationInSeconds = Lens.lens (\SavingsPlan' {termDurationInSeconds} -> termDurationInSeconds) (\s@SavingsPlan' {} a -> s {termDurationInSeconds = a} :: SavingsPlan)

-- | The up-front payment amount.
savingsPlan_upfrontPaymentAmount :: Lens.Lens' SavingsPlan (Prelude.Maybe Prelude.Text)
savingsPlan_upfrontPaymentAmount = Lens.lens (\SavingsPlan' {upfrontPaymentAmount} -> upfrontPaymentAmount) (\s@SavingsPlan' {} a -> s {upfrontPaymentAmount = a} :: SavingsPlan)

instance Data.FromJSON SavingsPlan where
  parseJSON =
    Data.withObject
      "SavingsPlan"
      ( \x ->
          SavingsPlan'
            Prelude.<$> (x Data..:? "commitment")
            Prelude.<*> (x Data..:? "currency")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "ec2InstanceFamily")
            Prelude.<*> (x Data..:? "end")
            Prelude.<*> (x Data..:? "offeringId")
            Prelude.<*> (x Data..:? "paymentOption")
            Prelude.<*> (x Data..:? "productTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "recurringPaymentAmount")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "savingsPlanArn")
            Prelude.<*> (x Data..:? "savingsPlanId")
            Prelude.<*> (x Data..:? "savingsPlanType")
            Prelude.<*> (x Data..:? "start")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "termDurationInSeconds")
            Prelude.<*> (x Data..:? "upfrontPaymentAmount")
      )

instance Prelude.Hashable SavingsPlan where
  hashWithSalt _salt SavingsPlan' {..} =
    _salt `Prelude.hashWithSalt` commitment
      `Prelude.hashWithSalt` currency
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ec2InstanceFamily
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` offeringId
      `Prelude.hashWithSalt` paymentOption
      `Prelude.hashWithSalt` productTypes
      `Prelude.hashWithSalt` recurringPaymentAmount
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` savingsPlanArn
      `Prelude.hashWithSalt` savingsPlanId
      `Prelude.hashWithSalt` savingsPlanType
      `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` termDurationInSeconds
      `Prelude.hashWithSalt` upfrontPaymentAmount

instance Prelude.NFData SavingsPlan where
  rnf SavingsPlan' {..} =
    Prelude.rnf commitment
      `Prelude.seq` Prelude.rnf currency
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ec2InstanceFamily
      `Prelude.seq` Prelude.rnf end
      `Prelude.seq` Prelude.rnf offeringId
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf productTypes
      `Prelude.seq` Prelude.rnf recurringPaymentAmount
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf savingsPlanArn
      `Prelude.seq` Prelude.rnf savingsPlanId
      `Prelude.seq` Prelude.rnf savingsPlanType
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf termDurationInSeconds
      `Prelude.seq` Prelude.rnf upfrontPaymentAmount
