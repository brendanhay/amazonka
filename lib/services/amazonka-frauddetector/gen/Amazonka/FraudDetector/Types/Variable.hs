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
-- Module      : Amazonka.FraudDetector.Types.Variable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.Variable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.DataSource
import Amazonka.FraudDetector.Types.DataType
import qualified Amazonka.Prelude as Prelude

-- | The variable.
--
-- /See:/ 'newVariable' smart constructor.
data Variable = Variable'
  { -- | The ARN of the variable.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time when the variable was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The data source of the variable.
    dataSource :: Prelude.Maybe DataSource,
    -- | The data type of the variable. For more information see
    -- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
    dataType :: Prelude.Maybe DataType,
    -- | The default value of the variable.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The description of the variable.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time when variable was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the variable.
    name :: Prelude.Maybe Prelude.Text,
    -- | The variable type of the variable.
    --
    -- Valid Values:
    -- @AUTH_CODE | AVS | BILLING_ADDRESS_L1 | BILLING_ADDRESS_L2 | BILLING_CITY | BILLING_COUNTRY | BILLING_NAME | BILLING_PHONE | BILLING_STATE | BILLING_ZIP | CARD_BIN | CATEGORICAL | CURRENCY_CODE | EMAIL_ADDRESS | FINGERPRINT | FRAUD_LABEL | FREE_FORM_TEXT | IP_ADDRESS | NUMERIC | ORDER_ID | PAYMENT_TYPE | PHONE_NUMBER | PRICE | PRODUCT_CATEGORY | SHIPPING_ADDRESS_L1 | SHIPPING_ADDRESS_L2 | SHIPPING_CITY | SHIPPING_COUNTRY | SHIPPING_NAME | SHIPPING_PHONE | SHIPPING_STATE | SHIPPING_ZIP | USERAGENT @
    variableType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Variable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'variable_arn' - The ARN of the variable.
--
-- 'createdTime', 'variable_createdTime' - The time when the variable was created.
--
-- 'dataSource', 'variable_dataSource' - The data source of the variable.
--
-- 'dataType', 'variable_dataType' - The data type of the variable. For more information see
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
--
-- 'defaultValue', 'variable_defaultValue' - The default value of the variable.
--
-- 'description', 'variable_description' - The description of the variable.
--
-- 'lastUpdatedTime', 'variable_lastUpdatedTime' - The time when variable was last updated.
--
-- 'name', 'variable_name' - The name of the variable.
--
-- 'variableType', 'variable_variableType' - The variable type of the variable.
--
-- Valid Values:
-- @AUTH_CODE | AVS | BILLING_ADDRESS_L1 | BILLING_ADDRESS_L2 | BILLING_CITY | BILLING_COUNTRY | BILLING_NAME | BILLING_PHONE | BILLING_STATE | BILLING_ZIP | CARD_BIN | CATEGORICAL | CURRENCY_CODE | EMAIL_ADDRESS | FINGERPRINT | FRAUD_LABEL | FREE_FORM_TEXT | IP_ADDRESS | NUMERIC | ORDER_ID | PAYMENT_TYPE | PHONE_NUMBER | PRICE | PRODUCT_CATEGORY | SHIPPING_ADDRESS_L1 | SHIPPING_ADDRESS_L2 | SHIPPING_CITY | SHIPPING_COUNTRY | SHIPPING_NAME | SHIPPING_PHONE | SHIPPING_STATE | SHIPPING_ZIP | USERAGENT @
newVariable ::
  Variable
newVariable =
  Variable'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      dataType = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      variableType = Prelude.Nothing
    }

-- | The ARN of the variable.
variable_arn :: Lens.Lens' Variable (Prelude.Maybe Prelude.Text)
variable_arn = Lens.lens (\Variable' {arn} -> arn) (\s@Variable' {} a -> s {arn = a} :: Variable)

-- | The time when the variable was created.
variable_createdTime :: Lens.Lens' Variable (Prelude.Maybe Prelude.Text)
variable_createdTime = Lens.lens (\Variable' {createdTime} -> createdTime) (\s@Variable' {} a -> s {createdTime = a} :: Variable)

-- | The data source of the variable.
variable_dataSource :: Lens.Lens' Variable (Prelude.Maybe DataSource)
variable_dataSource = Lens.lens (\Variable' {dataSource} -> dataSource) (\s@Variable' {} a -> s {dataSource = a} :: Variable)

-- | The data type of the variable. For more information see
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
variable_dataType :: Lens.Lens' Variable (Prelude.Maybe DataType)
variable_dataType = Lens.lens (\Variable' {dataType} -> dataType) (\s@Variable' {} a -> s {dataType = a} :: Variable)

-- | The default value of the variable.
variable_defaultValue :: Lens.Lens' Variable (Prelude.Maybe Prelude.Text)
variable_defaultValue = Lens.lens (\Variable' {defaultValue} -> defaultValue) (\s@Variable' {} a -> s {defaultValue = a} :: Variable)

-- | The description of the variable.
variable_description :: Lens.Lens' Variable (Prelude.Maybe Prelude.Text)
variable_description = Lens.lens (\Variable' {description} -> description) (\s@Variable' {} a -> s {description = a} :: Variable)

-- | The time when variable was last updated.
variable_lastUpdatedTime :: Lens.Lens' Variable (Prelude.Maybe Prelude.Text)
variable_lastUpdatedTime = Lens.lens (\Variable' {lastUpdatedTime} -> lastUpdatedTime) (\s@Variable' {} a -> s {lastUpdatedTime = a} :: Variable)

-- | The name of the variable.
variable_name :: Lens.Lens' Variable (Prelude.Maybe Prelude.Text)
variable_name = Lens.lens (\Variable' {name} -> name) (\s@Variable' {} a -> s {name = a} :: Variable)

-- | The variable type of the variable.
--
-- Valid Values:
-- @AUTH_CODE | AVS | BILLING_ADDRESS_L1 | BILLING_ADDRESS_L2 | BILLING_CITY | BILLING_COUNTRY | BILLING_NAME | BILLING_PHONE | BILLING_STATE | BILLING_ZIP | CARD_BIN | CATEGORICAL | CURRENCY_CODE | EMAIL_ADDRESS | FINGERPRINT | FRAUD_LABEL | FREE_FORM_TEXT | IP_ADDRESS | NUMERIC | ORDER_ID | PAYMENT_TYPE | PHONE_NUMBER | PRICE | PRODUCT_CATEGORY | SHIPPING_ADDRESS_L1 | SHIPPING_ADDRESS_L2 | SHIPPING_CITY | SHIPPING_COUNTRY | SHIPPING_NAME | SHIPPING_PHONE | SHIPPING_STATE | SHIPPING_ZIP | USERAGENT @
variable_variableType :: Lens.Lens' Variable (Prelude.Maybe Prelude.Text)
variable_variableType = Lens.lens (\Variable' {variableType} -> variableType) (\s@Variable' {} a -> s {variableType = a} :: Variable)

instance Data.FromJSON Variable where
  parseJSON =
    Data.withObject
      "Variable"
      ( \x ->
          Variable'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdTime")
            Prelude.<*> (x Data..:? "dataSource")
            Prelude.<*> (x Data..:? "dataType")
            Prelude.<*> (x Data..:? "defaultValue")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "variableType")
      )

instance Prelude.Hashable Variable where
  hashWithSalt _salt Variable' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` variableType

instance Prelude.NFData Variable where
  rnf Variable' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdTime `Prelude.seq`
        Prelude.rnf dataSource `Prelude.seq`
          Prelude.rnf dataType `Prelude.seq`
            Prelude.rnf defaultValue `Prelude.seq`
              Prelude.rnf description `Prelude.seq`
                Prelude.rnf lastUpdatedTime `Prelude.seq`
                  Prelude.rnf name `Prelude.seq`
                    Prelude.rnf variableType
