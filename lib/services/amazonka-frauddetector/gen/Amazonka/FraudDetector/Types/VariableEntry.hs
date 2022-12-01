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
-- Module      : Amazonka.FraudDetector.Types.VariableEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.VariableEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A variable in the list of variables for the batch create variable
-- request.
--
-- /See:/ 'newVariableEntry' smart constructor.
data VariableEntry = VariableEntry'
  { -- | The name of the variable.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the variable. For more information see
    -- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
    --
    -- Valid Values:
    -- @AUTH_CODE | AVS | BILLING_ADDRESS_L1 | BILLING_ADDRESS_L2 | BILLING_CITY | BILLING_COUNTRY | BILLING_NAME | BILLING_PHONE | BILLING_STATE | BILLING_ZIP | CARD_BIN | CATEGORICAL | CURRENCY_CODE | EMAIL_ADDRESS | FINGERPRINT | FRAUD_LABEL | FREE_FORM_TEXT | IP_ADDRESS | NUMERIC | ORDER_ID | PAYMENT_TYPE | PHONE_NUMBER | PRICE | PRODUCT_CATEGORY | SHIPPING_ADDRESS_L1 | SHIPPING_ADDRESS_L2 | SHIPPING_CITY | SHIPPING_COUNTRY | SHIPPING_NAME | SHIPPING_PHONE | SHIPPING_STATE | SHIPPING_ZIP | USERAGENT @
    variableType :: Prelude.Maybe Prelude.Text,
    -- | The default value of the variable.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The description of the variable.
    description :: Prelude.Maybe Prelude.Text,
    -- | The data source of the variable.
    dataSource :: Prelude.Maybe Prelude.Text,
    -- | The data type of the variable.
    dataType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariableEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'variableEntry_name' - The name of the variable.
--
-- 'variableType', 'variableEntry_variableType' - The type of the variable. For more information see
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
--
-- Valid Values:
-- @AUTH_CODE | AVS | BILLING_ADDRESS_L1 | BILLING_ADDRESS_L2 | BILLING_CITY | BILLING_COUNTRY | BILLING_NAME | BILLING_PHONE | BILLING_STATE | BILLING_ZIP | CARD_BIN | CATEGORICAL | CURRENCY_CODE | EMAIL_ADDRESS | FINGERPRINT | FRAUD_LABEL | FREE_FORM_TEXT | IP_ADDRESS | NUMERIC | ORDER_ID | PAYMENT_TYPE | PHONE_NUMBER | PRICE | PRODUCT_CATEGORY | SHIPPING_ADDRESS_L1 | SHIPPING_ADDRESS_L2 | SHIPPING_CITY | SHIPPING_COUNTRY | SHIPPING_NAME | SHIPPING_PHONE | SHIPPING_STATE | SHIPPING_ZIP | USERAGENT @
--
-- 'defaultValue', 'variableEntry_defaultValue' - The default value of the variable.
--
-- 'description', 'variableEntry_description' - The description of the variable.
--
-- 'dataSource', 'variableEntry_dataSource' - The data source of the variable.
--
-- 'dataType', 'variableEntry_dataType' - The data type of the variable.
newVariableEntry ::
  VariableEntry
newVariableEntry =
  VariableEntry'
    { name = Prelude.Nothing,
      variableType = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      description = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      dataType = Prelude.Nothing
    }

-- | The name of the variable.
variableEntry_name :: Lens.Lens' VariableEntry (Prelude.Maybe Prelude.Text)
variableEntry_name = Lens.lens (\VariableEntry' {name} -> name) (\s@VariableEntry' {} a -> s {name = a} :: VariableEntry)

-- | The type of the variable. For more information see
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
--
-- Valid Values:
-- @AUTH_CODE | AVS | BILLING_ADDRESS_L1 | BILLING_ADDRESS_L2 | BILLING_CITY | BILLING_COUNTRY | BILLING_NAME | BILLING_PHONE | BILLING_STATE | BILLING_ZIP | CARD_BIN | CATEGORICAL | CURRENCY_CODE | EMAIL_ADDRESS | FINGERPRINT | FRAUD_LABEL | FREE_FORM_TEXT | IP_ADDRESS | NUMERIC | ORDER_ID | PAYMENT_TYPE | PHONE_NUMBER | PRICE | PRODUCT_CATEGORY | SHIPPING_ADDRESS_L1 | SHIPPING_ADDRESS_L2 | SHIPPING_CITY | SHIPPING_COUNTRY | SHIPPING_NAME | SHIPPING_PHONE | SHIPPING_STATE | SHIPPING_ZIP | USERAGENT @
variableEntry_variableType :: Lens.Lens' VariableEntry (Prelude.Maybe Prelude.Text)
variableEntry_variableType = Lens.lens (\VariableEntry' {variableType} -> variableType) (\s@VariableEntry' {} a -> s {variableType = a} :: VariableEntry)

-- | The default value of the variable.
variableEntry_defaultValue :: Lens.Lens' VariableEntry (Prelude.Maybe Prelude.Text)
variableEntry_defaultValue = Lens.lens (\VariableEntry' {defaultValue} -> defaultValue) (\s@VariableEntry' {} a -> s {defaultValue = a} :: VariableEntry)

-- | The description of the variable.
variableEntry_description :: Lens.Lens' VariableEntry (Prelude.Maybe Prelude.Text)
variableEntry_description = Lens.lens (\VariableEntry' {description} -> description) (\s@VariableEntry' {} a -> s {description = a} :: VariableEntry)

-- | The data source of the variable.
variableEntry_dataSource :: Lens.Lens' VariableEntry (Prelude.Maybe Prelude.Text)
variableEntry_dataSource = Lens.lens (\VariableEntry' {dataSource} -> dataSource) (\s@VariableEntry' {} a -> s {dataSource = a} :: VariableEntry)

-- | The data type of the variable.
variableEntry_dataType :: Lens.Lens' VariableEntry (Prelude.Maybe Prelude.Text)
variableEntry_dataType = Lens.lens (\VariableEntry' {dataType} -> dataType) (\s@VariableEntry' {} a -> s {dataType = a} :: VariableEntry)

instance Prelude.Hashable VariableEntry where
  hashWithSalt _salt VariableEntry' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` variableType
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData VariableEntry where
  rnf VariableEntry' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf variableType
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf dataType

instance Core.ToJSON VariableEntry where
  toJSON VariableEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("variableType" Core..=) Prelude.<$> variableType,
            ("defaultValue" Core..=) Prelude.<$> defaultValue,
            ("description" Core..=) Prelude.<$> description,
            ("dataSource" Core..=) Prelude.<$> dataSource,
            ("dataType" Core..=) Prelude.<$> dataType
          ]
      )
