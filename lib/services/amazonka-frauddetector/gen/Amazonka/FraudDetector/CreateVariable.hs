{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FraudDetector.CreateVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a variable.
module Amazonka.FraudDetector.CreateVariable
  ( -- * Creating a Request
    CreateVariable (..),
    newCreateVariable,

    -- * Request Lenses
    createVariable_description,
    createVariable_tags,
    createVariable_variableType,
    createVariable_name,
    createVariable_dataType,
    createVariable_dataSource,
    createVariable_defaultValue,

    -- * Destructuring the Response
    CreateVariableResponse (..),
    newCreateVariableResponse,

    -- * Response Lenses
    createVariableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVariable' smart constructor.
data CreateVariable = CreateVariable'
  { -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | A collection of key and value pairs.
    tags :: Prelude.Maybe [Tag],
    -- | The variable type. For more information see
    -- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
    --
    -- Valid Values:
    -- @AUTH_CODE | AVS | BILLING_ADDRESS_L1 | BILLING_ADDRESS_L2 | BILLING_CITY | BILLING_COUNTRY | BILLING_NAME | BILLING_PHONE | BILLING_STATE | BILLING_ZIP | CARD_BIN | CATEGORICAL | CURRENCY_CODE | EMAIL_ADDRESS | FINGERPRINT | FRAUD_LABEL | FREE_FORM_TEXT | IP_ADDRESS | NUMERIC | ORDER_ID | PAYMENT_TYPE | PHONE_NUMBER | PRICE | PRODUCT_CATEGORY | SHIPPING_ADDRESS_L1 | SHIPPING_ADDRESS_L2 | SHIPPING_CITY | SHIPPING_COUNTRY | SHIPPING_NAME | SHIPPING_PHONE | SHIPPING_STATE | SHIPPING_ZIP | USERAGENT@
    variableType :: Prelude.Maybe Prelude.Text,
    -- | The name of the variable.
    name :: Prelude.Text,
    -- | The data type.
    dataType :: DataType,
    -- | The source of the data.
    dataSource :: DataSource,
    -- | The default value for the variable when no value is received.
    defaultValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVariable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createVariable_description' - The description.
--
-- 'tags', 'createVariable_tags' - A collection of key and value pairs.
--
-- 'variableType', 'createVariable_variableType' - The variable type. For more information see
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
--
-- Valid Values:
-- @AUTH_CODE | AVS | BILLING_ADDRESS_L1 | BILLING_ADDRESS_L2 | BILLING_CITY | BILLING_COUNTRY | BILLING_NAME | BILLING_PHONE | BILLING_STATE | BILLING_ZIP | CARD_BIN | CATEGORICAL | CURRENCY_CODE | EMAIL_ADDRESS | FINGERPRINT | FRAUD_LABEL | FREE_FORM_TEXT | IP_ADDRESS | NUMERIC | ORDER_ID | PAYMENT_TYPE | PHONE_NUMBER | PRICE | PRODUCT_CATEGORY | SHIPPING_ADDRESS_L1 | SHIPPING_ADDRESS_L2 | SHIPPING_CITY | SHIPPING_COUNTRY | SHIPPING_NAME | SHIPPING_PHONE | SHIPPING_STATE | SHIPPING_ZIP | USERAGENT@
--
-- 'name', 'createVariable_name' - The name of the variable.
--
-- 'dataType', 'createVariable_dataType' - The data type.
--
-- 'dataSource', 'createVariable_dataSource' - The source of the data.
--
-- 'defaultValue', 'createVariable_defaultValue' - The default value for the variable when no value is received.
newCreateVariable ::
  -- | 'name'
  Prelude.Text ->
  -- | 'dataType'
  DataType ->
  -- | 'dataSource'
  DataSource ->
  -- | 'defaultValue'
  Prelude.Text ->
  CreateVariable
newCreateVariable
  pName_
  pDataType_
  pDataSource_
  pDefaultValue_ =
    CreateVariable'
      { description = Prelude.Nothing,
        tags = Prelude.Nothing,
        variableType = Prelude.Nothing,
        name = pName_,
        dataType = pDataType_,
        dataSource = pDataSource_,
        defaultValue = pDefaultValue_
      }

-- | The description.
createVariable_description :: Lens.Lens' CreateVariable (Prelude.Maybe Prelude.Text)
createVariable_description = Lens.lens (\CreateVariable' {description} -> description) (\s@CreateVariable' {} a -> s {description = a} :: CreateVariable)

-- | A collection of key and value pairs.
createVariable_tags :: Lens.Lens' CreateVariable (Prelude.Maybe [Tag])
createVariable_tags = Lens.lens (\CreateVariable' {tags} -> tags) (\s@CreateVariable' {} a -> s {tags = a} :: CreateVariable) Prelude.. Lens.mapping Lens.coerced

-- | The variable type. For more information see
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
--
-- Valid Values:
-- @AUTH_CODE | AVS | BILLING_ADDRESS_L1 | BILLING_ADDRESS_L2 | BILLING_CITY | BILLING_COUNTRY | BILLING_NAME | BILLING_PHONE | BILLING_STATE | BILLING_ZIP | CARD_BIN | CATEGORICAL | CURRENCY_CODE | EMAIL_ADDRESS | FINGERPRINT | FRAUD_LABEL | FREE_FORM_TEXT | IP_ADDRESS | NUMERIC | ORDER_ID | PAYMENT_TYPE | PHONE_NUMBER | PRICE | PRODUCT_CATEGORY | SHIPPING_ADDRESS_L1 | SHIPPING_ADDRESS_L2 | SHIPPING_CITY | SHIPPING_COUNTRY | SHIPPING_NAME | SHIPPING_PHONE | SHIPPING_STATE | SHIPPING_ZIP | USERAGENT@
createVariable_variableType :: Lens.Lens' CreateVariable (Prelude.Maybe Prelude.Text)
createVariable_variableType = Lens.lens (\CreateVariable' {variableType} -> variableType) (\s@CreateVariable' {} a -> s {variableType = a} :: CreateVariable)

-- | The name of the variable.
createVariable_name :: Lens.Lens' CreateVariable Prelude.Text
createVariable_name = Lens.lens (\CreateVariable' {name} -> name) (\s@CreateVariable' {} a -> s {name = a} :: CreateVariable)

-- | The data type.
createVariable_dataType :: Lens.Lens' CreateVariable DataType
createVariable_dataType = Lens.lens (\CreateVariable' {dataType} -> dataType) (\s@CreateVariable' {} a -> s {dataType = a} :: CreateVariable)

-- | The source of the data.
createVariable_dataSource :: Lens.Lens' CreateVariable DataSource
createVariable_dataSource = Lens.lens (\CreateVariable' {dataSource} -> dataSource) (\s@CreateVariable' {} a -> s {dataSource = a} :: CreateVariable)

-- | The default value for the variable when no value is received.
createVariable_defaultValue :: Lens.Lens' CreateVariable Prelude.Text
createVariable_defaultValue = Lens.lens (\CreateVariable' {defaultValue} -> defaultValue) (\s@CreateVariable' {} a -> s {defaultValue = a} :: CreateVariable)

instance Core.AWSRequest CreateVariable where
  type
    AWSResponse CreateVariable =
      CreateVariableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateVariableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVariable where
  hashWithSalt _salt CreateVariable' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` variableType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` defaultValue

instance Prelude.NFData CreateVariable where
  rnf CreateVariable' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf variableType `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf dataType `Prelude.seq`
              Prelude.rnf dataSource `Prelude.seq`
                Prelude.rnf defaultValue

instance Data.ToHeaders CreateVariable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.CreateVariable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVariable where
  toJSON CreateVariable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            ("variableType" Data..=) Prelude.<$> variableType,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("dataType" Data..= dataType),
            Prelude.Just ("dataSource" Data..= dataSource),
            Prelude.Just ("defaultValue" Data..= defaultValue)
          ]
      )

instance Data.ToPath CreateVariable where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVariable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVariableResponse' smart constructor.
data CreateVariableResponse = CreateVariableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVariableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVariableResponse_httpStatus' - The response's http status code.
newCreateVariableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVariableResponse
newCreateVariableResponse pHttpStatus_ =
  CreateVariableResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createVariableResponse_httpStatus :: Lens.Lens' CreateVariableResponse Prelude.Int
createVariableResponse_httpStatus = Lens.lens (\CreateVariableResponse' {httpStatus} -> httpStatus) (\s@CreateVariableResponse' {} a -> s {httpStatus = a} :: CreateVariableResponse)

instance Prelude.NFData CreateVariableResponse where
  rnf CreateVariableResponse' {..} =
    Prelude.rnf httpStatus
