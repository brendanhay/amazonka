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
-- Module      : Amazonka.BillingConductor.CreateCustomLineItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom line item that can be used to create a one-time fixed
-- charge that can be applied to a single billing group for the current or
-- previous billing period. The one-time fixed charge is either a fee or
-- discount.
module Amazonka.BillingConductor.CreateCustomLineItem
  ( -- * Creating a Request
    CreateCustomLineItem (..),
    newCreateCustomLineItem,

    -- * Request Lenses
    createCustomLineItem_billingPeriodRange,
    createCustomLineItem_clientToken,
    createCustomLineItem_tags,
    createCustomLineItem_name,
    createCustomLineItem_description,
    createCustomLineItem_billingGroupArn,
    createCustomLineItem_chargeDetails,

    -- * Destructuring the Response
    CreateCustomLineItemResponse (..),
    newCreateCustomLineItemResponse,

    -- * Response Lenses
    createCustomLineItemResponse_arn,
    createCustomLineItemResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomLineItem' smart constructor.
data CreateCustomLineItem = CreateCustomLineItem'
  { -- | A time range for which the custom line item is effective.
    billingPeriodRange :: Prelude.Maybe CustomLineItemBillingPeriodRange,
    -- | The token that is needed to support idempotency. Idempotency isn\'t
    -- currently supported, but will be implemented in a future update.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A map that contains tag keys and tag values that are attached to a
    -- custom line item.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the custom line item.
    name :: Data.Sensitive Prelude.Text,
    -- | The description of the custom line item. This is shown on the Bills page
    -- in association with the charge value.
    description :: Data.Sensitive Prelude.Text,
    -- | The Amazon Resource Name (ARN) that references the billing group where
    -- the custom line item applies to.
    billingGroupArn :: Prelude.Text,
    -- | A @CustomLineItemChargeDetails@ that describes the charge details for a
    -- custom line item.
    chargeDetails :: CustomLineItemChargeDetails
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomLineItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriodRange', 'createCustomLineItem_billingPeriodRange' - A time range for which the custom line item is effective.
--
-- 'clientToken', 'createCustomLineItem_clientToken' - The token that is needed to support idempotency. Idempotency isn\'t
-- currently supported, but will be implemented in a future update.
--
-- 'tags', 'createCustomLineItem_tags' - A map that contains tag keys and tag values that are attached to a
-- custom line item.
--
-- 'name', 'createCustomLineItem_name' - The name of the custom line item.
--
-- 'description', 'createCustomLineItem_description' - The description of the custom line item. This is shown on the Bills page
-- in association with the charge value.
--
-- 'billingGroupArn', 'createCustomLineItem_billingGroupArn' - The Amazon Resource Name (ARN) that references the billing group where
-- the custom line item applies to.
--
-- 'chargeDetails', 'createCustomLineItem_chargeDetails' - A @CustomLineItemChargeDetails@ that describes the charge details for a
-- custom line item.
newCreateCustomLineItem ::
  -- | 'name'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'billingGroupArn'
  Prelude.Text ->
  -- | 'chargeDetails'
  CustomLineItemChargeDetails ->
  CreateCustomLineItem
newCreateCustomLineItem
  pName_
  pDescription_
  pBillingGroupArn_
  pChargeDetails_ =
    CreateCustomLineItem'
      { billingPeriodRange =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = Data._Sensitive Lens.# pName_,
        description = Data._Sensitive Lens.# pDescription_,
        billingGroupArn = pBillingGroupArn_,
        chargeDetails = pChargeDetails_
      }

-- | A time range for which the custom line item is effective.
createCustomLineItem_billingPeriodRange :: Lens.Lens' CreateCustomLineItem (Prelude.Maybe CustomLineItemBillingPeriodRange)
createCustomLineItem_billingPeriodRange = Lens.lens (\CreateCustomLineItem' {billingPeriodRange} -> billingPeriodRange) (\s@CreateCustomLineItem' {} a -> s {billingPeriodRange = a} :: CreateCustomLineItem)

-- | The token that is needed to support idempotency. Idempotency isn\'t
-- currently supported, but will be implemented in a future update.
createCustomLineItem_clientToken :: Lens.Lens' CreateCustomLineItem (Prelude.Maybe Prelude.Text)
createCustomLineItem_clientToken = Lens.lens (\CreateCustomLineItem' {clientToken} -> clientToken) (\s@CreateCustomLineItem' {} a -> s {clientToken = a} :: CreateCustomLineItem)

-- | A map that contains tag keys and tag values that are attached to a
-- custom line item.
createCustomLineItem_tags :: Lens.Lens' CreateCustomLineItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCustomLineItem_tags = Lens.lens (\CreateCustomLineItem' {tags} -> tags) (\s@CreateCustomLineItem' {} a -> s {tags = a} :: CreateCustomLineItem) Prelude.. Lens.mapping Lens.coerced

-- | The name of the custom line item.
createCustomLineItem_name :: Lens.Lens' CreateCustomLineItem Prelude.Text
createCustomLineItem_name = Lens.lens (\CreateCustomLineItem' {name} -> name) (\s@CreateCustomLineItem' {} a -> s {name = a} :: CreateCustomLineItem) Prelude.. Data._Sensitive

-- | The description of the custom line item. This is shown on the Bills page
-- in association with the charge value.
createCustomLineItem_description :: Lens.Lens' CreateCustomLineItem Prelude.Text
createCustomLineItem_description = Lens.lens (\CreateCustomLineItem' {description} -> description) (\s@CreateCustomLineItem' {} a -> s {description = a} :: CreateCustomLineItem) Prelude.. Data._Sensitive

-- | The Amazon Resource Name (ARN) that references the billing group where
-- the custom line item applies to.
createCustomLineItem_billingGroupArn :: Lens.Lens' CreateCustomLineItem Prelude.Text
createCustomLineItem_billingGroupArn = Lens.lens (\CreateCustomLineItem' {billingGroupArn} -> billingGroupArn) (\s@CreateCustomLineItem' {} a -> s {billingGroupArn = a} :: CreateCustomLineItem)

-- | A @CustomLineItemChargeDetails@ that describes the charge details for a
-- custom line item.
createCustomLineItem_chargeDetails :: Lens.Lens' CreateCustomLineItem CustomLineItemChargeDetails
createCustomLineItem_chargeDetails = Lens.lens (\CreateCustomLineItem' {chargeDetails} -> chargeDetails) (\s@CreateCustomLineItem' {} a -> s {chargeDetails = a} :: CreateCustomLineItem)

instance Core.AWSRequest CreateCustomLineItem where
  type
    AWSResponse CreateCustomLineItem =
      CreateCustomLineItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomLineItemResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomLineItem where
  hashWithSalt _salt CreateCustomLineItem' {..} =
    _salt
      `Prelude.hashWithSalt` billingPeriodRange
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` billingGroupArn
      `Prelude.hashWithSalt` chargeDetails

instance Prelude.NFData CreateCustomLineItem where
  rnf CreateCustomLineItem' {..} =
    Prelude.rnf billingPeriodRange `Prelude.seq`
      Prelude.rnf clientToken `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf billingGroupArn `Prelude.seq`
                Prelude.rnf chargeDetails

instance Data.ToHeaders CreateCustomLineItem where
  toHeaders CreateCustomLineItem' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateCustomLineItem where
  toJSON CreateCustomLineItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingPeriodRange" Data..=)
              Prelude.<$> billingPeriodRange,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Description" Data..= description),
            Prelude.Just
              ("BillingGroupArn" Data..= billingGroupArn),
            Prelude.Just
              ("ChargeDetails" Data..= chargeDetails)
          ]
      )

instance Data.ToPath CreateCustomLineItem where
  toPath = Prelude.const "/create-custom-line-item"

instance Data.ToQuery CreateCustomLineItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomLineItemResponse' smart constructor.
data CreateCustomLineItemResponse = CreateCustomLineItemResponse'
  { -- | The Amazon Resource Name (ARN) of the created custom line item.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomLineItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createCustomLineItemResponse_arn' - The Amazon Resource Name (ARN) of the created custom line item.
--
-- 'httpStatus', 'createCustomLineItemResponse_httpStatus' - The response's http status code.
newCreateCustomLineItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomLineItemResponse
newCreateCustomLineItemResponse pHttpStatus_ =
  CreateCustomLineItemResponse'
    { arn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the created custom line item.
createCustomLineItemResponse_arn :: Lens.Lens' CreateCustomLineItemResponse (Prelude.Maybe Prelude.Text)
createCustomLineItemResponse_arn = Lens.lens (\CreateCustomLineItemResponse' {arn} -> arn) (\s@CreateCustomLineItemResponse' {} a -> s {arn = a} :: CreateCustomLineItemResponse)

-- | The response's http status code.
createCustomLineItemResponse_httpStatus :: Lens.Lens' CreateCustomLineItemResponse Prelude.Int
createCustomLineItemResponse_httpStatus = Lens.lens (\CreateCustomLineItemResponse' {httpStatus} -> httpStatus) (\s@CreateCustomLineItemResponse' {} a -> s {httpStatus = a} :: CreateCustomLineItemResponse)

instance Prelude.NFData CreateCustomLineItemResponse where
  rnf CreateCustomLineItemResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf httpStatus
