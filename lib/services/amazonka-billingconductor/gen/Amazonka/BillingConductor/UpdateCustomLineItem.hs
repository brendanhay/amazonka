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
-- Module      : Amazonka.BillingConductor.UpdateCustomLineItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an existing custom line item in the current or previous billing
-- period.
module Amazonka.BillingConductor.UpdateCustomLineItem
  ( -- * Creating a Request
    UpdateCustomLineItem (..),
    newUpdateCustomLineItem,

    -- * Request Lenses
    updateCustomLineItem_name,
    updateCustomLineItem_chargeDetails,
    updateCustomLineItem_billingPeriodRange,
    updateCustomLineItem_description,
    updateCustomLineItem_arn,

    -- * Destructuring the Response
    UpdateCustomLineItemResponse (..),
    newUpdateCustomLineItemResponse,

    -- * Response Lenses
    updateCustomLineItemResponse_name,
    updateCustomLineItemResponse_chargeDetails,
    updateCustomLineItemResponse_billingGroupArn,
    updateCustomLineItemResponse_arn,
    updateCustomLineItemResponse_associationSize,
    updateCustomLineItemResponse_description,
    updateCustomLineItemResponse_lastModifiedTime,
    updateCustomLineItemResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCustomLineItem' smart constructor.
data UpdateCustomLineItem = UpdateCustomLineItem'
  { -- | The new name for the custom line item.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A @ListCustomLineItemChargeDetails@ containing the new charge details
    -- for the custom line item.
    chargeDetails :: Prelude.Maybe UpdateCustomLineItemChargeDetails,
    billingPeriodRange :: Prelude.Maybe CustomLineItemBillingPeriodRange,
    -- | The new line item description of the custom line item.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the custom line item to be updated.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomLineItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateCustomLineItem_name' - The new name for the custom line item.
--
-- 'chargeDetails', 'updateCustomLineItem_chargeDetails' - A @ListCustomLineItemChargeDetails@ containing the new charge details
-- for the custom line item.
--
-- 'billingPeriodRange', 'updateCustomLineItem_billingPeriodRange' - Undocumented member.
--
-- 'description', 'updateCustomLineItem_description' - The new line item description of the custom line item.
--
-- 'arn', 'updateCustomLineItem_arn' - The ARN of the custom line item to be updated.
newUpdateCustomLineItem ::
  -- | 'arn'
  Prelude.Text ->
  UpdateCustomLineItem
newUpdateCustomLineItem pArn_ =
  UpdateCustomLineItem'
    { name = Prelude.Nothing,
      chargeDetails = Prelude.Nothing,
      billingPeriodRange = Prelude.Nothing,
      description = Prelude.Nothing,
      arn = pArn_
    }

-- | The new name for the custom line item.
updateCustomLineItem_name :: Lens.Lens' UpdateCustomLineItem (Prelude.Maybe Prelude.Text)
updateCustomLineItem_name = Lens.lens (\UpdateCustomLineItem' {name} -> name) (\s@UpdateCustomLineItem' {} a -> s {name = a} :: UpdateCustomLineItem) Prelude.. Lens.mapping Data._Sensitive

-- | A @ListCustomLineItemChargeDetails@ containing the new charge details
-- for the custom line item.
updateCustomLineItem_chargeDetails :: Lens.Lens' UpdateCustomLineItem (Prelude.Maybe UpdateCustomLineItemChargeDetails)
updateCustomLineItem_chargeDetails = Lens.lens (\UpdateCustomLineItem' {chargeDetails} -> chargeDetails) (\s@UpdateCustomLineItem' {} a -> s {chargeDetails = a} :: UpdateCustomLineItem)

-- | Undocumented member.
updateCustomLineItem_billingPeriodRange :: Lens.Lens' UpdateCustomLineItem (Prelude.Maybe CustomLineItemBillingPeriodRange)
updateCustomLineItem_billingPeriodRange = Lens.lens (\UpdateCustomLineItem' {billingPeriodRange} -> billingPeriodRange) (\s@UpdateCustomLineItem' {} a -> s {billingPeriodRange = a} :: UpdateCustomLineItem)

-- | The new line item description of the custom line item.
updateCustomLineItem_description :: Lens.Lens' UpdateCustomLineItem (Prelude.Maybe Prelude.Text)
updateCustomLineItem_description = Lens.lens (\UpdateCustomLineItem' {description} -> description) (\s@UpdateCustomLineItem' {} a -> s {description = a} :: UpdateCustomLineItem) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the custom line item to be updated.
updateCustomLineItem_arn :: Lens.Lens' UpdateCustomLineItem Prelude.Text
updateCustomLineItem_arn = Lens.lens (\UpdateCustomLineItem' {arn} -> arn) (\s@UpdateCustomLineItem' {} a -> s {arn = a} :: UpdateCustomLineItem)

instance Core.AWSRequest UpdateCustomLineItem where
  type
    AWSResponse UpdateCustomLineItem =
      UpdateCustomLineItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCustomLineItemResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "ChargeDetails")
            Prelude.<*> (x Data..?> "BillingGroupArn")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AssociationSize")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCustomLineItem where
  hashWithSalt _salt UpdateCustomLineItem' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` chargeDetails
      `Prelude.hashWithSalt` billingPeriodRange
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateCustomLineItem where
  rnf UpdateCustomLineItem' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf chargeDetails
      `Prelude.seq` Prelude.rnf billingPeriodRange
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders UpdateCustomLineItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCustomLineItem where
  toJSON UpdateCustomLineItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("ChargeDetails" Data..=) Prelude.<$> chargeDetails,
            ("BillingPeriodRange" Data..=)
              Prelude.<$> billingPeriodRange,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdateCustomLineItem where
  toPath = Prelude.const "/update-custom-line-item"

instance Data.ToQuery UpdateCustomLineItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCustomLineItemResponse' smart constructor.
data UpdateCustomLineItemResponse = UpdateCustomLineItemResponse'
  { -- | The name of the successfully updated custom line item.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A @ListCustomLineItemChargeDetails@ containing the charge details of the
    -- successfully updated custom line item.
    chargeDetails :: Prelude.Maybe ListCustomLineItemChargeDetails,
    -- | The ARN of the billing group that the custom line item is applied to.
    billingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the successfully updated custom line item.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The number of resources that are associated to the custom line item.
    associationSize :: Prelude.Maybe Prelude.Natural,
    -- | The description of the successfully updated custom line item.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The most recent time when the custom line item was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomLineItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateCustomLineItemResponse_name' - The name of the successfully updated custom line item.
--
-- 'chargeDetails', 'updateCustomLineItemResponse_chargeDetails' - A @ListCustomLineItemChargeDetails@ containing the charge details of the
-- successfully updated custom line item.
--
-- 'billingGroupArn', 'updateCustomLineItemResponse_billingGroupArn' - The ARN of the billing group that the custom line item is applied to.
--
-- 'arn', 'updateCustomLineItemResponse_arn' - The ARN of the successfully updated custom line item.
--
-- 'associationSize', 'updateCustomLineItemResponse_associationSize' - The number of resources that are associated to the custom line item.
--
-- 'description', 'updateCustomLineItemResponse_description' - The description of the successfully updated custom line item.
--
-- 'lastModifiedTime', 'updateCustomLineItemResponse_lastModifiedTime' - The most recent time when the custom line item was modified.
--
-- 'httpStatus', 'updateCustomLineItemResponse_httpStatus' - The response's http status code.
newUpdateCustomLineItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCustomLineItemResponse
newUpdateCustomLineItemResponse pHttpStatus_ =
  UpdateCustomLineItemResponse'
    { name =
        Prelude.Nothing,
      chargeDetails = Prelude.Nothing,
      billingGroupArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      associationSize = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the successfully updated custom line item.
updateCustomLineItemResponse_name :: Lens.Lens' UpdateCustomLineItemResponse (Prelude.Maybe Prelude.Text)
updateCustomLineItemResponse_name = Lens.lens (\UpdateCustomLineItemResponse' {name} -> name) (\s@UpdateCustomLineItemResponse' {} a -> s {name = a} :: UpdateCustomLineItemResponse) Prelude.. Lens.mapping Data._Sensitive

-- | A @ListCustomLineItemChargeDetails@ containing the charge details of the
-- successfully updated custom line item.
updateCustomLineItemResponse_chargeDetails :: Lens.Lens' UpdateCustomLineItemResponse (Prelude.Maybe ListCustomLineItemChargeDetails)
updateCustomLineItemResponse_chargeDetails = Lens.lens (\UpdateCustomLineItemResponse' {chargeDetails} -> chargeDetails) (\s@UpdateCustomLineItemResponse' {} a -> s {chargeDetails = a} :: UpdateCustomLineItemResponse)

-- | The ARN of the billing group that the custom line item is applied to.
updateCustomLineItemResponse_billingGroupArn :: Lens.Lens' UpdateCustomLineItemResponse (Prelude.Maybe Prelude.Text)
updateCustomLineItemResponse_billingGroupArn = Lens.lens (\UpdateCustomLineItemResponse' {billingGroupArn} -> billingGroupArn) (\s@UpdateCustomLineItemResponse' {} a -> s {billingGroupArn = a} :: UpdateCustomLineItemResponse)

-- | The ARN of the successfully updated custom line item.
updateCustomLineItemResponse_arn :: Lens.Lens' UpdateCustomLineItemResponse (Prelude.Maybe Prelude.Text)
updateCustomLineItemResponse_arn = Lens.lens (\UpdateCustomLineItemResponse' {arn} -> arn) (\s@UpdateCustomLineItemResponse' {} a -> s {arn = a} :: UpdateCustomLineItemResponse)

-- | The number of resources that are associated to the custom line item.
updateCustomLineItemResponse_associationSize :: Lens.Lens' UpdateCustomLineItemResponse (Prelude.Maybe Prelude.Natural)
updateCustomLineItemResponse_associationSize = Lens.lens (\UpdateCustomLineItemResponse' {associationSize} -> associationSize) (\s@UpdateCustomLineItemResponse' {} a -> s {associationSize = a} :: UpdateCustomLineItemResponse)

-- | The description of the successfully updated custom line item.
updateCustomLineItemResponse_description :: Lens.Lens' UpdateCustomLineItemResponse (Prelude.Maybe Prelude.Text)
updateCustomLineItemResponse_description = Lens.lens (\UpdateCustomLineItemResponse' {description} -> description) (\s@UpdateCustomLineItemResponse' {} a -> s {description = a} :: UpdateCustomLineItemResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The most recent time when the custom line item was modified.
updateCustomLineItemResponse_lastModifiedTime :: Lens.Lens' UpdateCustomLineItemResponse (Prelude.Maybe Prelude.Integer)
updateCustomLineItemResponse_lastModifiedTime = Lens.lens (\UpdateCustomLineItemResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateCustomLineItemResponse' {} a -> s {lastModifiedTime = a} :: UpdateCustomLineItemResponse)

-- | The response's http status code.
updateCustomLineItemResponse_httpStatus :: Lens.Lens' UpdateCustomLineItemResponse Prelude.Int
updateCustomLineItemResponse_httpStatus = Lens.lens (\UpdateCustomLineItemResponse' {httpStatus} -> httpStatus) (\s@UpdateCustomLineItemResponse' {} a -> s {httpStatus = a} :: UpdateCustomLineItemResponse)

instance Prelude.NFData UpdateCustomLineItemResponse where
  rnf UpdateCustomLineItemResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf chargeDetails
      `Prelude.seq` Prelude.rnf billingGroupArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf associationSize
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf httpStatus
