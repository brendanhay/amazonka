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
-- Module      : Amazonka.BillingConductor.DeleteCustomLineItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the custom line item identified by the given ARN in the current,
-- or previous billing period.
module Amazonka.BillingConductor.DeleteCustomLineItem
  ( -- * Creating a Request
    DeleteCustomLineItem (..),
    newDeleteCustomLineItem,

    -- * Request Lenses
    deleteCustomLineItem_billingPeriodRange,
    deleteCustomLineItem_arn,

    -- * Destructuring the Response
    DeleteCustomLineItemResponse (..),
    newDeleteCustomLineItemResponse,

    -- * Response Lenses
    deleteCustomLineItemResponse_arn,
    deleteCustomLineItemResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCustomLineItem' smart constructor.
data DeleteCustomLineItem = DeleteCustomLineItem'
  { billingPeriodRange :: Prelude.Maybe CustomLineItemBillingPeriodRange,
    -- | The ARN of the custom line item to be deleted.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomLineItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriodRange', 'deleteCustomLineItem_billingPeriodRange' - Undocumented member.
--
-- 'arn', 'deleteCustomLineItem_arn' - The ARN of the custom line item to be deleted.
newDeleteCustomLineItem ::
  -- | 'arn'
  Prelude.Text ->
  DeleteCustomLineItem
newDeleteCustomLineItem pArn_ =
  DeleteCustomLineItem'
    { billingPeriodRange =
        Prelude.Nothing,
      arn = pArn_
    }

-- | Undocumented member.
deleteCustomLineItem_billingPeriodRange :: Lens.Lens' DeleteCustomLineItem (Prelude.Maybe CustomLineItemBillingPeriodRange)
deleteCustomLineItem_billingPeriodRange = Lens.lens (\DeleteCustomLineItem' {billingPeriodRange} -> billingPeriodRange) (\s@DeleteCustomLineItem' {} a -> s {billingPeriodRange = a} :: DeleteCustomLineItem)

-- | The ARN of the custom line item to be deleted.
deleteCustomLineItem_arn :: Lens.Lens' DeleteCustomLineItem Prelude.Text
deleteCustomLineItem_arn = Lens.lens (\DeleteCustomLineItem' {arn} -> arn) (\s@DeleteCustomLineItem' {} a -> s {arn = a} :: DeleteCustomLineItem)

instance Core.AWSRequest DeleteCustomLineItem where
  type
    AWSResponse DeleteCustomLineItem =
      DeleteCustomLineItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCustomLineItemResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCustomLineItem where
  hashWithSalt _salt DeleteCustomLineItem' {..} =
    _salt `Prelude.hashWithSalt` billingPeriodRange
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteCustomLineItem where
  rnf DeleteCustomLineItem' {..} =
    Prelude.rnf billingPeriodRange
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders DeleteCustomLineItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCustomLineItem where
  toJSON DeleteCustomLineItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingPeriodRange" Data..=)
              Prelude.<$> billingPeriodRange,
            Prelude.Just ("Arn" Data..= arn)
          ]
      )

instance Data.ToPath DeleteCustomLineItem where
  toPath = Prelude.const "/delete-custom-line-item"

instance Data.ToQuery DeleteCustomLineItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomLineItemResponse' smart constructor.
data DeleteCustomLineItemResponse = DeleteCustomLineItemResponse'
  { -- | Then ARN of the deleted custom line item.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomLineItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteCustomLineItemResponse_arn' - Then ARN of the deleted custom line item.
--
-- 'httpStatus', 'deleteCustomLineItemResponse_httpStatus' - The response's http status code.
newDeleteCustomLineItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCustomLineItemResponse
newDeleteCustomLineItemResponse pHttpStatus_ =
  DeleteCustomLineItemResponse'
    { arn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Then ARN of the deleted custom line item.
deleteCustomLineItemResponse_arn :: Lens.Lens' DeleteCustomLineItemResponse (Prelude.Maybe Prelude.Text)
deleteCustomLineItemResponse_arn = Lens.lens (\DeleteCustomLineItemResponse' {arn} -> arn) (\s@DeleteCustomLineItemResponse' {} a -> s {arn = a} :: DeleteCustomLineItemResponse)

-- | The response's http status code.
deleteCustomLineItemResponse_httpStatus :: Lens.Lens' DeleteCustomLineItemResponse Prelude.Int
deleteCustomLineItemResponse_httpStatus = Lens.lens (\DeleteCustomLineItemResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomLineItemResponse' {} a -> s {httpStatus = a} :: DeleteCustomLineItemResponse)

instance Prelude.NFData DeleteCustomLineItemResponse where
  rnf DeleteCustomLineItemResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
