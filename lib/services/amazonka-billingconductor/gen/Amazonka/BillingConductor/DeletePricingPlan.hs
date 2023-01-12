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
-- Module      : Amazonka.BillingConductor.DeletePricingPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a pricing plan. The pricing plan must not be associated with any
-- billing groups to delete successfully.
module Amazonka.BillingConductor.DeletePricingPlan
  ( -- * Creating a Request
    DeletePricingPlan (..),
    newDeletePricingPlan,

    -- * Request Lenses
    deletePricingPlan_arn,

    -- * Destructuring the Response
    DeletePricingPlanResponse (..),
    newDeletePricingPlanResponse,

    -- * Response Lenses
    deletePricingPlanResponse_arn,
    deletePricingPlanResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePricingPlan' smart constructor.
data DeletePricingPlan = DeletePricingPlan'
  { -- | The Amazon Resource Name (ARN) of the pricing plan that you\'re
    -- deleting.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePricingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deletePricingPlan_arn' - The Amazon Resource Name (ARN) of the pricing plan that you\'re
-- deleting.
newDeletePricingPlan ::
  -- | 'arn'
  Prelude.Text ->
  DeletePricingPlan
newDeletePricingPlan pArn_ =
  DeletePricingPlan' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the pricing plan that you\'re
-- deleting.
deletePricingPlan_arn :: Lens.Lens' DeletePricingPlan Prelude.Text
deletePricingPlan_arn = Lens.lens (\DeletePricingPlan' {arn} -> arn) (\s@DeletePricingPlan' {} a -> s {arn = a} :: DeletePricingPlan)

instance Core.AWSRequest DeletePricingPlan where
  type
    AWSResponse DeletePricingPlan =
      DeletePricingPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePricingPlanResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePricingPlan where
  hashWithSalt _salt DeletePricingPlan' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeletePricingPlan where
  rnf DeletePricingPlan' {..} = Prelude.rnf arn

instance Data.ToHeaders DeletePricingPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePricingPlan where
  toJSON DeletePricingPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Data..= arn)]
      )

instance Data.ToPath DeletePricingPlan where
  toPath = Prelude.const "/delete-pricing-plan"

instance Data.ToQuery DeletePricingPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePricingPlanResponse' smart constructor.
data DeletePricingPlanResponse = DeletePricingPlanResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted pricing plan.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePricingPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deletePricingPlanResponse_arn' - The Amazon Resource Name (ARN) of the deleted pricing plan.
--
-- 'httpStatus', 'deletePricingPlanResponse_httpStatus' - The response's http status code.
newDeletePricingPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePricingPlanResponse
newDeletePricingPlanResponse pHttpStatus_ =
  DeletePricingPlanResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted pricing plan.
deletePricingPlanResponse_arn :: Lens.Lens' DeletePricingPlanResponse (Prelude.Maybe Prelude.Text)
deletePricingPlanResponse_arn = Lens.lens (\DeletePricingPlanResponse' {arn} -> arn) (\s@DeletePricingPlanResponse' {} a -> s {arn = a} :: DeletePricingPlanResponse)

-- | The response's http status code.
deletePricingPlanResponse_httpStatus :: Lens.Lens' DeletePricingPlanResponse Prelude.Int
deletePricingPlanResponse_httpStatus = Lens.lens (\DeletePricingPlanResponse' {httpStatus} -> httpStatus) (\s@DeletePricingPlanResponse' {} a -> s {httpStatus = a} :: DeletePricingPlanResponse)

instance Prelude.NFData DeletePricingPlanResponse where
  rnf DeletePricingPlanResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
