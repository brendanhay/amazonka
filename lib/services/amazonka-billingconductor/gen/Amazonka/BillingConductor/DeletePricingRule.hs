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
-- Module      : Amazonka.BillingConductor.DeletePricingRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the pricing rule that\'s identified by the input Amazon Resource
-- Name (ARN).
module Amazonka.BillingConductor.DeletePricingRule
  ( -- * Creating a Request
    DeletePricingRule (..),
    newDeletePricingRule,

    -- * Request Lenses
    deletePricingRule_arn,

    -- * Destructuring the Response
    DeletePricingRuleResponse (..),
    newDeletePricingRuleResponse,

    -- * Response Lenses
    deletePricingRuleResponse_arn,
    deletePricingRuleResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePricingRule' smart constructor.
data DeletePricingRule = DeletePricingRule'
  { -- | The Amazon Resource Name (ARN) of the pricing rule that you are
    -- deleting.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePricingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deletePricingRule_arn' - The Amazon Resource Name (ARN) of the pricing rule that you are
-- deleting.
newDeletePricingRule ::
  -- | 'arn'
  Prelude.Text ->
  DeletePricingRule
newDeletePricingRule pArn_ =
  DeletePricingRule' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the pricing rule that you are
-- deleting.
deletePricingRule_arn :: Lens.Lens' DeletePricingRule Prelude.Text
deletePricingRule_arn = Lens.lens (\DeletePricingRule' {arn} -> arn) (\s@DeletePricingRule' {} a -> s {arn = a} :: DeletePricingRule)

instance Core.AWSRequest DeletePricingRule where
  type
    AWSResponse DeletePricingRule =
      DeletePricingRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePricingRuleResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePricingRule where
  hashWithSalt _salt DeletePricingRule' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeletePricingRule where
  rnf DeletePricingRule' {..} = Prelude.rnf arn

instance Data.ToHeaders DeletePricingRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePricingRule where
  toJSON DeletePricingRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Data..= arn)]
      )

instance Data.ToPath DeletePricingRule where
  toPath = Prelude.const "/delete-pricing-rule"

instance Data.ToQuery DeletePricingRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePricingRuleResponse' smart constructor.
data DeletePricingRuleResponse = DeletePricingRuleResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted pricing rule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePricingRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deletePricingRuleResponse_arn' - The Amazon Resource Name (ARN) of the deleted pricing rule.
--
-- 'httpStatus', 'deletePricingRuleResponse_httpStatus' - The response's http status code.
newDeletePricingRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePricingRuleResponse
newDeletePricingRuleResponse pHttpStatus_ =
  DeletePricingRuleResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted pricing rule.
deletePricingRuleResponse_arn :: Lens.Lens' DeletePricingRuleResponse (Prelude.Maybe Prelude.Text)
deletePricingRuleResponse_arn = Lens.lens (\DeletePricingRuleResponse' {arn} -> arn) (\s@DeletePricingRuleResponse' {} a -> s {arn = a} :: DeletePricingRuleResponse)

-- | The response's http status code.
deletePricingRuleResponse_httpStatus :: Lens.Lens' DeletePricingRuleResponse Prelude.Int
deletePricingRuleResponse_httpStatus = Lens.lens (\DeletePricingRuleResponse' {httpStatus} -> httpStatus) (\s@DeletePricingRuleResponse' {} a -> s {httpStatus = a} :: DeletePricingRuleResponse)

instance Prelude.NFData DeletePricingRuleResponse where
  rnf DeletePricingRuleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
