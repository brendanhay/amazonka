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
-- Module      : Amazonka.IotTwinMaker.GetPricingPlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the pricing plan.
module Amazonka.IotTwinMaker.GetPricingPlan
  ( -- * Creating a Request
    GetPricingPlan (..),
    newGetPricingPlan,

    -- * Destructuring the Response
    GetPricingPlanResponse (..),
    newGetPricingPlanResponse,

    -- * Response Lenses
    getPricingPlanResponse_pendingPricingPlan,
    getPricingPlanResponse_httpStatus,
    getPricingPlanResponse_currentPricingPlan,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPricingPlan' smart constructor.
data GetPricingPlan = GetPricingPlan'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPricingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetPricingPlan ::
  GetPricingPlan
newGetPricingPlan = GetPricingPlan'

instance Core.AWSRequest GetPricingPlan where
  type
    AWSResponse GetPricingPlan =
      GetPricingPlanResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPricingPlanResponse'
            Prelude.<$> (x Core..?> "pendingPricingPlan")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "currentPricingPlan")
      )

instance Prelude.Hashable GetPricingPlan where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetPricingPlan where
  rnf _ = ()

instance Core.ToHeaders GetPricingPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetPricingPlan where
  toPath = Prelude.const "/pricingplan"

instance Core.ToQuery GetPricingPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPricingPlanResponse' smart constructor.
data GetPricingPlanResponse = GetPricingPlanResponse'
  { -- | The pending pricing plan.
    pendingPricingPlan :: Prelude.Maybe PricingPlan,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The chosen pricing plan for the current billing cycle.
    currentPricingPlan :: PricingPlan
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPricingPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingPricingPlan', 'getPricingPlanResponse_pendingPricingPlan' - The pending pricing plan.
--
-- 'httpStatus', 'getPricingPlanResponse_httpStatus' - The response's http status code.
--
-- 'currentPricingPlan', 'getPricingPlanResponse_currentPricingPlan' - The chosen pricing plan for the current billing cycle.
newGetPricingPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'currentPricingPlan'
  PricingPlan ->
  GetPricingPlanResponse
newGetPricingPlanResponse
  pHttpStatus_
  pCurrentPricingPlan_ =
    GetPricingPlanResponse'
      { pendingPricingPlan =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        currentPricingPlan = pCurrentPricingPlan_
      }

-- | The pending pricing plan.
getPricingPlanResponse_pendingPricingPlan :: Lens.Lens' GetPricingPlanResponse (Prelude.Maybe PricingPlan)
getPricingPlanResponse_pendingPricingPlan = Lens.lens (\GetPricingPlanResponse' {pendingPricingPlan} -> pendingPricingPlan) (\s@GetPricingPlanResponse' {} a -> s {pendingPricingPlan = a} :: GetPricingPlanResponse)

-- | The response's http status code.
getPricingPlanResponse_httpStatus :: Lens.Lens' GetPricingPlanResponse Prelude.Int
getPricingPlanResponse_httpStatus = Lens.lens (\GetPricingPlanResponse' {httpStatus} -> httpStatus) (\s@GetPricingPlanResponse' {} a -> s {httpStatus = a} :: GetPricingPlanResponse)

-- | The chosen pricing plan for the current billing cycle.
getPricingPlanResponse_currentPricingPlan :: Lens.Lens' GetPricingPlanResponse PricingPlan
getPricingPlanResponse_currentPricingPlan = Lens.lens (\GetPricingPlanResponse' {currentPricingPlan} -> currentPricingPlan) (\s@GetPricingPlanResponse' {} a -> s {currentPricingPlan = a} :: GetPricingPlanResponse)

instance Prelude.NFData GetPricingPlanResponse where
  rnf GetPricingPlanResponse' {..} =
    Prelude.rnf pendingPricingPlan
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf currentPricingPlan
