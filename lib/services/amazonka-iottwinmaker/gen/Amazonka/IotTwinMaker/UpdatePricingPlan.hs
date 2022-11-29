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
-- Module      : Amazonka.IotTwinMaker.UpdatePricingPlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the pricing plan.
module Amazonka.IotTwinMaker.UpdatePricingPlan
  ( -- * Creating a Request
    UpdatePricingPlan (..),
    newUpdatePricingPlan,

    -- * Request Lenses
    updatePricingPlan_bundleNames,
    updatePricingPlan_pricingMode,

    -- * Destructuring the Response
    UpdatePricingPlanResponse (..),
    newUpdatePricingPlanResponse,

    -- * Response Lenses
    updatePricingPlanResponse_pendingPricingPlan,
    updatePricingPlanResponse_httpStatus,
    updatePricingPlanResponse_currentPricingPlan,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePricingPlan' smart constructor.
data UpdatePricingPlan = UpdatePricingPlan'
  { -- | The bundle names.
    bundleNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The pricing mode.
    pricingMode :: PricingMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePricingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleNames', 'updatePricingPlan_bundleNames' - The bundle names.
--
-- 'pricingMode', 'updatePricingPlan_pricingMode' - The pricing mode.
newUpdatePricingPlan ::
  -- | 'pricingMode'
  PricingMode ->
  UpdatePricingPlan
newUpdatePricingPlan pPricingMode_ =
  UpdatePricingPlan'
    { bundleNames = Prelude.Nothing,
      pricingMode = pPricingMode_
    }

-- | The bundle names.
updatePricingPlan_bundleNames :: Lens.Lens' UpdatePricingPlan (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updatePricingPlan_bundleNames = Lens.lens (\UpdatePricingPlan' {bundleNames} -> bundleNames) (\s@UpdatePricingPlan' {} a -> s {bundleNames = a} :: UpdatePricingPlan) Prelude.. Lens.mapping Lens.coerced

-- | The pricing mode.
updatePricingPlan_pricingMode :: Lens.Lens' UpdatePricingPlan PricingMode
updatePricingPlan_pricingMode = Lens.lens (\UpdatePricingPlan' {pricingMode} -> pricingMode) (\s@UpdatePricingPlan' {} a -> s {pricingMode = a} :: UpdatePricingPlan)

instance Core.AWSRequest UpdatePricingPlan where
  type
    AWSResponse UpdatePricingPlan =
      UpdatePricingPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePricingPlanResponse'
            Prelude.<$> (x Core..?> "pendingPricingPlan")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "currentPricingPlan")
      )

instance Prelude.Hashable UpdatePricingPlan where
  hashWithSalt _salt UpdatePricingPlan' {..} =
    _salt `Prelude.hashWithSalt` bundleNames
      `Prelude.hashWithSalt` pricingMode

instance Prelude.NFData UpdatePricingPlan where
  rnf UpdatePricingPlan' {..} =
    Prelude.rnf bundleNames
      `Prelude.seq` Prelude.rnf pricingMode

instance Core.ToHeaders UpdatePricingPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdatePricingPlan where
  toJSON UpdatePricingPlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bundleNames" Core..=) Prelude.<$> bundleNames,
            Prelude.Just ("pricingMode" Core..= pricingMode)
          ]
      )

instance Core.ToPath UpdatePricingPlan where
  toPath = Prelude.const "/pricingplan"

instance Core.ToQuery UpdatePricingPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePricingPlanResponse' smart constructor.
data UpdatePricingPlanResponse = UpdatePricingPlanResponse'
  { -- | Update the pending pricing plan.
    pendingPricingPlan :: Prelude.Maybe PricingPlan,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Update the current pricing plan.
    currentPricingPlan :: PricingPlan
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePricingPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingPricingPlan', 'updatePricingPlanResponse_pendingPricingPlan' - Update the pending pricing plan.
--
-- 'httpStatus', 'updatePricingPlanResponse_httpStatus' - The response's http status code.
--
-- 'currentPricingPlan', 'updatePricingPlanResponse_currentPricingPlan' - Update the current pricing plan.
newUpdatePricingPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'currentPricingPlan'
  PricingPlan ->
  UpdatePricingPlanResponse
newUpdatePricingPlanResponse
  pHttpStatus_
  pCurrentPricingPlan_ =
    UpdatePricingPlanResponse'
      { pendingPricingPlan =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        currentPricingPlan = pCurrentPricingPlan_
      }

-- | Update the pending pricing plan.
updatePricingPlanResponse_pendingPricingPlan :: Lens.Lens' UpdatePricingPlanResponse (Prelude.Maybe PricingPlan)
updatePricingPlanResponse_pendingPricingPlan = Lens.lens (\UpdatePricingPlanResponse' {pendingPricingPlan} -> pendingPricingPlan) (\s@UpdatePricingPlanResponse' {} a -> s {pendingPricingPlan = a} :: UpdatePricingPlanResponse)

-- | The response's http status code.
updatePricingPlanResponse_httpStatus :: Lens.Lens' UpdatePricingPlanResponse Prelude.Int
updatePricingPlanResponse_httpStatus = Lens.lens (\UpdatePricingPlanResponse' {httpStatus} -> httpStatus) (\s@UpdatePricingPlanResponse' {} a -> s {httpStatus = a} :: UpdatePricingPlanResponse)

-- | Update the current pricing plan.
updatePricingPlanResponse_currentPricingPlan :: Lens.Lens' UpdatePricingPlanResponse PricingPlan
updatePricingPlanResponse_currentPricingPlan = Lens.lens (\UpdatePricingPlanResponse' {currentPricingPlan} -> currentPricingPlan) (\s@UpdatePricingPlanResponse' {} a -> s {currentPricingPlan = a} :: UpdatePricingPlanResponse)

instance Prelude.NFData UpdatePricingPlanResponse where
  rnf UpdatePricingPlanResponse' {..} =
    Prelude.rnf pendingPricingPlan
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf currentPricingPlan
