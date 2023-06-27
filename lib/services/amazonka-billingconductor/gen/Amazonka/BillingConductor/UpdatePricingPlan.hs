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
-- Module      : Amazonka.BillingConductor.UpdatePricingPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This updates an existing pricing plan.
module Amazonka.BillingConductor.UpdatePricingPlan
  ( -- * Creating a Request
    UpdatePricingPlan (..),
    newUpdatePricingPlan,

    -- * Request Lenses
    updatePricingPlan_description,
    updatePricingPlan_name,
    updatePricingPlan_arn,

    -- * Destructuring the Response
    UpdatePricingPlanResponse (..),
    newUpdatePricingPlanResponse,

    -- * Response Lenses
    updatePricingPlanResponse_arn,
    updatePricingPlanResponse_description,
    updatePricingPlanResponse_lastModifiedTime,
    updatePricingPlanResponse_name,
    updatePricingPlanResponse_size,
    updatePricingPlanResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePricingPlan' smart constructor.
data UpdatePricingPlan = UpdatePricingPlan'
  { -- | The description of the pricing plan.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the pricing plan. The name must be unique to each pricing
    -- plan.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the pricing plan that you\'re
    -- updating.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePricingPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updatePricingPlan_description' - The description of the pricing plan.
--
-- 'name', 'updatePricingPlan_name' - The name of the pricing plan. The name must be unique to each pricing
-- plan.
--
-- 'arn', 'updatePricingPlan_arn' - The Amazon Resource Name (ARN) of the pricing plan that you\'re
-- updating.
newUpdatePricingPlan ::
  -- | 'arn'
  Prelude.Text ->
  UpdatePricingPlan
newUpdatePricingPlan pArn_ =
  UpdatePricingPlan'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      arn = pArn_
    }

-- | The description of the pricing plan.
updatePricingPlan_description :: Lens.Lens' UpdatePricingPlan (Prelude.Maybe Prelude.Text)
updatePricingPlan_description = Lens.lens (\UpdatePricingPlan' {description} -> description) (\s@UpdatePricingPlan' {} a -> s {description = a} :: UpdatePricingPlan) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the pricing plan. The name must be unique to each pricing
-- plan.
updatePricingPlan_name :: Lens.Lens' UpdatePricingPlan (Prelude.Maybe Prelude.Text)
updatePricingPlan_name = Lens.lens (\UpdatePricingPlan' {name} -> name) (\s@UpdatePricingPlan' {} a -> s {name = a} :: UpdatePricingPlan) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the pricing plan that you\'re
-- updating.
updatePricingPlan_arn :: Lens.Lens' UpdatePricingPlan Prelude.Text
updatePricingPlan_arn = Lens.lens (\UpdatePricingPlan' {arn} -> arn) (\s@UpdatePricingPlan' {} a -> s {arn = a} :: UpdatePricingPlan)

instance Core.AWSRequest UpdatePricingPlan where
  type
    AWSResponse UpdatePricingPlan =
      UpdatePricingPlanResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePricingPlanResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Size")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePricingPlan where
  hashWithSalt _salt UpdatePricingPlan' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdatePricingPlan where
  rnf UpdatePricingPlan' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders UpdatePricingPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePricingPlan where
  toJSON UpdatePricingPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("Arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdatePricingPlan where
  toPath = Prelude.const "/update-pricing-plan"

instance Data.ToQuery UpdatePricingPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePricingPlanResponse' smart constructor.
data UpdatePricingPlanResponse = UpdatePricingPlanResponse'
  { -- | The Amazon Resource Name (ARN) of the updated pricing plan.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The new description for the pricing rule.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The most recent time when the pricing plan was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The name of the pricing plan. The name must be unique to each pricing
    -- plan.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The pricing rules count that\'s currently associated with this pricing
    -- plan list.
    size :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePricingPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updatePricingPlanResponse_arn' - The Amazon Resource Name (ARN) of the updated pricing plan.
--
-- 'description', 'updatePricingPlanResponse_description' - The new description for the pricing rule.
--
-- 'lastModifiedTime', 'updatePricingPlanResponse_lastModifiedTime' - The most recent time when the pricing plan was modified.
--
-- 'name', 'updatePricingPlanResponse_name' - The name of the pricing plan. The name must be unique to each pricing
-- plan.
--
-- 'size', 'updatePricingPlanResponse_size' - The pricing rules count that\'s currently associated with this pricing
-- plan list.
--
-- 'httpStatus', 'updatePricingPlanResponse_httpStatus' - The response's http status code.
newUpdatePricingPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePricingPlanResponse
newUpdatePricingPlanResponse pHttpStatus_ =
  UpdatePricingPlanResponse'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      size = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated pricing plan.
updatePricingPlanResponse_arn :: Lens.Lens' UpdatePricingPlanResponse (Prelude.Maybe Prelude.Text)
updatePricingPlanResponse_arn = Lens.lens (\UpdatePricingPlanResponse' {arn} -> arn) (\s@UpdatePricingPlanResponse' {} a -> s {arn = a} :: UpdatePricingPlanResponse)

-- | The new description for the pricing rule.
updatePricingPlanResponse_description :: Lens.Lens' UpdatePricingPlanResponse (Prelude.Maybe Prelude.Text)
updatePricingPlanResponse_description = Lens.lens (\UpdatePricingPlanResponse' {description} -> description) (\s@UpdatePricingPlanResponse' {} a -> s {description = a} :: UpdatePricingPlanResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The most recent time when the pricing plan was modified.
updatePricingPlanResponse_lastModifiedTime :: Lens.Lens' UpdatePricingPlanResponse (Prelude.Maybe Prelude.Integer)
updatePricingPlanResponse_lastModifiedTime = Lens.lens (\UpdatePricingPlanResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdatePricingPlanResponse' {} a -> s {lastModifiedTime = a} :: UpdatePricingPlanResponse)

-- | The name of the pricing plan. The name must be unique to each pricing
-- plan.
updatePricingPlanResponse_name :: Lens.Lens' UpdatePricingPlanResponse (Prelude.Maybe Prelude.Text)
updatePricingPlanResponse_name = Lens.lens (\UpdatePricingPlanResponse' {name} -> name) (\s@UpdatePricingPlanResponse' {} a -> s {name = a} :: UpdatePricingPlanResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The pricing rules count that\'s currently associated with this pricing
-- plan list.
updatePricingPlanResponse_size :: Lens.Lens' UpdatePricingPlanResponse (Prelude.Maybe Prelude.Natural)
updatePricingPlanResponse_size = Lens.lens (\UpdatePricingPlanResponse' {size} -> size) (\s@UpdatePricingPlanResponse' {} a -> s {size = a} :: UpdatePricingPlanResponse)

-- | The response's http status code.
updatePricingPlanResponse_httpStatus :: Lens.Lens' UpdatePricingPlanResponse Prelude.Int
updatePricingPlanResponse_httpStatus = Lens.lens (\UpdatePricingPlanResponse' {httpStatus} -> httpStatus) (\s@UpdatePricingPlanResponse' {} a -> s {httpStatus = a} :: UpdatePricingPlanResponse)

instance Prelude.NFData UpdatePricingPlanResponse where
  rnf UpdatePricingPlanResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf httpStatus
