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
-- Module      : Amazonka.CostExplorer.DescribeCostCategoryDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the name, Amazon Resource Name (ARN), rules, definition, and
-- effective dates of a Cost Category that\'s defined in the account.
--
-- You have the option to use @EffectiveOn@ to return a Cost Category
-- that\'s active on a specific date. If there\'s no @EffectiveOn@
-- specified, you see a Cost Category that\'s effective on the current
-- date. If Cost Category is still effective, @EffectiveEnd@ is omitted in
-- the response.
module Amazonka.CostExplorer.DescribeCostCategoryDefinition
  ( -- * Creating a Request
    DescribeCostCategoryDefinition (..),
    newDescribeCostCategoryDefinition,

    -- * Request Lenses
    describeCostCategoryDefinition_effectiveOn,
    describeCostCategoryDefinition_costCategoryArn,

    -- * Destructuring the Response
    DescribeCostCategoryDefinitionResponse (..),
    newDescribeCostCategoryDefinitionResponse,

    -- * Response Lenses
    describeCostCategoryDefinitionResponse_costCategory,
    describeCostCategoryDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCostCategoryDefinition' smart constructor.
data DescribeCostCategoryDefinition = DescribeCostCategoryDefinition'
  { -- | The date when the Cost Category was effective.
    effectiveOn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for your Cost Category.
    costCategoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCostCategoryDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effectiveOn', 'describeCostCategoryDefinition_effectiveOn' - The date when the Cost Category was effective.
--
-- 'costCategoryArn', 'describeCostCategoryDefinition_costCategoryArn' - The unique identifier for your Cost Category.
newDescribeCostCategoryDefinition ::
  -- | 'costCategoryArn'
  Prelude.Text ->
  DescribeCostCategoryDefinition
newDescribeCostCategoryDefinition pCostCategoryArn_ =
  DescribeCostCategoryDefinition'
    { effectiveOn =
        Prelude.Nothing,
      costCategoryArn = pCostCategoryArn_
    }

-- | The date when the Cost Category was effective.
describeCostCategoryDefinition_effectiveOn :: Lens.Lens' DescribeCostCategoryDefinition (Prelude.Maybe Prelude.Text)
describeCostCategoryDefinition_effectiveOn = Lens.lens (\DescribeCostCategoryDefinition' {effectiveOn} -> effectiveOn) (\s@DescribeCostCategoryDefinition' {} a -> s {effectiveOn = a} :: DescribeCostCategoryDefinition)

-- | The unique identifier for your Cost Category.
describeCostCategoryDefinition_costCategoryArn :: Lens.Lens' DescribeCostCategoryDefinition Prelude.Text
describeCostCategoryDefinition_costCategoryArn = Lens.lens (\DescribeCostCategoryDefinition' {costCategoryArn} -> costCategoryArn) (\s@DescribeCostCategoryDefinition' {} a -> s {costCategoryArn = a} :: DescribeCostCategoryDefinition)

instance
  Core.AWSRequest
    DescribeCostCategoryDefinition
  where
  type
    AWSResponse DescribeCostCategoryDefinition =
      DescribeCostCategoryDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCostCategoryDefinitionResponse'
            Prelude.<$> (x Core..?> "CostCategory")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCostCategoryDefinition
  where
  hashWithSalt
    _salt
    DescribeCostCategoryDefinition' {..} =
      _salt `Prelude.hashWithSalt` effectiveOn
        `Prelude.hashWithSalt` costCategoryArn

instance
  Prelude.NFData
    DescribeCostCategoryDefinition
  where
  rnf DescribeCostCategoryDefinition' {..} =
    Prelude.rnf effectiveOn
      `Prelude.seq` Prelude.rnf costCategoryArn

instance
  Core.ToHeaders
    DescribeCostCategoryDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.DescribeCostCategoryDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCostCategoryDefinition where
  toJSON DescribeCostCategoryDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EffectiveOn" Core..=) Prelude.<$> effectiveOn,
            Prelude.Just
              ("CostCategoryArn" Core..= costCategoryArn)
          ]
      )

instance Core.ToPath DescribeCostCategoryDefinition where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCostCategoryDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCostCategoryDefinitionResponse' smart constructor.
data DescribeCostCategoryDefinitionResponse = DescribeCostCategoryDefinitionResponse'
  { costCategory :: Prelude.Maybe CostCategory,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCostCategoryDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costCategory', 'describeCostCategoryDefinitionResponse_costCategory' - Undocumented member.
--
-- 'httpStatus', 'describeCostCategoryDefinitionResponse_httpStatus' - The response's http status code.
newDescribeCostCategoryDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCostCategoryDefinitionResponse
newDescribeCostCategoryDefinitionResponse
  pHttpStatus_ =
    DescribeCostCategoryDefinitionResponse'
      { costCategory =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeCostCategoryDefinitionResponse_costCategory :: Lens.Lens' DescribeCostCategoryDefinitionResponse (Prelude.Maybe CostCategory)
describeCostCategoryDefinitionResponse_costCategory = Lens.lens (\DescribeCostCategoryDefinitionResponse' {costCategory} -> costCategory) (\s@DescribeCostCategoryDefinitionResponse' {} a -> s {costCategory = a} :: DescribeCostCategoryDefinitionResponse)

-- | The response's http status code.
describeCostCategoryDefinitionResponse_httpStatus :: Lens.Lens' DescribeCostCategoryDefinitionResponse Prelude.Int
describeCostCategoryDefinitionResponse_httpStatus = Lens.lens (\DescribeCostCategoryDefinitionResponse' {httpStatus} -> httpStatus) (\s@DescribeCostCategoryDefinitionResponse' {} a -> s {httpStatus = a} :: DescribeCostCategoryDefinitionResponse)

instance
  Prelude.NFData
    DescribeCostCategoryDefinitionResponse
  where
  rnf DescribeCostCategoryDefinitionResponse' {..} =
    Prelude.rnf costCategory
      `Prelude.seq` Prelude.rnf httpStatus
