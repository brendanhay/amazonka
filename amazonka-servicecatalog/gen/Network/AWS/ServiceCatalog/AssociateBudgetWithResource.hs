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
-- Module      : Network.AWS.ServiceCatalog.AssociateBudgetWithResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified budget with the specified resource.
module Network.AWS.ServiceCatalog.AssociateBudgetWithResource
  ( -- * Creating a Request
    AssociateBudgetWithResource (..),
    newAssociateBudgetWithResource,

    -- * Request Lenses
    associateBudgetWithResource_budgetName,
    associateBudgetWithResource_resourceId,

    -- * Destructuring the Response
    AssociateBudgetWithResourceResponse (..),
    newAssociateBudgetWithResourceResponse,

    -- * Response Lenses
    associateBudgetWithResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newAssociateBudgetWithResource' smart constructor.
data AssociateBudgetWithResource = AssociateBudgetWithResource'
  { -- | The name of the budget you want to associate.
    budgetName :: Core.Text,
    -- | The resource identifier. Either a portfolio-id or a product-id.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateBudgetWithResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'budgetName', 'associateBudgetWithResource_budgetName' - The name of the budget you want to associate.
--
-- 'resourceId', 'associateBudgetWithResource_resourceId' - The resource identifier. Either a portfolio-id or a product-id.
newAssociateBudgetWithResource ::
  -- | 'budgetName'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  AssociateBudgetWithResource
newAssociateBudgetWithResource
  pBudgetName_
  pResourceId_ =
    AssociateBudgetWithResource'
      { budgetName =
          pBudgetName_,
        resourceId = pResourceId_
      }

-- | The name of the budget you want to associate.
associateBudgetWithResource_budgetName :: Lens.Lens' AssociateBudgetWithResource Core.Text
associateBudgetWithResource_budgetName = Lens.lens (\AssociateBudgetWithResource' {budgetName} -> budgetName) (\s@AssociateBudgetWithResource' {} a -> s {budgetName = a} :: AssociateBudgetWithResource)

-- | The resource identifier. Either a portfolio-id or a product-id.
associateBudgetWithResource_resourceId :: Lens.Lens' AssociateBudgetWithResource Core.Text
associateBudgetWithResource_resourceId = Lens.lens (\AssociateBudgetWithResource' {resourceId} -> resourceId) (\s@AssociateBudgetWithResource' {} a -> s {resourceId = a} :: AssociateBudgetWithResource)

instance Core.AWSRequest AssociateBudgetWithResource where
  type
    AWSResponse AssociateBudgetWithResource =
      AssociateBudgetWithResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateBudgetWithResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateBudgetWithResource

instance Core.NFData AssociateBudgetWithResource

instance Core.ToHeaders AssociateBudgetWithResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.AssociateBudgetWithResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateBudgetWithResource where
  toJSON AssociateBudgetWithResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath AssociateBudgetWithResource where
  toPath = Core.const "/"

instance Core.ToQuery AssociateBudgetWithResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateBudgetWithResourceResponse' smart constructor.
data AssociateBudgetWithResourceResponse = AssociateBudgetWithResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateBudgetWithResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateBudgetWithResourceResponse_httpStatus' - The response's http status code.
newAssociateBudgetWithResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateBudgetWithResourceResponse
newAssociateBudgetWithResourceResponse pHttpStatus_ =
  AssociateBudgetWithResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateBudgetWithResourceResponse_httpStatus :: Lens.Lens' AssociateBudgetWithResourceResponse Core.Int
associateBudgetWithResourceResponse_httpStatus = Lens.lens (\AssociateBudgetWithResourceResponse' {httpStatus} -> httpStatus) (\s@AssociateBudgetWithResourceResponse' {} a -> s {httpStatus = a} :: AssociateBudgetWithResourceResponse)

instance
  Core.NFData
    AssociateBudgetWithResourceResponse
