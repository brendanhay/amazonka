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
-- Module      : Network.AWS.ServiceCatalog.DisassociateBudgetFromResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified budget from the specified resource.
module Network.AWS.ServiceCatalog.DisassociateBudgetFromResource
  ( -- * Creating a Request
    DisassociateBudgetFromResource (..),
    newDisassociateBudgetFromResource,

    -- * Request Lenses
    disassociateBudgetFromResource_budgetName,
    disassociateBudgetFromResource_resourceId,

    -- * Destructuring the Response
    DisassociateBudgetFromResourceResponse (..),
    newDisassociateBudgetFromResourceResponse,

    -- * Response Lenses
    disassociateBudgetFromResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDisassociateBudgetFromResource' smart constructor.
data DisassociateBudgetFromResource = DisassociateBudgetFromResource'
  { -- | The name of the budget you want to disassociate.
    budgetName :: Core.Text,
    -- | The resource identifier you want to disassociate from. Either a
    -- portfolio-id or a product-id.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateBudgetFromResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'budgetName', 'disassociateBudgetFromResource_budgetName' - The name of the budget you want to disassociate.
--
-- 'resourceId', 'disassociateBudgetFromResource_resourceId' - The resource identifier you want to disassociate from. Either a
-- portfolio-id or a product-id.
newDisassociateBudgetFromResource ::
  -- | 'budgetName'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  DisassociateBudgetFromResource
newDisassociateBudgetFromResource
  pBudgetName_
  pResourceId_ =
    DisassociateBudgetFromResource'
      { budgetName =
          pBudgetName_,
        resourceId = pResourceId_
      }

-- | The name of the budget you want to disassociate.
disassociateBudgetFromResource_budgetName :: Lens.Lens' DisassociateBudgetFromResource Core.Text
disassociateBudgetFromResource_budgetName = Lens.lens (\DisassociateBudgetFromResource' {budgetName} -> budgetName) (\s@DisassociateBudgetFromResource' {} a -> s {budgetName = a} :: DisassociateBudgetFromResource)

-- | The resource identifier you want to disassociate from. Either a
-- portfolio-id or a product-id.
disassociateBudgetFromResource_resourceId :: Lens.Lens' DisassociateBudgetFromResource Core.Text
disassociateBudgetFromResource_resourceId = Lens.lens (\DisassociateBudgetFromResource' {resourceId} -> resourceId) (\s@DisassociateBudgetFromResource' {} a -> s {resourceId = a} :: DisassociateBudgetFromResource)

instance
  Core.AWSRequest
    DisassociateBudgetFromResource
  where
  type
    AWSResponse DisassociateBudgetFromResource =
      DisassociateBudgetFromResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateBudgetFromResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateBudgetFromResource

instance Core.NFData DisassociateBudgetFromResource

instance
  Core.ToHeaders
    DisassociateBudgetFromResource
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DisassociateBudgetFromResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateBudgetFromResource where
  toJSON DisassociateBudgetFromResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath DisassociateBudgetFromResource where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateBudgetFromResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateBudgetFromResourceResponse' smart constructor.
data DisassociateBudgetFromResourceResponse = DisassociateBudgetFromResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateBudgetFromResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateBudgetFromResourceResponse_httpStatus' - The response's http status code.
newDisassociateBudgetFromResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateBudgetFromResourceResponse
newDisassociateBudgetFromResourceResponse
  pHttpStatus_ =
    DisassociateBudgetFromResourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateBudgetFromResourceResponse_httpStatus :: Lens.Lens' DisassociateBudgetFromResourceResponse Core.Int
disassociateBudgetFromResourceResponse_httpStatus = Lens.lens (\DisassociateBudgetFromResourceResponse' {httpStatus} -> httpStatus) (\s@DisassociateBudgetFromResourceResponse' {} a -> s {httpStatus = a} :: DisassociateBudgetFromResourceResponse)

instance
  Core.NFData
    DisassociateBudgetFromResourceResponse
