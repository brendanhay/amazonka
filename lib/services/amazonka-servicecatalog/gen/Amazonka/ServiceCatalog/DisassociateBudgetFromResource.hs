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
-- Module      : Amazonka.ServiceCatalog.DisassociateBudgetFromResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified budget from the specified resource.
module Amazonka.ServiceCatalog.DisassociateBudgetFromResource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDisassociateBudgetFromResource' smart constructor.
data DisassociateBudgetFromResource = DisassociateBudgetFromResource'
  { -- | The name of the budget you want to disassociate.
    budgetName :: Prelude.Text,
    -- | The resource identifier you want to disassociate from. Either a
    -- portfolio-id or a product-id.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
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
disassociateBudgetFromResource_budgetName :: Lens.Lens' DisassociateBudgetFromResource Prelude.Text
disassociateBudgetFromResource_budgetName = Lens.lens (\DisassociateBudgetFromResource' {budgetName} -> budgetName) (\s@DisassociateBudgetFromResource' {} a -> s {budgetName = a} :: DisassociateBudgetFromResource)

-- | The resource identifier you want to disassociate from. Either a
-- portfolio-id or a product-id.
disassociateBudgetFromResource_resourceId :: Lens.Lens' DisassociateBudgetFromResource Prelude.Text
disassociateBudgetFromResource_resourceId = Lens.lens (\DisassociateBudgetFromResource' {resourceId} -> resourceId) (\s@DisassociateBudgetFromResource' {} a -> s {resourceId = a} :: DisassociateBudgetFromResource)

instance
  Core.AWSRequest
    DisassociateBudgetFromResource
  where
  type
    AWSResponse DisassociateBudgetFromResource =
      DisassociateBudgetFromResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateBudgetFromResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateBudgetFromResource
  where
  hashWithSalt
    _salt
    DisassociateBudgetFromResource' {..} =
      _salt
        `Prelude.hashWithSalt` budgetName
        `Prelude.hashWithSalt` resourceId

instance
  Prelude.NFData
    DisassociateBudgetFromResource
  where
  rnf DisassociateBudgetFromResource' {..} =
    Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf resourceId

instance
  Data.ToHeaders
    DisassociateBudgetFromResource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DisassociateBudgetFromResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateBudgetFromResource where
  toJSON DisassociateBudgetFromResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("BudgetName" Data..= budgetName),
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath DisassociateBudgetFromResource where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateBudgetFromResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateBudgetFromResourceResponse' smart constructor.
data DisassociateBudgetFromResourceResponse = DisassociateBudgetFromResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DisassociateBudgetFromResourceResponse
newDisassociateBudgetFromResourceResponse
  pHttpStatus_ =
    DisassociateBudgetFromResourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateBudgetFromResourceResponse_httpStatus :: Lens.Lens' DisassociateBudgetFromResourceResponse Prelude.Int
disassociateBudgetFromResourceResponse_httpStatus = Lens.lens (\DisassociateBudgetFromResourceResponse' {httpStatus} -> httpStatus) (\s@DisassociateBudgetFromResourceResponse' {} a -> s {httpStatus = a} :: DisassociateBudgetFromResourceResponse)

instance
  Prelude.NFData
    DisassociateBudgetFromResourceResponse
  where
  rnf DisassociateBudgetFromResourceResponse' {..} =
    Prelude.rnf httpStatus
