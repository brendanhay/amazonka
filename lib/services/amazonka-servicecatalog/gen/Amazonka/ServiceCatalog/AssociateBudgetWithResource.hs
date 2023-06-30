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
-- Module      : Amazonka.ServiceCatalog.AssociateBudgetWithResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified budget with the specified resource.
module Amazonka.ServiceCatalog.AssociateBudgetWithResource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newAssociateBudgetWithResource' smart constructor.
data AssociateBudgetWithResource = AssociateBudgetWithResource'
  { -- | The name of the budget you want to associate.
    budgetName :: Prelude.Text,
    -- | The resource identifier. Either a portfolio-id or a product-id.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
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
associateBudgetWithResource_budgetName :: Lens.Lens' AssociateBudgetWithResource Prelude.Text
associateBudgetWithResource_budgetName = Lens.lens (\AssociateBudgetWithResource' {budgetName} -> budgetName) (\s@AssociateBudgetWithResource' {} a -> s {budgetName = a} :: AssociateBudgetWithResource)

-- | The resource identifier. Either a portfolio-id or a product-id.
associateBudgetWithResource_resourceId :: Lens.Lens' AssociateBudgetWithResource Prelude.Text
associateBudgetWithResource_resourceId = Lens.lens (\AssociateBudgetWithResource' {resourceId} -> resourceId) (\s@AssociateBudgetWithResource' {} a -> s {resourceId = a} :: AssociateBudgetWithResource)

instance Core.AWSRequest AssociateBudgetWithResource where
  type
    AWSResponse AssociateBudgetWithResource =
      AssociateBudgetWithResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateBudgetWithResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateBudgetWithResource where
  hashWithSalt _salt AssociateBudgetWithResource' {..} =
    _salt
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData AssociateBudgetWithResource where
  rnf AssociateBudgetWithResource' {..} =
    Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders AssociateBudgetWithResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.AssociateBudgetWithResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateBudgetWithResource where
  toJSON AssociateBudgetWithResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("BudgetName" Data..= budgetName),
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath AssociateBudgetWithResource where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateBudgetWithResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateBudgetWithResourceResponse' smart constructor.
data AssociateBudgetWithResourceResponse = AssociateBudgetWithResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AssociateBudgetWithResourceResponse
newAssociateBudgetWithResourceResponse pHttpStatus_ =
  AssociateBudgetWithResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateBudgetWithResourceResponse_httpStatus :: Lens.Lens' AssociateBudgetWithResourceResponse Prelude.Int
associateBudgetWithResourceResponse_httpStatus = Lens.lens (\AssociateBudgetWithResourceResponse' {httpStatus} -> httpStatus) (\s@AssociateBudgetWithResourceResponse' {} a -> s {httpStatus = a} :: AssociateBudgetWithResourceResponse)

instance
  Prelude.NFData
    AssociateBudgetWithResourceResponse
  where
  rnf AssociateBudgetWithResourceResponse' {..} =
    Prelude.rnf httpStatus
