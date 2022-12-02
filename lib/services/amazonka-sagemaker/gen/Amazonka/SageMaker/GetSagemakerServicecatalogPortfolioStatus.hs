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
-- Module      : Amazonka.SageMaker.GetSagemakerServicecatalogPortfolioStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of Service Catalog in SageMaker. Service Catalog is used
-- to create SageMaker projects.
module Amazonka.SageMaker.GetSagemakerServicecatalogPortfolioStatus
  ( -- * Creating a Request
    GetSagemakerServicecatalogPortfolioStatus (..),
    newGetSagemakerServicecatalogPortfolioStatus,

    -- * Destructuring the Response
    GetSagemakerServicecatalogPortfolioStatusResponse (..),
    newGetSagemakerServicecatalogPortfolioStatusResponse,

    -- * Response Lenses
    getSagemakerServicecatalogPortfolioStatusResponse_status,
    getSagemakerServicecatalogPortfolioStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newGetSagemakerServicecatalogPortfolioStatus' smart constructor.
data GetSagemakerServicecatalogPortfolioStatus = GetSagemakerServicecatalogPortfolioStatus'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSagemakerServicecatalogPortfolioStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetSagemakerServicecatalogPortfolioStatus ::
  GetSagemakerServicecatalogPortfolioStatus
newGetSagemakerServicecatalogPortfolioStatus =
  GetSagemakerServicecatalogPortfolioStatus'

instance
  Core.AWSRequest
    GetSagemakerServicecatalogPortfolioStatus
  where
  type
    AWSResponse
      GetSagemakerServicecatalogPortfolioStatus =
      GetSagemakerServicecatalogPortfolioStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSagemakerServicecatalogPortfolioStatusResponse'
            Prelude.<$> (x Data..?> "Status")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSagemakerServicecatalogPortfolioStatus
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetSagemakerServicecatalogPortfolioStatus
  where
  rnf _ = ()

instance
  Data.ToHeaders
    GetSagemakerServicecatalogPortfolioStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.GetSagemakerServicecatalogPortfolioStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetSagemakerServicecatalogPortfolioStatus
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    GetSagemakerServicecatalogPortfolioStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetSagemakerServicecatalogPortfolioStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSagemakerServicecatalogPortfolioStatusResponse' smart constructor.
data GetSagemakerServicecatalogPortfolioStatusResponse = GetSagemakerServicecatalogPortfolioStatusResponse'
  { -- | Whether Service Catalog is enabled or disabled in SageMaker.
    status :: Prelude.Maybe SagemakerServicecatalogStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSagemakerServicecatalogPortfolioStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getSagemakerServicecatalogPortfolioStatusResponse_status' - Whether Service Catalog is enabled or disabled in SageMaker.
--
-- 'httpStatus', 'getSagemakerServicecatalogPortfolioStatusResponse_httpStatus' - The response's http status code.
newGetSagemakerServicecatalogPortfolioStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSagemakerServicecatalogPortfolioStatusResponse
newGetSagemakerServicecatalogPortfolioStatusResponse
  pHttpStatus_ =
    GetSagemakerServicecatalogPortfolioStatusResponse'
      { status =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Whether Service Catalog is enabled or disabled in SageMaker.
getSagemakerServicecatalogPortfolioStatusResponse_status :: Lens.Lens' GetSagemakerServicecatalogPortfolioStatusResponse (Prelude.Maybe SagemakerServicecatalogStatus)
getSagemakerServicecatalogPortfolioStatusResponse_status = Lens.lens (\GetSagemakerServicecatalogPortfolioStatusResponse' {status} -> status) (\s@GetSagemakerServicecatalogPortfolioStatusResponse' {} a -> s {status = a} :: GetSagemakerServicecatalogPortfolioStatusResponse)

-- | The response's http status code.
getSagemakerServicecatalogPortfolioStatusResponse_httpStatus :: Lens.Lens' GetSagemakerServicecatalogPortfolioStatusResponse Prelude.Int
getSagemakerServicecatalogPortfolioStatusResponse_httpStatus = Lens.lens (\GetSagemakerServicecatalogPortfolioStatusResponse' {httpStatus} -> httpStatus) (\s@GetSagemakerServicecatalogPortfolioStatusResponse' {} a -> s {httpStatus = a} :: GetSagemakerServicecatalogPortfolioStatusResponse)

instance
  Prelude.NFData
    GetSagemakerServicecatalogPortfolioStatusResponse
  where
  rnf
    GetSagemakerServicecatalogPortfolioStatusResponse' {..} =
      Prelude.rnf status
        `Prelude.seq` Prelude.rnf httpStatus
