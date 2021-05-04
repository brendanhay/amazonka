{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.GetSagemakerServicecatalogPortfolioStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of Service Catalog in SageMaker. Service Catalog is used
-- to create SageMaker projects.
module Network.AWS.SageMaker.GetSagemakerServicecatalogPortfolioStatus
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newGetSagemakerServicecatalogPortfolioStatus' smart constructor.
data GetSagemakerServicecatalogPortfolioStatus = GetSagemakerServicecatalogPortfolioStatus'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetSagemakerServicecatalogPortfolioStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetSagemakerServicecatalogPortfolioStatus ::
  GetSagemakerServicecatalogPortfolioStatus
newGetSagemakerServicecatalogPortfolioStatus =
  GetSagemakerServicecatalogPortfolioStatus'

instance
  Prelude.AWSRequest
    GetSagemakerServicecatalogPortfolioStatus
  where
  type
    Rs GetSagemakerServicecatalogPortfolioStatus =
      GetSagemakerServicecatalogPortfolioStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSagemakerServicecatalogPortfolioStatusResponse'
            Prelude.<$> (x Prelude..?> "Status")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSagemakerServicecatalogPortfolioStatus

instance
  Prelude.NFData
    GetSagemakerServicecatalogPortfolioStatus

instance
  Prelude.ToHeaders
    GetSagemakerServicecatalogPortfolioStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.GetSagemakerServicecatalogPortfolioStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    GetSagemakerServicecatalogPortfolioStatus
  where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance
  Prelude.ToPath
    GetSagemakerServicecatalogPortfolioStatus
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
