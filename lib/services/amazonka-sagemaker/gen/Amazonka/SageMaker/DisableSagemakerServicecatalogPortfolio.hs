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
-- Module      : Amazonka.SageMaker.DisableSagemakerServicecatalogPortfolio
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables using Service Catalog in SageMaker. Service Catalog is used to
-- create SageMaker projects.
module Amazonka.SageMaker.DisableSagemakerServicecatalogPortfolio
  ( -- * Creating a Request
    DisableSagemakerServicecatalogPortfolio (..),
    newDisableSagemakerServicecatalogPortfolio,

    -- * Destructuring the Response
    DisableSagemakerServicecatalogPortfolioResponse (..),
    newDisableSagemakerServicecatalogPortfolioResponse,

    -- * Response Lenses
    disableSagemakerServicecatalogPortfolioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDisableSagemakerServicecatalogPortfolio' smart constructor.
data DisableSagemakerServicecatalogPortfolio = DisableSagemakerServicecatalogPortfolio'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableSagemakerServicecatalogPortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableSagemakerServicecatalogPortfolio ::
  DisableSagemakerServicecatalogPortfolio
newDisableSagemakerServicecatalogPortfolio =
  DisableSagemakerServicecatalogPortfolio'

instance
  Core.AWSRequest
    DisableSagemakerServicecatalogPortfolio
  where
  type
    AWSResponse
      DisableSagemakerServicecatalogPortfolio =
      DisableSagemakerServicecatalogPortfolioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableSagemakerServicecatalogPortfolioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableSagemakerServicecatalogPortfolio
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DisableSagemakerServicecatalogPortfolio
  where
  rnf _ = ()

instance
  Core.ToHeaders
    DisableSagemakerServicecatalogPortfolio
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DisableSagemakerServicecatalogPortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DisableSagemakerServicecatalogPortfolio
  where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance
  Core.ToPath
    DisableSagemakerServicecatalogPortfolio
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DisableSagemakerServicecatalogPortfolio
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableSagemakerServicecatalogPortfolioResponse' smart constructor.
data DisableSagemakerServicecatalogPortfolioResponse = DisableSagemakerServicecatalogPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableSagemakerServicecatalogPortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableSagemakerServicecatalogPortfolioResponse_httpStatus' - The response's http status code.
newDisableSagemakerServicecatalogPortfolioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableSagemakerServicecatalogPortfolioResponse
newDisableSagemakerServicecatalogPortfolioResponse
  pHttpStatus_ =
    DisableSagemakerServicecatalogPortfolioResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disableSagemakerServicecatalogPortfolioResponse_httpStatus :: Lens.Lens' DisableSagemakerServicecatalogPortfolioResponse Prelude.Int
disableSagemakerServicecatalogPortfolioResponse_httpStatus = Lens.lens (\DisableSagemakerServicecatalogPortfolioResponse' {httpStatus} -> httpStatus) (\s@DisableSagemakerServicecatalogPortfolioResponse' {} a -> s {httpStatus = a} :: DisableSagemakerServicecatalogPortfolioResponse)

instance
  Prelude.NFData
    DisableSagemakerServicecatalogPortfolioResponse
  where
  rnf
    DisableSagemakerServicecatalogPortfolioResponse' {..} =
      Prelude.rnf httpStatus
