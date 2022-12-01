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
-- Module      : Amazonka.SageMaker.EnableSagemakerServicecatalogPortfolio
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables using Service Catalog in SageMaker. Service Catalog is used to
-- create SageMaker projects.
module Amazonka.SageMaker.EnableSagemakerServicecatalogPortfolio
  ( -- * Creating a Request
    EnableSagemakerServicecatalogPortfolio (..),
    newEnableSagemakerServicecatalogPortfolio,

    -- * Destructuring the Response
    EnableSagemakerServicecatalogPortfolioResponse (..),
    newEnableSagemakerServicecatalogPortfolioResponse,

    -- * Response Lenses
    enableSagemakerServicecatalogPortfolioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newEnableSagemakerServicecatalogPortfolio' smart constructor.
data EnableSagemakerServicecatalogPortfolio = EnableSagemakerServicecatalogPortfolio'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableSagemakerServicecatalogPortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableSagemakerServicecatalogPortfolio ::
  EnableSagemakerServicecatalogPortfolio
newEnableSagemakerServicecatalogPortfolio =
  EnableSagemakerServicecatalogPortfolio'

instance
  Core.AWSRequest
    EnableSagemakerServicecatalogPortfolio
  where
  type
    AWSResponse
      EnableSagemakerServicecatalogPortfolio =
      EnableSagemakerServicecatalogPortfolioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableSagemakerServicecatalogPortfolioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableSagemakerServicecatalogPortfolio
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    EnableSagemakerServicecatalogPortfolio
  where
  rnf _ = ()

instance
  Core.ToHeaders
    EnableSagemakerServicecatalogPortfolio
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.EnableSagemakerServicecatalogPortfolio" ::
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
    EnableSagemakerServicecatalogPortfolio
  where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance
  Core.ToPath
    EnableSagemakerServicecatalogPortfolio
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    EnableSagemakerServicecatalogPortfolio
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableSagemakerServicecatalogPortfolioResponse' smart constructor.
data EnableSagemakerServicecatalogPortfolioResponse = EnableSagemakerServicecatalogPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableSagemakerServicecatalogPortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableSagemakerServicecatalogPortfolioResponse_httpStatus' - The response's http status code.
newEnableSagemakerServicecatalogPortfolioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableSagemakerServicecatalogPortfolioResponse
newEnableSagemakerServicecatalogPortfolioResponse
  pHttpStatus_ =
    EnableSagemakerServicecatalogPortfolioResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
enableSagemakerServicecatalogPortfolioResponse_httpStatus :: Lens.Lens' EnableSagemakerServicecatalogPortfolioResponse Prelude.Int
enableSagemakerServicecatalogPortfolioResponse_httpStatus = Lens.lens (\EnableSagemakerServicecatalogPortfolioResponse' {httpStatus} -> httpStatus) (\s@EnableSagemakerServicecatalogPortfolioResponse' {} a -> s {httpStatus = a} :: EnableSagemakerServicecatalogPortfolioResponse)

instance
  Prelude.NFData
    EnableSagemakerServicecatalogPortfolioResponse
  where
  rnf
    EnableSagemakerServicecatalogPortfolioResponse' {..} =
      Prelude.rnf httpStatus
