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
-- Module      : Network.AWS.SageMaker.EnableSagemakerServicecatalogPortfolio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables using Service Catalog in SageMaker. Service Catalog is used to
-- create SageMaker projects.
module Network.AWS.SageMaker.EnableSagemakerServicecatalogPortfolio
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newEnableSagemakerServicecatalogPortfolio' smart constructor.
data EnableSagemakerServicecatalogPortfolio = EnableSagemakerServicecatalogPortfolio'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableSagemakerServicecatalogPortfolioResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    EnableSagemakerServicecatalogPortfolio

instance
  Core.NFData
    EnableSagemakerServicecatalogPortfolio

instance
  Core.ToHeaders
    EnableSagemakerServicecatalogPortfolio
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.EnableSagemakerServicecatalogPortfolio" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    EnableSagemakerServicecatalogPortfolio
  where
  toJSON = Core.const (Core.Object Core.mempty)

instance
  Core.ToPath
    EnableSagemakerServicecatalogPortfolio
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    EnableSagemakerServicecatalogPortfolio
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEnableSagemakerServicecatalogPortfolioResponse' smart constructor.
data EnableSagemakerServicecatalogPortfolioResponse = EnableSagemakerServicecatalogPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  EnableSagemakerServicecatalogPortfolioResponse
newEnableSagemakerServicecatalogPortfolioResponse
  pHttpStatus_ =
    EnableSagemakerServicecatalogPortfolioResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
enableSagemakerServicecatalogPortfolioResponse_httpStatus :: Lens.Lens' EnableSagemakerServicecatalogPortfolioResponse Core.Int
enableSagemakerServicecatalogPortfolioResponse_httpStatus = Lens.lens (\EnableSagemakerServicecatalogPortfolioResponse' {httpStatus} -> httpStatus) (\s@EnableSagemakerServicecatalogPortfolioResponse' {} a -> s {httpStatus = a} :: EnableSagemakerServicecatalogPortfolioResponse)

instance
  Core.NFData
    EnableSagemakerServicecatalogPortfolioResponse
