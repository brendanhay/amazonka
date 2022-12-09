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
-- Module      : Amazonka.ComputeOptimizer.GetLambdaFunctionRecommendations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns Lambda function recommendations.
--
-- Compute Optimizer generates recommendations for functions that meet a
-- specific set of requirements. For more information, see the
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/requirements.html Supported resources and requirements>
-- in the /Compute Optimizer User Guide/.
module Amazonka.ComputeOptimizer.GetLambdaFunctionRecommendations
  ( -- * Creating a Request
    GetLambdaFunctionRecommendations (..),
    newGetLambdaFunctionRecommendations,

    -- * Request Lenses
    getLambdaFunctionRecommendations_accountIds,
    getLambdaFunctionRecommendations_filters,
    getLambdaFunctionRecommendations_functionArns,
    getLambdaFunctionRecommendations_maxResults,
    getLambdaFunctionRecommendations_nextToken,

    -- * Destructuring the Response
    GetLambdaFunctionRecommendationsResponse (..),
    newGetLambdaFunctionRecommendationsResponse,

    -- * Response Lenses
    getLambdaFunctionRecommendationsResponse_lambdaFunctionRecommendations,
    getLambdaFunctionRecommendationsResponse_nextToken,
    getLambdaFunctionRecommendationsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLambdaFunctionRecommendations' smart constructor.
data GetLambdaFunctionRecommendations = GetLambdaFunctionRecommendations'
  { -- | The ID of the Amazon Web Services account for which to return function
    -- recommendations.
    --
    -- If your account is the management account of an organization, use this
    -- parameter to specify the member account for which you want to return
    -- function recommendations.
    --
    -- Only one account ID can be specified per request.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | An array of objects to specify a filter that returns a more specific
    -- list of function recommendations.
    filters :: Prelude.Maybe [LambdaFunctionRecommendationFilter],
    -- | The Amazon Resource Name (ARN) of the functions for which to return
    -- recommendations.
    --
    -- You can specify a qualified or unqualified ARN. If you specify an
    -- unqualified ARN without a function version suffix, Compute Optimizer
    -- will return recommendations for the latest (@$LATEST@) version of the
    -- function. If you specify a qualified ARN with a version suffix, Compute
    -- Optimizer will return recommendations for the specified function
    -- version. For more information about using function versions, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html#versioning-versions-using Using versions>
    -- in the /Lambda Developer Guide/.
    functionArns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of function recommendations to return with a single
    -- request.
    --
    -- To retrieve the remaining results, make another request with the
    -- returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token to advance to the next page of function recommendations.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLambdaFunctionRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'getLambdaFunctionRecommendations_accountIds' - The ID of the Amazon Web Services account for which to return function
-- recommendations.
--
-- If your account is the management account of an organization, use this
-- parameter to specify the member account for which you want to return
-- function recommendations.
--
-- Only one account ID can be specified per request.
--
-- 'filters', 'getLambdaFunctionRecommendations_filters' - An array of objects to specify a filter that returns a more specific
-- list of function recommendations.
--
-- 'functionArns', 'getLambdaFunctionRecommendations_functionArns' - The Amazon Resource Name (ARN) of the functions for which to return
-- recommendations.
--
-- You can specify a qualified or unqualified ARN. If you specify an
-- unqualified ARN without a function version suffix, Compute Optimizer
-- will return recommendations for the latest (@$LATEST@) version of the
-- function. If you specify a qualified ARN with a version suffix, Compute
-- Optimizer will return recommendations for the specified function
-- version. For more information about using function versions, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html#versioning-versions-using Using versions>
-- in the /Lambda Developer Guide/.
--
-- 'maxResults', 'getLambdaFunctionRecommendations_maxResults' - The maximum number of function recommendations to return with a single
-- request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
--
-- 'nextToken', 'getLambdaFunctionRecommendations_nextToken' - The token to advance to the next page of function recommendations.
newGetLambdaFunctionRecommendations ::
  GetLambdaFunctionRecommendations
newGetLambdaFunctionRecommendations =
  GetLambdaFunctionRecommendations'
    { accountIds =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      functionArns = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services account for which to return function
-- recommendations.
--
-- If your account is the management account of an organization, use this
-- parameter to specify the member account for which you want to return
-- function recommendations.
--
-- Only one account ID can be specified per request.
getLambdaFunctionRecommendations_accountIds :: Lens.Lens' GetLambdaFunctionRecommendations (Prelude.Maybe [Prelude.Text])
getLambdaFunctionRecommendations_accountIds = Lens.lens (\GetLambdaFunctionRecommendations' {accountIds} -> accountIds) (\s@GetLambdaFunctionRecommendations' {} a -> s {accountIds = a} :: GetLambdaFunctionRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects to specify a filter that returns a more specific
-- list of function recommendations.
getLambdaFunctionRecommendations_filters :: Lens.Lens' GetLambdaFunctionRecommendations (Prelude.Maybe [LambdaFunctionRecommendationFilter])
getLambdaFunctionRecommendations_filters = Lens.lens (\GetLambdaFunctionRecommendations' {filters} -> filters) (\s@GetLambdaFunctionRecommendations' {} a -> s {filters = a} :: GetLambdaFunctionRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the functions for which to return
-- recommendations.
--
-- You can specify a qualified or unqualified ARN. If you specify an
-- unqualified ARN without a function version suffix, Compute Optimizer
-- will return recommendations for the latest (@$LATEST@) version of the
-- function. If you specify a qualified ARN with a version suffix, Compute
-- Optimizer will return recommendations for the specified function
-- version. For more information about using function versions, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html#versioning-versions-using Using versions>
-- in the /Lambda Developer Guide/.
getLambdaFunctionRecommendations_functionArns :: Lens.Lens' GetLambdaFunctionRecommendations (Prelude.Maybe [Prelude.Text])
getLambdaFunctionRecommendations_functionArns = Lens.lens (\GetLambdaFunctionRecommendations' {functionArns} -> functionArns) (\s@GetLambdaFunctionRecommendations' {} a -> s {functionArns = a} :: GetLambdaFunctionRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of function recommendations to return with a single
-- request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
getLambdaFunctionRecommendations_maxResults :: Lens.Lens' GetLambdaFunctionRecommendations (Prelude.Maybe Prelude.Int)
getLambdaFunctionRecommendations_maxResults = Lens.lens (\GetLambdaFunctionRecommendations' {maxResults} -> maxResults) (\s@GetLambdaFunctionRecommendations' {} a -> s {maxResults = a} :: GetLambdaFunctionRecommendations)

-- | The token to advance to the next page of function recommendations.
getLambdaFunctionRecommendations_nextToken :: Lens.Lens' GetLambdaFunctionRecommendations (Prelude.Maybe Prelude.Text)
getLambdaFunctionRecommendations_nextToken = Lens.lens (\GetLambdaFunctionRecommendations' {nextToken} -> nextToken) (\s@GetLambdaFunctionRecommendations' {} a -> s {nextToken = a} :: GetLambdaFunctionRecommendations)

instance
  Core.AWSRequest
    GetLambdaFunctionRecommendations
  where
  type
    AWSResponse GetLambdaFunctionRecommendations =
      GetLambdaFunctionRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLambdaFunctionRecommendationsResponse'
            Prelude.<$> ( x Data..?> "lambdaFunctionRecommendations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetLambdaFunctionRecommendations
  where
  hashWithSalt
    _salt
    GetLambdaFunctionRecommendations' {..} =
      _salt `Prelude.hashWithSalt` accountIds
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` functionArns
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    GetLambdaFunctionRecommendations
  where
  rnf GetLambdaFunctionRecommendations' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf functionArns
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    GetLambdaFunctionRecommendations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.GetLambdaFunctionRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLambdaFunctionRecommendations where
  toJSON GetLambdaFunctionRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountIds" Data..=) Prelude.<$> accountIds,
            ("filters" Data..=) Prelude.<$> filters,
            ("functionArns" Data..=) Prelude.<$> functionArns,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetLambdaFunctionRecommendations where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetLambdaFunctionRecommendations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLambdaFunctionRecommendationsResponse' smart constructor.
data GetLambdaFunctionRecommendationsResponse = GetLambdaFunctionRecommendationsResponse'
  { -- | An array of objects that describe function recommendations.
    lambdaFunctionRecommendations :: Prelude.Maybe [LambdaFunctionRecommendation],
    -- | The token to use to advance to the next page of function
    -- recommendations.
    --
    -- This value is null when there are no more pages of function
    -- recommendations to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLambdaFunctionRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaFunctionRecommendations', 'getLambdaFunctionRecommendationsResponse_lambdaFunctionRecommendations' - An array of objects that describe function recommendations.
--
-- 'nextToken', 'getLambdaFunctionRecommendationsResponse_nextToken' - The token to use to advance to the next page of function
-- recommendations.
--
-- This value is null when there are no more pages of function
-- recommendations to return.
--
-- 'httpStatus', 'getLambdaFunctionRecommendationsResponse_httpStatus' - The response's http status code.
newGetLambdaFunctionRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLambdaFunctionRecommendationsResponse
newGetLambdaFunctionRecommendationsResponse
  pHttpStatus_ =
    GetLambdaFunctionRecommendationsResponse'
      { lambdaFunctionRecommendations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe function recommendations.
getLambdaFunctionRecommendationsResponse_lambdaFunctionRecommendations :: Lens.Lens' GetLambdaFunctionRecommendationsResponse (Prelude.Maybe [LambdaFunctionRecommendation])
getLambdaFunctionRecommendationsResponse_lambdaFunctionRecommendations = Lens.lens (\GetLambdaFunctionRecommendationsResponse' {lambdaFunctionRecommendations} -> lambdaFunctionRecommendations) (\s@GetLambdaFunctionRecommendationsResponse' {} a -> s {lambdaFunctionRecommendations = a} :: GetLambdaFunctionRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to advance to the next page of function
-- recommendations.
--
-- This value is null when there are no more pages of function
-- recommendations to return.
getLambdaFunctionRecommendationsResponse_nextToken :: Lens.Lens' GetLambdaFunctionRecommendationsResponse (Prelude.Maybe Prelude.Text)
getLambdaFunctionRecommendationsResponse_nextToken = Lens.lens (\GetLambdaFunctionRecommendationsResponse' {nextToken} -> nextToken) (\s@GetLambdaFunctionRecommendationsResponse' {} a -> s {nextToken = a} :: GetLambdaFunctionRecommendationsResponse)

-- | The response's http status code.
getLambdaFunctionRecommendationsResponse_httpStatus :: Lens.Lens' GetLambdaFunctionRecommendationsResponse Prelude.Int
getLambdaFunctionRecommendationsResponse_httpStatus = Lens.lens (\GetLambdaFunctionRecommendationsResponse' {httpStatus} -> httpStatus) (\s@GetLambdaFunctionRecommendationsResponse' {} a -> s {httpStatus = a} :: GetLambdaFunctionRecommendationsResponse)

instance
  Prelude.NFData
    GetLambdaFunctionRecommendationsResponse
  where
  rnf GetLambdaFunctionRecommendationsResponse' {..} =
    Prelude.rnf lambdaFunctionRecommendations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
