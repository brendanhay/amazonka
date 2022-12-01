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
-- Module      : Amazonka.ComputeOptimizer.GetEC2InstanceRecommendations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns Amazon EC2 instance recommendations.
--
-- Compute Optimizer generates recommendations for Amazon Elastic Compute
-- Cloud (Amazon EC2) instances that meet a specific set of requirements.
-- For more information, see the
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/requirements.html Supported resources and requirements>
-- in the /Compute Optimizer User Guide/.
module Amazonka.ComputeOptimizer.GetEC2InstanceRecommendations
  ( -- * Creating a Request
    GetEC2InstanceRecommendations (..),
    newGetEC2InstanceRecommendations,

    -- * Request Lenses
    getEC2InstanceRecommendations_accountIds,
    getEC2InstanceRecommendations_nextToken,
    getEC2InstanceRecommendations_recommendationPreferences,
    getEC2InstanceRecommendations_filters,
    getEC2InstanceRecommendations_maxResults,
    getEC2InstanceRecommendations_instanceArns,

    -- * Destructuring the Response
    GetEC2InstanceRecommendationsResponse (..),
    newGetEC2InstanceRecommendationsResponse,

    -- * Response Lenses
    getEC2InstanceRecommendationsResponse_instanceRecommendations,
    getEC2InstanceRecommendationsResponse_nextToken,
    getEC2InstanceRecommendationsResponse_errors,
    getEC2InstanceRecommendationsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEC2InstanceRecommendations' smart constructor.
data GetEC2InstanceRecommendations = GetEC2InstanceRecommendations'
  { -- | The ID of the Amazon Web Services account for which to return instance
    -- recommendations.
    --
    -- If your account is the management account of an organization, use this
    -- parameter to specify the member account for which you want to return
    -- instance recommendations.
    --
    -- Only one account ID can be specified per request.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | The token to advance to the next page of instance recommendations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An object to specify the preferences for the Amazon EC2 instance
    -- recommendations to return in the response.
    recommendationPreferences :: Prelude.Maybe RecommendationPreferences,
    -- | An array of objects to specify a filter that returns a more specific
    -- list of instance recommendations.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of instance recommendations to return with a single
    -- request.
    --
    -- To retrieve the remaining results, make another request with the
    -- returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the instances for which to return
    -- recommendations.
    instanceArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEC2InstanceRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'getEC2InstanceRecommendations_accountIds' - The ID of the Amazon Web Services account for which to return instance
-- recommendations.
--
-- If your account is the management account of an organization, use this
-- parameter to specify the member account for which you want to return
-- instance recommendations.
--
-- Only one account ID can be specified per request.
--
-- 'nextToken', 'getEC2InstanceRecommendations_nextToken' - The token to advance to the next page of instance recommendations.
--
-- 'recommendationPreferences', 'getEC2InstanceRecommendations_recommendationPreferences' - An object to specify the preferences for the Amazon EC2 instance
-- recommendations to return in the response.
--
-- 'filters', 'getEC2InstanceRecommendations_filters' - An array of objects to specify a filter that returns a more specific
-- list of instance recommendations.
--
-- 'maxResults', 'getEC2InstanceRecommendations_maxResults' - The maximum number of instance recommendations to return with a single
-- request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
--
-- 'instanceArns', 'getEC2InstanceRecommendations_instanceArns' - The Amazon Resource Name (ARN) of the instances for which to return
-- recommendations.
newGetEC2InstanceRecommendations ::
  GetEC2InstanceRecommendations
newGetEC2InstanceRecommendations =
  GetEC2InstanceRecommendations'
    { accountIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      recommendationPreferences = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceArns = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services account for which to return instance
-- recommendations.
--
-- If your account is the management account of an organization, use this
-- parameter to specify the member account for which you want to return
-- instance recommendations.
--
-- Only one account ID can be specified per request.
getEC2InstanceRecommendations_accountIds :: Lens.Lens' GetEC2InstanceRecommendations (Prelude.Maybe [Prelude.Text])
getEC2InstanceRecommendations_accountIds = Lens.lens (\GetEC2InstanceRecommendations' {accountIds} -> accountIds) (\s@GetEC2InstanceRecommendations' {} a -> s {accountIds = a} :: GetEC2InstanceRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of instance recommendations.
getEC2InstanceRecommendations_nextToken :: Lens.Lens' GetEC2InstanceRecommendations (Prelude.Maybe Prelude.Text)
getEC2InstanceRecommendations_nextToken = Lens.lens (\GetEC2InstanceRecommendations' {nextToken} -> nextToken) (\s@GetEC2InstanceRecommendations' {} a -> s {nextToken = a} :: GetEC2InstanceRecommendations)

-- | An object to specify the preferences for the Amazon EC2 instance
-- recommendations to return in the response.
getEC2InstanceRecommendations_recommendationPreferences :: Lens.Lens' GetEC2InstanceRecommendations (Prelude.Maybe RecommendationPreferences)
getEC2InstanceRecommendations_recommendationPreferences = Lens.lens (\GetEC2InstanceRecommendations' {recommendationPreferences} -> recommendationPreferences) (\s@GetEC2InstanceRecommendations' {} a -> s {recommendationPreferences = a} :: GetEC2InstanceRecommendations)

-- | An array of objects to specify a filter that returns a more specific
-- list of instance recommendations.
getEC2InstanceRecommendations_filters :: Lens.Lens' GetEC2InstanceRecommendations (Prelude.Maybe [Filter])
getEC2InstanceRecommendations_filters = Lens.lens (\GetEC2InstanceRecommendations' {filters} -> filters) (\s@GetEC2InstanceRecommendations' {} a -> s {filters = a} :: GetEC2InstanceRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of instance recommendations to return with a single
-- request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
getEC2InstanceRecommendations_maxResults :: Lens.Lens' GetEC2InstanceRecommendations (Prelude.Maybe Prelude.Int)
getEC2InstanceRecommendations_maxResults = Lens.lens (\GetEC2InstanceRecommendations' {maxResults} -> maxResults) (\s@GetEC2InstanceRecommendations' {} a -> s {maxResults = a} :: GetEC2InstanceRecommendations)

-- | The Amazon Resource Name (ARN) of the instances for which to return
-- recommendations.
getEC2InstanceRecommendations_instanceArns :: Lens.Lens' GetEC2InstanceRecommendations (Prelude.Maybe [Prelude.Text])
getEC2InstanceRecommendations_instanceArns = Lens.lens (\GetEC2InstanceRecommendations' {instanceArns} -> instanceArns) (\s@GetEC2InstanceRecommendations' {} a -> s {instanceArns = a} :: GetEC2InstanceRecommendations) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    GetEC2InstanceRecommendations
  where
  type
    AWSResponse GetEC2InstanceRecommendations =
      GetEC2InstanceRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEC2InstanceRecommendationsResponse'
            Prelude.<$> ( x Core..?> "instanceRecommendations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetEC2InstanceRecommendations
  where
  hashWithSalt _salt GetEC2InstanceRecommendations' {..} =
    _salt `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` recommendationPreferences
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceArns

instance Prelude.NFData GetEC2InstanceRecommendations where
  rnf GetEC2InstanceRecommendations' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendationPreferences
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceArns

instance Core.ToHeaders GetEC2InstanceRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComputeOptimizerService.GetEC2InstanceRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetEC2InstanceRecommendations where
  toJSON GetEC2InstanceRecommendations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("accountIds" Core..=) Prelude.<$> accountIds,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("recommendationPreferences" Core..=)
              Prelude.<$> recommendationPreferences,
            ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("instanceArns" Core..=) Prelude.<$> instanceArns
          ]
      )

instance Core.ToPath GetEC2InstanceRecommendations where
  toPath = Prelude.const "/"

instance Core.ToQuery GetEC2InstanceRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEC2InstanceRecommendationsResponse' smart constructor.
data GetEC2InstanceRecommendationsResponse = GetEC2InstanceRecommendationsResponse'
  { -- | An array of objects that describe instance recommendations.
    instanceRecommendations :: Prelude.Maybe [InstanceRecommendation],
    -- | The token to use to advance to the next page of instance
    -- recommendations.
    --
    -- This value is null when there are no more pages of instance
    -- recommendations to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe errors of the request.
    --
    -- For example, an error is returned if you request recommendations for an
    -- instance of an unsupported instance family.
    errors :: Prelude.Maybe [GetRecommendationError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEC2InstanceRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceRecommendations', 'getEC2InstanceRecommendationsResponse_instanceRecommendations' - An array of objects that describe instance recommendations.
--
-- 'nextToken', 'getEC2InstanceRecommendationsResponse_nextToken' - The token to use to advance to the next page of instance
-- recommendations.
--
-- This value is null when there are no more pages of instance
-- recommendations to return.
--
-- 'errors', 'getEC2InstanceRecommendationsResponse_errors' - An array of objects that describe errors of the request.
--
-- For example, an error is returned if you request recommendations for an
-- instance of an unsupported instance family.
--
-- 'httpStatus', 'getEC2InstanceRecommendationsResponse_httpStatus' - The response's http status code.
newGetEC2InstanceRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEC2InstanceRecommendationsResponse
newGetEC2InstanceRecommendationsResponse pHttpStatus_ =
  GetEC2InstanceRecommendationsResponse'
    { instanceRecommendations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe instance recommendations.
getEC2InstanceRecommendationsResponse_instanceRecommendations :: Lens.Lens' GetEC2InstanceRecommendationsResponse (Prelude.Maybe [InstanceRecommendation])
getEC2InstanceRecommendationsResponse_instanceRecommendations = Lens.lens (\GetEC2InstanceRecommendationsResponse' {instanceRecommendations} -> instanceRecommendations) (\s@GetEC2InstanceRecommendationsResponse' {} a -> s {instanceRecommendations = a} :: GetEC2InstanceRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to advance to the next page of instance
-- recommendations.
--
-- This value is null when there are no more pages of instance
-- recommendations to return.
getEC2InstanceRecommendationsResponse_nextToken :: Lens.Lens' GetEC2InstanceRecommendationsResponse (Prelude.Maybe Prelude.Text)
getEC2InstanceRecommendationsResponse_nextToken = Lens.lens (\GetEC2InstanceRecommendationsResponse' {nextToken} -> nextToken) (\s@GetEC2InstanceRecommendationsResponse' {} a -> s {nextToken = a} :: GetEC2InstanceRecommendationsResponse)

-- | An array of objects that describe errors of the request.
--
-- For example, an error is returned if you request recommendations for an
-- instance of an unsupported instance family.
getEC2InstanceRecommendationsResponse_errors :: Lens.Lens' GetEC2InstanceRecommendationsResponse (Prelude.Maybe [GetRecommendationError])
getEC2InstanceRecommendationsResponse_errors = Lens.lens (\GetEC2InstanceRecommendationsResponse' {errors} -> errors) (\s@GetEC2InstanceRecommendationsResponse' {} a -> s {errors = a} :: GetEC2InstanceRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEC2InstanceRecommendationsResponse_httpStatus :: Lens.Lens' GetEC2InstanceRecommendationsResponse Prelude.Int
getEC2InstanceRecommendationsResponse_httpStatus = Lens.lens (\GetEC2InstanceRecommendationsResponse' {httpStatus} -> httpStatus) (\s@GetEC2InstanceRecommendationsResponse' {} a -> s {httpStatus = a} :: GetEC2InstanceRecommendationsResponse)

instance
  Prelude.NFData
    GetEC2InstanceRecommendationsResponse
  where
  rnf GetEC2InstanceRecommendationsResponse' {..} =
    Prelude.rnf instanceRecommendations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
