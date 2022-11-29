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
-- Module      : Amazonka.ComputeOptimizer.GetAutoScalingGroupRecommendations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns Auto Scaling group recommendations.
--
-- Compute Optimizer generates recommendations for Amazon EC2 Auto Scaling
-- groups that meet a specific set of requirements. For more information,
-- see the
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/requirements.html Supported resources and requirements>
-- in the /Compute Optimizer User Guide/.
module Amazonka.ComputeOptimizer.GetAutoScalingGroupRecommendations
  ( -- * Creating a Request
    GetAutoScalingGroupRecommendations (..),
    newGetAutoScalingGroupRecommendations,

    -- * Request Lenses
    getAutoScalingGroupRecommendations_accountIds,
    getAutoScalingGroupRecommendations_nextToken,
    getAutoScalingGroupRecommendations_recommendationPreferences,
    getAutoScalingGroupRecommendations_filters,
    getAutoScalingGroupRecommendations_maxResults,
    getAutoScalingGroupRecommendations_autoScalingGroupArns,

    -- * Destructuring the Response
    GetAutoScalingGroupRecommendationsResponse (..),
    newGetAutoScalingGroupRecommendationsResponse,

    -- * Response Lenses
    getAutoScalingGroupRecommendationsResponse_nextToken,
    getAutoScalingGroupRecommendationsResponse_errors,
    getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations,
    getAutoScalingGroupRecommendationsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAutoScalingGroupRecommendations' smart constructor.
data GetAutoScalingGroupRecommendations = GetAutoScalingGroupRecommendations'
  { -- | The ID of the Amazon Web Services account for which to return Auto
    -- Scaling group recommendations.
    --
    -- If your account is the management account of an organization, use this
    -- parameter to specify the member account for which you want to return
    -- Auto Scaling group recommendations.
    --
    -- Only one account ID can be specified per request.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | The token to advance to the next page of Auto Scaling group
    -- recommendations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An object to specify the preferences for the Auto Scaling group
    -- recommendations to return in the response.
    recommendationPreferences :: Prelude.Maybe RecommendationPreferences,
    -- | An array of objects to specify a filter that returns a more specific
    -- list of Auto Scaling group recommendations.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of Auto Scaling group recommendations to return with
    -- a single request.
    --
    -- To retrieve the remaining results, make another request with the
    -- returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Auto Scaling groups for which to
    -- return recommendations.
    autoScalingGroupArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAutoScalingGroupRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'getAutoScalingGroupRecommendations_accountIds' - The ID of the Amazon Web Services account for which to return Auto
-- Scaling group recommendations.
--
-- If your account is the management account of an organization, use this
-- parameter to specify the member account for which you want to return
-- Auto Scaling group recommendations.
--
-- Only one account ID can be specified per request.
--
-- 'nextToken', 'getAutoScalingGroupRecommendations_nextToken' - The token to advance to the next page of Auto Scaling group
-- recommendations.
--
-- 'recommendationPreferences', 'getAutoScalingGroupRecommendations_recommendationPreferences' - An object to specify the preferences for the Auto Scaling group
-- recommendations to return in the response.
--
-- 'filters', 'getAutoScalingGroupRecommendations_filters' - An array of objects to specify a filter that returns a more specific
-- list of Auto Scaling group recommendations.
--
-- 'maxResults', 'getAutoScalingGroupRecommendations_maxResults' - The maximum number of Auto Scaling group recommendations to return with
-- a single request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
--
-- 'autoScalingGroupArns', 'getAutoScalingGroupRecommendations_autoScalingGroupArns' - The Amazon Resource Name (ARN) of the Auto Scaling groups for which to
-- return recommendations.
newGetAutoScalingGroupRecommendations ::
  GetAutoScalingGroupRecommendations
newGetAutoScalingGroupRecommendations =
  GetAutoScalingGroupRecommendations'
    { accountIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      recommendationPreferences =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      autoScalingGroupArns = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services account for which to return Auto
-- Scaling group recommendations.
--
-- If your account is the management account of an organization, use this
-- parameter to specify the member account for which you want to return
-- Auto Scaling group recommendations.
--
-- Only one account ID can be specified per request.
getAutoScalingGroupRecommendations_accountIds :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe [Prelude.Text])
getAutoScalingGroupRecommendations_accountIds = Lens.lens (\GetAutoScalingGroupRecommendations' {accountIds} -> accountIds) (\s@GetAutoScalingGroupRecommendations' {} a -> s {accountIds = a} :: GetAutoScalingGroupRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of Auto Scaling group
-- recommendations.
getAutoScalingGroupRecommendations_nextToken :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe Prelude.Text)
getAutoScalingGroupRecommendations_nextToken = Lens.lens (\GetAutoScalingGroupRecommendations' {nextToken} -> nextToken) (\s@GetAutoScalingGroupRecommendations' {} a -> s {nextToken = a} :: GetAutoScalingGroupRecommendations)

-- | An object to specify the preferences for the Auto Scaling group
-- recommendations to return in the response.
getAutoScalingGroupRecommendations_recommendationPreferences :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe RecommendationPreferences)
getAutoScalingGroupRecommendations_recommendationPreferences = Lens.lens (\GetAutoScalingGroupRecommendations' {recommendationPreferences} -> recommendationPreferences) (\s@GetAutoScalingGroupRecommendations' {} a -> s {recommendationPreferences = a} :: GetAutoScalingGroupRecommendations)

-- | An array of objects to specify a filter that returns a more specific
-- list of Auto Scaling group recommendations.
getAutoScalingGroupRecommendations_filters :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe [Filter])
getAutoScalingGroupRecommendations_filters = Lens.lens (\GetAutoScalingGroupRecommendations' {filters} -> filters) (\s@GetAutoScalingGroupRecommendations' {} a -> s {filters = a} :: GetAutoScalingGroupRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of Auto Scaling group recommendations to return with
-- a single request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
getAutoScalingGroupRecommendations_maxResults :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe Prelude.Int)
getAutoScalingGroupRecommendations_maxResults = Lens.lens (\GetAutoScalingGroupRecommendations' {maxResults} -> maxResults) (\s@GetAutoScalingGroupRecommendations' {} a -> s {maxResults = a} :: GetAutoScalingGroupRecommendations)

-- | The Amazon Resource Name (ARN) of the Auto Scaling groups for which to
-- return recommendations.
getAutoScalingGroupRecommendations_autoScalingGroupArns :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe [Prelude.Text])
getAutoScalingGroupRecommendations_autoScalingGroupArns = Lens.lens (\GetAutoScalingGroupRecommendations' {autoScalingGroupArns} -> autoScalingGroupArns) (\s@GetAutoScalingGroupRecommendations' {} a -> s {autoScalingGroupArns = a} :: GetAutoScalingGroupRecommendations) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    GetAutoScalingGroupRecommendations
  where
  type
    AWSResponse GetAutoScalingGroupRecommendations =
      GetAutoScalingGroupRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAutoScalingGroupRecommendationsResponse'
            Prelude.<$> (x Core..?> "nextToken")
              Prelude.<*> (x Core..?> "errors" Core..!@ Prelude.mempty)
              Prelude.<*> ( x Core..?> "autoScalingGroupRecommendations"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAutoScalingGroupRecommendations
  where
  hashWithSalt
    _salt
    GetAutoScalingGroupRecommendations' {..} =
      _salt `Prelude.hashWithSalt` accountIds
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` recommendationPreferences
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` autoScalingGroupArns

instance
  Prelude.NFData
    GetAutoScalingGroupRecommendations
  where
  rnf GetAutoScalingGroupRecommendations' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendationPreferences
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf autoScalingGroupArns

instance
  Core.ToHeaders
    GetAutoScalingGroupRecommendations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComputeOptimizerService.GetAutoScalingGroupRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    GetAutoScalingGroupRecommendations
  where
  toJSON GetAutoScalingGroupRecommendations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("accountIds" Core..=) Prelude.<$> accountIds,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("recommendationPreferences" Core..=)
              Prelude.<$> recommendationPreferences,
            ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("autoScalingGroupArns" Core..=)
              Prelude.<$> autoScalingGroupArns
          ]
      )

instance
  Core.ToPath
    GetAutoScalingGroupRecommendations
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetAutoScalingGroupRecommendations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAutoScalingGroupRecommendationsResponse' smart constructor.
data GetAutoScalingGroupRecommendationsResponse = GetAutoScalingGroupRecommendationsResponse'
  { -- | The token to use to advance to the next page of Auto Scaling group
    -- recommendations.
    --
    -- This value is null when there are no more pages of Auto Scaling group
    -- recommendations to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe errors of the request.
    --
    -- For example, an error is returned if you request recommendations for an
    -- unsupported Auto Scaling group.
    errors :: Prelude.Maybe [GetRecommendationError],
    -- | An array of objects that describe Auto Scaling group recommendations.
    autoScalingGroupRecommendations :: Prelude.Maybe [AutoScalingGroupRecommendation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAutoScalingGroupRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAutoScalingGroupRecommendationsResponse_nextToken' - The token to use to advance to the next page of Auto Scaling group
-- recommendations.
--
-- This value is null when there are no more pages of Auto Scaling group
-- recommendations to return.
--
-- 'errors', 'getAutoScalingGroupRecommendationsResponse_errors' - An array of objects that describe errors of the request.
--
-- For example, an error is returned if you request recommendations for an
-- unsupported Auto Scaling group.
--
-- 'autoScalingGroupRecommendations', 'getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations' - An array of objects that describe Auto Scaling group recommendations.
--
-- 'httpStatus', 'getAutoScalingGroupRecommendationsResponse_httpStatus' - The response's http status code.
newGetAutoScalingGroupRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAutoScalingGroupRecommendationsResponse
newGetAutoScalingGroupRecommendationsResponse
  pHttpStatus_ =
    GetAutoScalingGroupRecommendationsResponse'
      { nextToken =
          Prelude.Nothing,
        errors = Prelude.Nothing,
        autoScalingGroupRecommendations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to advance to the next page of Auto Scaling group
-- recommendations.
--
-- This value is null when there are no more pages of Auto Scaling group
-- recommendations to return.
getAutoScalingGroupRecommendationsResponse_nextToken :: Lens.Lens' GetAutoScalingGroupRecommendationsResponse (Prelude.Maybe Prelude.Text)
getAutoScalingGroupRecommendationsResponse_nextToken = Lens.lens (\GetAutoScalingGroupRecommendationsResponse' {nextToken} -> nextToken) (\s@GetAutoScalingGroupRecommendationsResponse' {} a -> s {nextToken = a} :: GetAutoScalingGroupRecommendationsResponse)

-- | An array of objects that describe errors of the request.
--
-- For example, an error is returned if you request recommendations for an
-- unsupported Auto Scaling group.
getAutoScalingGroupRecommendationsResponse_errors :: Lens.Lens' GetAutoScalingGroupRecommendationsResponse (Prelude.Maybe [GetRecommendationError])
getAutoScalingGroupRecommendationsResponse_errors = Lens.lens (\GetAutoScalingGroupRecommendationsResponse' {errors} -> errors) (\s@GetAutoScalingGroupRecommendationsResponse' {} a -> s {errors = a} :: GetAutoScalingGroupRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that describe Auto Scaling group recommendations.
getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations :: Lens.Lens' GetAutoScalingGroupRecommendationsResponse (Prelude.Maybe [AutoScalingGroupRecommendation])
getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations = Lens.lens (\GetAutoScalingGroupRecommendationsResponse' {autoScalingGroupRecommendations} -> autoScalingGroupRecommendations) (\s@GetAutoScalingGroupRecommendationsResponse' {} a -> s {autoScalingGroupRecommendations = a} :: GetAutoScalingGroupRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getAutoScalingGroupRecommendationsResponse_httpStatus :: Lens.Lens' GetAutoScalingGroupRecommendationsResponse Prelude.Int
getAutoScalingGroupRecommendationsResponse_httpStatus = Lens.lens (\GetAutoScalingGroupRecommendationsResponse' {httpStatus} -> httpStatus) (\s@GetAutoScalingGroupRecommendationsResponse' {} a -> s {httpStatus = a} :: GetAutoScalingGroupRecommendationsResponse)

instance
  Prelude.NFData
    GetAutoScalingGroupRecommendationsResponse
  where
  rnf GetAutoScalingGroupRecommendationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf autoScalingGroupRecommendations
      `Prelude.seq` Prelude.rnf httpStatus
