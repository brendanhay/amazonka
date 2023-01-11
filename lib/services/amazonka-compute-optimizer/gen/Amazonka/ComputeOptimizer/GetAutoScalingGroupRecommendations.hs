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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    getAutoScalingGroupRecommendations_autoScalingGroupArns,
    getAutoScalingGroupRecommendations_filters,
    getAutoScalingGroupRecommendations_maxResults,
    getAutoScalingGroupRecommendations_nextToken,
    getAutoScalingGroupRecommendations_recommendationPreferences,

    -- * Destructuring the Response
    GetAutoScalingGroupRecommendationsResponse (..),
    newGetAutoScalingGroupRecommendationsResponse,

    -- * Response Lenses
    getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations,
    getAutoScalingGroupRecommendationsResponse_errors,
    getAutoScalingGroupRecommendationsResponse_nextToken,
    getAutoScalingGroupRecommendationsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | The Amazon Resource Name (ARN) of the Auto Scaling groups for which to
    -- return recommendations.
    autoScalingGroupArns :: Prelude.Maybe [Prelude.Text],
    -- | An array of objects to specify a filter that returns a more specific
    -- list of Auto Scaling group recommendations.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of Auto Scaling group recommendations to return with
    -- a single request.
    --
    -- To retrieve the remaining results, make another request with the
    -- returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to advance to the next page of Auto Scaling group
    -- recommendations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An object to specify the preferences for the Auto Scaling group
    -- recommendations to return in the response.
    recommendationPreferences :: Prelude.Maybe RecommendationPreferences
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
-- 'autoScalingGroupArns', 'getAutoScalingGroupRecommendations_autoScalingGroupArns' - The Amazon Resource Name (ARN) of the Auto Scaling groups for which to
-- return recommendations.
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
-- 'nextToken', 'getAutoScalingGroupRecommendations_nextToken' - The token to advance to the next page of Auto Scaling group
-- recommendations.
--
-- 'recommendationPreferences', 'getAutoScalingGroupRecommendations_recommendationPreferences' - An object to specify the preferences for the Auto Scaling group
-- recommendations to return in the response.
newGetAutoScalingGroupRecommendations ::
  GetAutoScalingGroupRecommendations
newGetAutoScalingGroupRecommendations =
  GetAutoScalingGroupRecommendations'
    { accountIds =
        Prelude.Nothing,
      autoScalingGroupArns = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      recommendationPreferences =
        Prelude.Nothing
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

-- | The Amazon Resource Name (ARN) of the Auto Scaling groups for which to
-- return recommendations.
getAutoScalingGroupRecommendations_autoScalingGroupArns :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe [Prelude.Text])
getAutoScalingGroupRecommendations_autoScalingGroupArns = Lens.lens (\GetAutoScalingGroupRecommendations' {autoScalingGroupArns} -> autoScalingGroupArns) (\s@GetAutoScalingGroupRecommendations' {} a -> s {autoScalingGroupArns = a} :: GetAutoScalingGroupRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects to specify a filter that returns a more specific
-- list of Auto Scaling group recommendations.
getAutoScalingGroupRecommendations_filters :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe [Filter])
getAutoScalingGroupRecommendations_filters = Lens.lens (\GetAutoScalingGroupRecommendations' {filters} -> filters) (\s@GetAutoScalingGroupRecommendations' {} a -> s {filters = a} :: GetAutoScalingGroupRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of Auto Scaling group recommendations to return with
-- a single request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
getAutoScalingGroupRecommendations_maxResults :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe Prelude.Natural)
getAutoScalingGroupRecommendations_maxResults = Lens.lens (\GetAutoScalingGroupRecommendations' {maxResults} -> maxResults) (\s@GetAutoScalingGroupRecommendations' {} a -> s {maxResults = a} :: GetAutoScalingGroupRecommendations)

-- | The token to advance to the next page of Auto Scaling group
-- recommendations.
getAutoScalingGroupRecommendations_nextToken :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe Prelude.Text)
getAutoScalingGroupRecommendations_nextToken = Lens.lens (\GetAutoScalingGroupRecommendations' {nextToken} -> nextToken) (\s@GetAutoScalingGroupRecommendations' {} a -> s {nextToken = a} :: GetAutoScalingGroupRecommendations)

-- | An object to specify the preferences for the Auto Scaling group
-- recommendations to return in the response.
getAutoScalingGroupRecommendations_recommendationPreferences :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe RecommendationPreferences)
getAutoScalingGroupRecommendations_recommendationPreferences = Lens.lens (\GetAutoScalingGroupRecommendations' {recommendationPreferences} -> recommendationPreferences) (\s@GetAutoScalingGroupRecommendations' {} a -> s {recommendationPreferences = a} :: GetAutoScalingGroupRecommendations)

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
            Prelude.<$> ( x Data..?> "autoScalingGroupRecommendations"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "nextToken")
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
        `Prelude.hashWithSalt` autoScalingGroupArns
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` recommendationPreferences

instance
  Prelude.NFData
    GetAutoScalingGroupRecommendations
  where
  rnf GetAutoScalingGroupRecommendations' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf autoScalingGroupArns
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendationPreferences

instance
  Data.ToHeaders
    GetAutoScalingGroupRecommendations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.GetAutoScalingGroupRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetAutoScalingGroupRecommendations
  where
  toJSON GetAutoScalingGroupRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountIds" Data..=) Prelude.<$> accountIds,
            ("autoScalingGroupArns" Data..=)
              Prelude.<$> autoScalingGroupArns,
            ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("recommendationPreferences" Data..=)
              Prelude.<$> recommendationPreferences
          ]
      )

instance
  Data.ToPath
    GetAutoScalingGroupRecommendations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetAutoScalingGroupRecommendations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAutoScalingGroupRecommendationsResponse' smart constructor.
data GetAutoScalingGroupRecommendationsResponse = GetAutoScalingGroupRecommendationsResponse'
  { -- | An array of objects that describe Auto Scaling group recommendations.
    autoScalingGroupRecommendations :: Prelude.Maybe [AutoScalingGroupRecommendation],
    -- | An array of objects that describe errors of the request.
    --
    -- For example, an error is returned if you request recommendations for an
    -- unsupported Auto Scaling group.
    errors :: Prelude.Maybe [GetRecommendationError],
    -- | The token to use to advance to the next page of Auto Scaling group
    -- recommendations.
    --
    -- This value is null when there are no more pages of Auto Scaling group
    -- recommendations to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'autoScalingGroupRecommendations', 'getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations' - An array of objects that describe Auto Scaling group recommendations.
--
-- 'errors', 'getAutoScalingGroupRecommendationsResponse_errors' - An array of objects that describe errors of the request.
--
-- For example, an error is returned if you request recommendations for an
-- unsupported Auto Scaling group.
--
-- 'nextToken', 'getAutoScalingGroupRecommendationsResponse_nextToken' - The token to use to advance to the next page of Auto Scaling group
-- recommendations.
--
-- This value is null when there are no more pages of Auto Scaling group
-- recommendations to return.
--
-- 'httpStatus', 'getAutoScalingGroupRecommendationsResponse_httpStatus' - The response's http status code.
newGetAutoScalingGroupRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAutoScalingGroupRecommendationsResponse
newGetAutoScalingGroupRecommendationsResponse
  pHttpStatus_ =
    GetAutoScalingGroupRecommendationsResponse'
      { autoScalingGroupRecommendations =
          Prelude.Nothing,
        errors = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe Auto Scaling group recommendations.
getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations :: Lens.Lens' GetAutoScalingGroupRecommendationsResponse (Prelude.Maybe [AutoScalingGroupRecommendation])
getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations = Lens.lens (\GetAutoScalingGroupRecommendationsResponse' {autoScalingGroupRecommendations} -> autoScalingGroupRecommendations) (\s@GetAutoScalingGroupRecommendationsResponse' {} a -> s {autoScalingGroupRecommendations = a} :: GetAutoScalingGroupRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that describe errors of the request.
--
-- For example, an error is returned if you request recommendations for an
-- unsupported Auto Scaling group.
getAutoScalingGroupRecommendationsResponse_errors :: Lens.Lens' GetAutoScalingGroupRecommendationsResponse (Prelude.Maybe [GetRecommendationError])
getAutoScalingGroupRecommendationsResponse_errors = Lens.lens (\GetAutoScalingGroupRecommendationsResponse' {errors} -> errors) (\s@GetAutoScalingGroupRecommendationsResponse' {} a -> s {errors = a} :: GetAutoScalingGroupRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to advance to the next page of Auto Scaling group
-- recommendations.
--
-- This value is null when there are no more pages of Auto Scaling group
-- recommendations to return.
getAutoScalingGroupRecommendationsResponse_nextToken :: Lens.Lens' GetAutoScalingGroupRecommendationsResponse (Prelude.Maybe Prelude.Text)
getAutoScalingGroupRecommendationsResponse_nextToken = Lens.lens (\GetAutoScalingGroupRecommendationsResponse' {nextToken} -> nextToken) (\s@GetAutoScalingGroupRecommendationsResponse' {} a -> s {nextToken = a} :: GetAutoScalingGroupRecommendationsResponse)

-- | The response's http status code.
getAutoScalingGroupRecommendationsResponse_httpStatus :: Lens.Lens' GetAutoScalingGroupRecommendationsResponse Prelude.Int
getAutoScalingGroupRecommendationsResponse_httpStatus = Lens.lens (\GetAutoScalingGroupRecommendationsResponse' {httpStatus} -> httpStatus) (\s@GetAutoScalingGroupRecommendationsResponse' {} a -> s {httpStatus = a} :: GetAutoScalingGroupRecommendationsResponse)

instance
  Prelude.NFData
    GetAutoScalingGroupRecommendationsResponse
  where
  rnf GetAutoScalingGroupRecommendationsResponse' {..} =
    Prelude.rnf autoScalingGroupRecommendations
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
