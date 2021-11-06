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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    getAutoScalingGroupRecommendations_filters,
    getAutoScalingGroupRecommendations_autoScalingGroupArns,
    getAutoScalingGroupRecommendations_recommendationPreferences,
    getAutoScalingGroupRecommendations_nextToken,
    getAutoScalingGroupRecommendations_maxResults,

    -- * Destructuring the Response
    GetAutoScalingGroupRecommendationsResponse (..),
    newGetAutoScalingGroupRecommendationsResponse,

    -- * Response Lenses
    getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations,
    getAutoScalingGroupRecommendationsResponse_nextToken,
    getAutoScalingGroupRecommendationsResponse_errors,
    getAutoScalingGroupRecommendationsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
    -- | An array of objects to specify a filter that returns a more specific
    -- list of Auto Scaling group recommendations.
    filters :: Prelude.Maybe [Filter],
    -- | The Amazon Resource Name (ARN) of the Auto Scaling groups for which to
    -- return recommendations.
    autoScalingGroupArns :: Prelude.Maybe [Prelude.Text],
    -- | An object to specify the preferences for the Auto Scaling group
    -- recommendations to return in the response.
    recommendationPreferences :: Prelude.Maybe RecommendationPreferences,
    -- | The token to advance to the next page of Auto Scaling group
    -- recommendations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of Auto Scaling group recommendations to return with
    -- a single request.
    --
    -- To retrieve the remaining results, make another request with the
    -- returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'filters', 'getAutoScalingGroupRecommendations_filters' - An array of objects to specify a filter that returns a more specific
-- list of Auto Scaling group recommendations.
--
-- 'autoScalingGroupArns', 'getAutoScalingGroupRecommendations_autoScalingGroupArns' - The Amazon Resource Name (ARN) of the Auto Scaling groups for which to
-- return recommendations.
--
-- 'recommendationPreferences', 'getAutoScalingGroupRecommendations_recommendationPreferences' - An object to specify the preferences for the Auto Scaling group
-- recommendations to return in the response.
--
-- 'nextToken', 'getAutoScalingGroupRecommendations_nextToken' - The token to advance to the next page of Auto Scaling group
-- recommendations.
--
-- 'maxResults', 'getAutoScalingGroupRecommendations_maxResults' - The maximum number of Auto Scaling group recommendations to return with
-- a single request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
newGetAutoScalingGroupRecommendations ::
  GetAutoScalingGroupRecommendations
newGetAutoScalingGroupRecommendations =
  GetAutoScalingGroupRecommendations'
    { accountIds =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      autoScalingGroupArns = Prelude.Nothing,
      recommendationPreferences =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
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

-- | An array of objects to specify a filter that returns a more specific
-- list of Auto Scaling group recommendations.
getAutoScalingGroupRecommendations_filters :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe [Filter])
getAutoScalingGroupRecommendations_filters = Lens.lens (\GetAutoScalingGroupRecommendations' {filters} -> filters) (\s@GetAutoScalingGroupRecommendations' {} a -> s {filters = a} :: GetAutoScalingGroupRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Auto Scaling groups for which to
-- return recommendations.
getAutoScalingGroupRecommendations_autoScalingGroupArns :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe [Prelude.Text])
getAutoScalingGroupRecommendations_autoScalingGroupArns = Lens.lens (\GetAutoScalingGroupRecommendations' {autoScalingGroupArns} -> autoScalingGroupArns) (\s@GetAutoScalingGroupRecommendations' {} a -> s {autoScalingGroupArns = a} :: GetAutoScalingGroupRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | An object to specify the preferences for the Auto Scaling group
-- recommendations to return in the response.
getAutoScalingGroupRecommendations_recommendationPreferences :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe RecommendationPreferences)
getAutoScalingGroupRecommendations_recommendationPreferences = Lens.lens (\GetAutoScalingGroupRecommendations' {recommendationPreferences} -> recommendationPreferences) (\s@GetAutoScalingGroupRecommendations' {} a -> s {recommendationPreferences = a} :: GetAutoScalingGroupRecommendations)

-- | The token to advance to the next page of Auto Scaling group
-- recommendations.
getAutoScalingGroupRecommendations_nextToken :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe Prelude.Text)
getAutoScalingGroupRecommendations_nextToken = Lens.lens (\GetAutoScalingGroupRecommendations' {nextToken} -> nextToken) (\s@GetAutoScalingGroupRecommendations' {} a -> s {nextToken = a} :: GetAutoScalingGroupRecommendations)

-- | The maximum number of Auto Scaling group recommendations to return with
-- a single request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
getAutoScalingGroupRecommendations_maxResults :: Lens.Lens' GetAutoScalingGroupRecommendations (Prelude.Maybe Prelude.Int)
getAutoScalingGroupRecommendations_maxResults = Lens.lens (\GetAutoScalingGroupRecommendations' {maxResults} -> maxResults) (\s@GetAutoScalingGroupRecommendations' {} a -> s {maxResults = a} :: GetAutoScalingGroupRecommendations)

instance
  Core.AWSRequest
    GetAutoScalingGroupRecommendations
  where
  type
    AWSResponse GetAutoScalingGroupRecommendations =
      GetAutoScalingGroupRecommendationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAutoScalingGroupRecommendationsResponse'
            Prelude.<$> ( x Core..?> "autoScalingGroupRecommendations"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "nextToken")
              Prelude.<*> (x Core..?> "errors" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAutoScalingGroupRecommendations

instance
  Prelude.NFData
    GetAutoScalingGroupRecommendations

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
            ("filters" Core..=) Prelude.<$> filters,
            ("autoScalingGroupArns" Core..=)
              Prelude.<$> autoScalingGroupArns,
            ("recommendationPreferences" Core..=)
              Prelude.<$> recommendationPreferences,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
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
  { -- | An array of objects that describe Auto Scaling group recommendations.
    autoScalingGroupRecommendations :: Prelude.Maybe [AutoScalingGroupRecommendation],
    -- | The token to use to advance to the next page of Auto Scaling group
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
        nextToken = Prelude.Nothing,
        errors = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe Auto Scaling group recommendations.
getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations :: Lens.Lens' GetAutoScalingGroupRecommendationsResponse (Prelude.Maybe [AutoScalingGroupRecommendation])
getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations = Lens.lens (\GetAutoScalingGroupRecommendationsResponse' {autoScalingGroupRecommendations} -> autoScalingGroupRecommendations) (\s@GetAutoScalingGroupRecommendationsResponse' {} a -> s {autoScalingGroupRecommendations = a} :: GetAutoScalingGroupRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

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

-- | The response's http status code.
getAutoScalingGroupRecommendationsResponse_httpStatus :: Lens.Lens' GetAutoScalingGroupRecommendationsResponse Prelude.Int
getAutoScalingGroupRecommendationsResponse_httpStatus = Lens.lens (\GetAutoScalingGroupRecommendationsResponse' {httpStatus} -> httpStatus) (\s@GetAutoScalingGroupRecommendationsResponse' {} a -> s {httpStatus = a} :: GetAutoScalingGroupRecommendationsResponse)

instance
  Prelude.NFData
    GetAutoScalingGroupRecommendationsResponse
