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
-- Module      : Amazonka.ComputeOptimizer.GetECSServiceRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns Amazon ECS service recommendations.
--
-- Compute Optimizer generates recommendations for Amazon ECS services on
-- Fargate that meet a specific set of requirements. For more information,
-- see the
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/requirements.html Supported resources and requirements>
-- in the /Compute Optimizer User Guide/.
module Amazonka.ComputeOptimizer.GetECSServiceRecommendations
  ( -- * Creating a Request
    GetECSServiceRecommendations (..),
    newGetECSServiceRecommendations,

    -- * Request Lenses
    getECSServiceRecommendations_accountIds,
    getECSServiceRecommendations_filters,
    getECSServiceRecommendations_maxResults,
    getECSServiceRecommendations_nextToken,
    getECSServiceRecommendations_serviceArns,

    -- * Destructuring the Response
    GetECSServiceRecommendationsResponse (..),
    newGetECSServiceRecommendationsResponse,

    -- * Response Lenses
    getECSServiceRecommendationsResponse_ecsServiceRecommendations,
    getECSServiceRecommendationsResponse_errors,
    getECSServiceRecommendationsResponse_nextToken,
    getECSServiceRecommendationsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetECSServiceRecommendations' smart constructor.
data GetECSServiceRecommendations = GetECSServiceRecommendations'
  { -- | Return the ECS service recommendations to the specified Amazon Web
    -- Services account IDs.
    --
    -- If your account is the management account or the delegated administrator
    -- of an organization, use this parameter to return the ECS service
    -- recommendations to specific member accounts.
    --
    -- You can only specify one account ID per request.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | An array of objects to specify a filter that returns a more specific
    -- list of ECS service recommendations.
    filters :: Prelude.Maybe [ECSServiceRecommendationFilter],
    -- | The maximum number of ECS service recommendations to return with a
    -- single request.
    --
    -- To retrieve the remaining results, make another request with the
    -- returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to advance to the next page of ECS service recommendations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN that identifies the ECS service.
    --
    -- The following is the format of the ARN:
    --
    -- @arn:aws:ecs:region:aws_account_id:service\/cluster-name\/service-name@
    serviceArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetECSServiceRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'getECSServiceRecommendations_accountIds' - Return the ECS service recommendations to the specified Amazon Web
-- Services account IDs.
--
-- If your account is the management account or the delegated administrator
-- of an organization, use this parameter to return the ECS service
-- recommendations to specific member accounts.
--
-- You can only specify one account ID per request.
--
-- 'filters', 'getECSServiceRecommendations_filters' - An array of objects to specify a filter that returns a more specific
-- list of ECS service recommendations.
--
-- 'maxResults', 'getECSServiceRecommendations_maxResults' - The maximum number of ECS service recommendations to return with a
-- single request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
--
-- 'nextToken', 'getECSServiceRecommendations_nextToken' - The token to advance to the next page of ECS service recommendations.
--
-- 'serviceArns', 'getECSServiceRecommendations_serviceArns' - The ARN that identifies the ECS service.
--
-- The following is the format of the ARN:
--
-- @arn:aws:ecs:region:aws_account_id:service\/cluster-name\/service-name@
newGetECSServiceRecommendations ::
  GetECSServiceRecommendations
newGetECSServiceRecommendations =
  GetECSServiceRecommendations'
    { accountIds =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceArns = Prelude.Nothing
    }

-- | Return the ECS service recommendations to the specified Amazon Web
-- Services account IDs.
--
-- If your account is the management account or the delegated administrator
-- of an organization, use this parameter to return the ECS service
-- recommendations to specific member accounts.
--
-- You can only specify one account ID per request.
getECSServiceRecommendations_accountIds :: Lens.Lens' GetECSServiceRecommendations (Prelude.Maybe [Prelude.Text])
getECSServiceRecommendations_accountIds = Lens.lens (\GetECSServiceRecommendations' {accountIds} -> accountIds) (\s@GetECSServiceRecommendations' {} a -> s {accountIds = a} :: GetECSServiceRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects to specify a filter that returns a more specific
-- list of ECS service recommendations.
getECSServiceRecommendations_filters :: Lens.Lens' GetECSServiceRecommendations (Prelude.Maybe [ECSServiceRecommendationFilter])
getECSServiceRecommendations_filters = Lens.lens (\GetECSServiceRecommendations' {filters} -> filters) (\s@GetECSServiceRecommendations' {} a -> s {filters = a} :: GetECSServiceRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of ECS service recommendations to return with a
-- single request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
getECSServiceRecommendations_maxResults :: Lens.Lens' GetECSServiceRecommendations (Prelude.Maybe Prelude.Natural)
getECSServiceRecommendations_maxResults = Lens.lens (\GetECSServiceRecommendations' {maxResults} -> maxResults) (\s@GetECSServiceRecommendations' {} a -> s {maxResults = a} :: GetECSServiceRecommendations)

-- | The token to advance to the next page of ECS service recommendations.
getECSServiceRecommendations_nextToken :: Lens.Lens' GetECSServiceRecommendations (Prelude.Maybe Prelude.Text)
getECSServiceRecommendations_nextToken = Lens.lens (\GetECSServiceRecommendations' {nextToken} -> nextToken) (\s@GetECSServiceRecommendations' {} a -> s {nextToken = a} :: GetECSServiceRecommendations)

-- | The ARN that identifies the ECS service.
--
-- The following is the format of the ARN:
--
-- @arn:aws:ecs:region:aws_account_id:service\/cluster-name\/service-name@
getECSServiceRecommendations_serviceArns :: Lens.Lens' GetECSServiceRecommendations (Prelude.Maybe [Prelude.Text])
getECSServiceRecommendations_serviceArns = Lens.lens (\GetECSServiceRecommendations' {serviceArns} -> serviceArns) (\s@GetECSServiceRecommendations' {} a -> s {serviceArns = a} :: GetECSServiceRecommendations) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest GetECSServiceRecommendations where
  type
    AWSResponse GetECSServiceRecommendations =
      GetECSServiceRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetECSServiceRecommendationsResponse'
            Prelude.<$> ( x
                            Data..?> "ecsServiceRecommendations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetECSServiceRecommendations
  where
  hashWithSalt _salt GetECSServiceRecommendations' {..} =
    _salt
      `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceArns

instance Prelude.NFData GetECSServiceRecommendations where
  rnf GetECSServiceRecommendations' {..} =
    Prelude.rnf accountIds `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf serviceArns

instance Data.ToHeaders GetECSServiceRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.GetECSServiceRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetECSServiceRecommendations where
  toJSON GetECSServiceRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountIds" Data..=) Prelude.<$> accountIds,
            ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("serviceArns" Data..=) Prelude.<$> serviceArns
          ]
      )

instance Data.ToPath GetECSServiceRecommendations where
  toPath = Prelude.const "/"

instance Data.ToQuery GetECSServiceRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetECSServiceRecommendationsResponse' smart constructor.
data GetECSServiceRecommendationsResponse = GetECSServiceRecommendationsResponse'
  { -- | An array of objects that describe the ECS service recommendations.
    ecsServiceRecommendations :: Prelude.Maybe [ECSServiceRecommendation],
    -- | An array of objects that describe errors of the request.
    errors :: Prelude.Maybe [GetRecommendationError],
    -- | The token to advance to the next page of ECS service recommendations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetECSServiceRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ecsServiceRecommendations', 'getECSServiceRecommendationsResponse_ecsServiceRecommendations' - An array of objects that describe the ECS service recommendations.
--
-- 'errors', 'getECSServiceRecommendationsResponse_errors' - An array of objects that describe errors of the request.
--
-- 'nextToken', 'getECSServiceRecommendationsResponse_nextToken' - The token to advance to the next page of ECS service recommendations.
--
-- 'httpStatus', 'getECSServiceRecommendationsResponse_httpStatus' - The response's http status code.
newGetECSServiceRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetECSServiceRecommendationsResponse
newGetECSServiceRecommendationsResponse pHttpStatus_ =
  GetECSServiceRecommendationsResponse'
    { ecsServiceRecommendations =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the ECS service recommendations.
getECSServiceRecommendationsResponse_ecsServiceRecommendations :: Lens.Lens' GetECSServiceRecommendationsResponse (Prelude.Maybe [ECSServiceRecommendation])
getECSServiceRecommendationsResponse_ecsServiceRecommendations = Lens.lens (\GetECSServiceRecommendationsResponse' {ecsServiceRecommendations} -> ecsServiceRecommendations) (\s@GetECSServiceRecommendationsResponse' {} a -> s {ecsServiceRecommendations = a} :: GetECSServiceRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that describe errors of the request.
getECSServiceRecommendationsResponse_errors :: Lens.Lens' GetECSServiceRecommendationsResponse (Prelude.Maybe [GetRecommendationError])
getECSServiceRecommendationsResponse_errors = Lens.lens (\GetECSServiceRecommendationsResponse' {errors} -> errors) (\s@GetECSServiceRecommendationsResponse' {} a -> s {errors = a} :: GetECSServiceRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of ECS service recommendations.
getECSServiceRecommendationsResponse_nextToken :: Lens.Lens' GetECSServiceRecommendationsResponse (Prelude.Maybe Prelude.Text)
getECSServiceRecommendationsResponse_nextToken = Lens.lens (\GetECSServiceRecommendationsResponse' {nextToken} -> nextToken) (\s@GetECSServiceRecommendationsResponse' {} a -> s {nextToken = a} :: GetECSServiceRecommendationsResponse)

-- | The response's http status code.
getECSServiceRecommendationsResponse_httpStatus :: Lens.Lens' GetECSServiceRecommendationsResponse Prelude.Int
getECSServiceRecommendationsResponse_httpStatus = Lens.lens (\GetECSServiceRecommendationsResponse' {httpStatus} -> httpStatus) (\s@GetECSServiceRecommendationsResponse' {} a -> s {httpStatus = a} :: GetECSServiceRecommendationsResponse)

instance
  Prelude.NFData
    GetECSServiceRecommendationsResponse
  where
  rnf GetECSServiceRecommendationsResponse' {..} =
    Prelude.rnf ecsServiceRecommendations `Prelude.seq`
      Prelude.rnf errors `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf httpStatus
