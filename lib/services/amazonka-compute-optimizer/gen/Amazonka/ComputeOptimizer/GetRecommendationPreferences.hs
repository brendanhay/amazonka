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
-- Module      : Amazonka.ComputeOptimizer.GetRecommendationPreferences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns existing recommendation preferences, such as enhanced
-- infrastructure metrics.
--
-- Use the @scope@ parameter to specify which preferences to return. You
-- can specify to return preferences for an organization, a specific
-- account ID, or a specific EC2 instance or Auto Scaling group Amazon
-- Resource Name (ARN).
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
module Amazonka.ComputeOptimizer.GetRecommendationPreferences
  ( -- * Creating a Request
    GetRecommendationPreferences (..),
    newGetRecommendationPreferences,

    -- * Request Lenses
    getRecommendationPreferences_maxResults,
    getRecommendationPreferences_nextToken,
    getRecommendationPreferences_scope,
    getRecommendationPreferences_resourceType,

    -- * Destructuring the Response
    GetRecommendationPreferencesResponse (..),
    newGetRecommendationPreferencesResponse,

    -- * Response Lenses
    getRecommendationPreferencesResponse_nextToken,
    getRecommendationPreferencesResponse_recommendationPreferencesDetails,
    getRecommendationPreferencesResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRecommendationPreferences' smart constructor.
data GetRecommendationPreferences = GetRecommendationPreferences'
  { -- | The maximum number of recommendation preferences to return with a single
    -- request.
    --
    -- To retrieve the remaining results, make another request with the
    -- returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token to advance to the next page of recommendation preferences.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the scope of the recommendation preference to
    -- return.
    --
    -- You can return recommendation preferences that are created at the
    -- organization level (for management accounts of an organization only),
    -- account level, and resource level. For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
    -- in the /Compute Optimizer User Guide/.
    scope :: Prelude.Maybe Scope,
    -- | The target resource type of the recommendation preference for which to
    -- return preferences.
    --
    -- The @Ec2Instance@ option encompasses standalone instances and instances
    -- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
    -- encompasses only instances that are part of an Auto Scaling group.
    --
    -- The valid values for this parameter are @Ec2Instance@ and
    -- @AutoScalingGroup@.
    resourceType :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommendationPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getRecommendationPreferences_maxResults' - The maximum number of recommendation preferences to return with a single
-- request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
--
-- 'nextToken', 'getRecommendationPreferences_nextToken' - The token to advance to the next page of recommendation preferences.
--
-- 'scope', 'getRecommendationPreferences_scope' - An object that describes the scope of the recommendation preference to
-- return.
--
-- You can return recommendation preferences that are created at the
-- organization level (for management accounts of an organization only),
-- account level, and resource level. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
--
-- 'resourceType', 'getRecommendationPreferences_resourceType' - The target resource type of the recommendation preference for which to
-- return preferences.
--
-- The @Ec2Instance@ option encompasses standalone instances and instances
-- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
-- encompasses only instances that are part of an Auto Scaling group.
--
-- The valid values for this parameter are @Ec2Instance@ and
-- @AutoScalingGroup@.
newGetRecommendationPreferences ::
  -- | 'resourceType'
  ResourceType ->
  GetRecommendationPreferences
newGetRecommendationPreferences pResourceType_ =
  GetRecommendationPreferences'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      scope = Prelude.Nothing,
      resourceType = pResourceType_
    }

-- | The maximum number of recommendation preferences to return with a single
-- request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
getRecommendationPreferences_maxResults :: Lens.Lens' GetRecommendationPreferences (Prelude.Maybe Prelude.Int)
getRecommendationPreferences_maxResults = Lens.lens (\GetRecommendationPreferences' {maxResults} -> maxResults) (\s@GetRecommendationPreferences' {} a -> s {maxResults = a} :: GetRecommendationPreferences)

-- | The token to advance to the next page of recommendation preferences.
getRecommendationPreferences_nextToken :: Lens.Lens' GetRecommendationPreferences (Prelude.Maybe Prelude.Text)
getRecommendationPreferences_nextToken = Lens.lens (\GetRecommendationPreferences' {nextToken} -> nextToken) (\s@GetRecommendationPreferences' {} a -> s {nextToken = a} :: GetRecommendationPreferences)

-- | An object that describes the scope of the recommendation preference to
-- return.
--
-- You can return recommendation preferences that are created at the
-- organization level (for management accounts of an organization only),
-- account level, and resource level. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
getRecommendationPreferences_scope :: Lens.Lens' GetRecommendationPreferences (Prelude.Maybe Scope)
getRecommendationPreferences_scope = Lens.lens (\GetRecommendationPreferences' {scope} -> scope) (\s@GetRecommendationPreferences' {} a -> s {scope = a} :: GetRecommendationPreferences)

-- | The target resource type of the recommendation preference for which to
-- return preferences.
--
-- The @Ec2Instance@ option encompasses standalone instances and instances
-- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
-- encompasses only instances that are part of an Auto Scaling group.
--
-- The valid values for this parameter are @Ec2Instance@ and
-- @AutoScalingGroup@.
getRecommendationPreferences_resourceType :: Lens.Lens' GetRecommendationPreferences ResourceType
getRecommendationPreferences_resourceType = Lens.lens (\GetRecommendationPreferences' {resourceType} -> resourceType) (\s@GetRecommendationPreferences' {} a -> s {resourceType = a} :: GetRecommendationPreferences)

instance Core.AWSRequest GetRecommendationPreferences where
  type
    AWSResponse GetRecommendationPreferences =
      GetRecommendationPreferencesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecommendationPreferencesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "recommendationPreferencesDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRecommendationPreferences
  where
  hashWithSalt _salt GetRecommendationPreferences' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData GetRecommendationPreferences where
  rnf GetRecommendationPreferences' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders GetRecommendationPreferences where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.GetRecommendationPreferences" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRecommendationPreferences where
  toJSON GetRecommendationPreferences' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("scope" Data..=) Prelude.<$> scope,
            Prelude.Just ("resourceType" Data..= resourceType)
          ]
      )

instance Data.ToPath GetRecommendationPreferences where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRecommendationPreferences where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRecommendationPreferencesResponse' smart constructor.
data GetRecommendationPreferencesResponse = GetRecommendationPreferencesResponse'
  { -- | The token to use to advance to the next page of recommendation
    -- preferences.
    --
    -- This value is null when there are no more pages of recommendation
    -- preferences to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe recommendation preferences.
    recommendationPreferencesDetails :: Prelude.Maybe [RecommendationPreferencesDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommendationPreferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getRecommendationPreferencesResponse_nextToken' - The token to use to advance to the next page of recommendation
-- preferences.
--
-- This value is null when there are no more pages of recommendation
-- preferences to return.
--
-- 'recommendationPreferencesDetails', 'getRecommendationPreferencesResponse_recommendationPreferencesDetails' - An array of objects that describe recommendation preferences.
--
-- 'httpStatus', 'getRecommendationPreferencesResponse_httpStatus' - The response's http status code.
newGetRecommendationPreferencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRecommendationPreferencesResponse
newGetRecommendationPreferencesResponse pHttpStatus_ =
  GetRecommendationPreferencesResponse'
    { nextToken =
        Prelude.Nothing,
      recommendationPreferencesDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to advance to the next page of recommendation
-- preferences.
--
-- This value is null when there are no more pages of recommendation
-- preferences to return.
getRecommendationPreferencesResponse_nextToken :: Lens.Lens' GetRecommendationPreferencesResponse (Prelude.Maybe Prelude.Text)
getRecommendationPreferencesResponse_nextToken = Lens.lens (\GetRecommendationPreferencesResponse' {nextToken} -> nextToken) (\s@GetRecommendationPreferencesResponse' {} a -> s {nextToken = a} :: GetRecommendationPreferencesResponse)

-- | An array of objects that describe recommendation preferences.
getRecommendationPreferencesResponse_recommendationPreferencesDetails :: Lens.Lens' GetRecommendationPreferencesResponse (Prelude.Maybe [RecommendationPreferencesDetail])
getRecommendationPreferencesResponse_recommendationPreferencesDetails = Lens.lens (\GetRecommendationPreferencesResponse' {recommendationPreferencesDetails} -> recommendationPreferencesDetails) (\s@GetRecommendationPreferencesResponse' {} a -> s {recommendationPreferencesDetails = a} :: GetRecommendationPreferencesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRecommendationPreferencesResponse_httpStatus :: Lens.Lens' GetRecommendationPreferencesResponse Prelude.Int
getRecommendationPreferencesResponse_httpStatus = Lens.lens (\GetRecommendationPreferencesResponse' {httpStatus} -> httpStatus) (\s@GetRecommendationPreferencesResponse' {} a -> s {httpStatus = a} :: GetRecommendationPreferencesResponse)

instance
  Prelude.NFData
    GetRecommendationPreferencesResponse
  where
  rnf GetRecommendationPreferencesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendationPreferencesDetails
      `Prelude.seq` Prelude.rnf httpStatus
