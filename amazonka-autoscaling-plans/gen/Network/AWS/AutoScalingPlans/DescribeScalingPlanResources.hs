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
-- Module      : Network.AWS.AutoScalingPlans.DescribeScalingPlanResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scalable resources in the specified scaling plan.
--
-- This operation returns paginated results.
module Network.AWS.AutoScalingPlans.DescribeScalingPlanResources
  ( -- * Creating a Request
    DescribeScalingPlanResources (..),
    newDescribeScalingPlanResources,

    -- * Request Lenses
    describeScalingPlanResources_nextToken,
    describeScalingPlanResources_maxResults,
    describeScalingPlanResources_scalingPlanName,
    describeScalingPlanResources_scalingPlanVersion,

    -- * Destructuring the Response
    DescribeScalingPlanResourcesResponse (..),
    newDescribeScalingPlanResourcesResponse,

    -- * Response Lenses
    describeScalingPlanResourcesResponse_nextToken,
    describeScalingPlanResourcesResponse_scalingPlanResources,
    describeScalingPlanResourcesResponse_httpStatus,
  )
where

import Network.AWS.AutoScalingPlans.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeScalingPlanResources' smart constructor.
data DescribeScalingPlanResources = DescribeScalingPlanResources'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of scalable resources to return. The value must be
    -- between 1 and 50. The default value is 50.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The name of the scaling plan.
    scalingPlanName :: Prelude.Text,
    -- | The version number of the scaling plan. Currently, the only valid value
    -- is @1@.
    scalingPlanVersion :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingPlanResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScalingPlanResources_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'describeScalingPlanResources_maxResults' - The maximum number of scalable resources to return. The value must be
-- between 1 and 50. The default value is 50.
--
-- 'scalingPlanName', 'describeScalingPlanResources_scalingPlanName' - The name of the scaling plan.
--
-- 'scalingPlanVersion', 'describeScalingPlanResources_scalingPlanVersion' - The version number of the scaling plan. Currently, the only valid value
-- is @1@.
newDescribeScalingPlanResources ::
  -- | 'scalingPlanName'
  Prelude.Text ->
  -- | 'scalingPlanVersion'
  Prelude.Integer ->
  DescribeScalingPlanResources
newDescribeScalingPlanResources
  pScalingPlanName_
  pScalingPlanVersion_ =
    DescribeScalingPlanResources'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        scalingPlanName = pScalingPlanName_,
        scalingPlanVersion = pScalingPlanVersion_
      }

-- | The token for the next set of results.
describeScalingPlanResources_nextToken :: Lens.Lens' DescribeScalingPlanResources (Prelude.Maybe Prelude.Text)
describeScalingPlanResources_nextToken = Lens.lens (\DescribeScalingPlanResources' {nextToken} -> nextToken) (\s@DescribeScalingPlanResources' {} a -> s {nextToken = a} :: DescribeScalingPlanResources)

-- | The maximum number of scalable resources to return. The value must be
-- between 1 and 50. The default value is 50.
describeScalingPlanResources_maxResults :: Lens.Lens' DescribeScalingPlanResources (Prelude.Maybe Prelude.Int)
describeScalingPlanResources_maxResults = Lens.lens (\DescribeScalingPlanResources' {maxResults} -> maxResults) (\s@DescribeScalingPlanResources' {} a -> s {maxResults = a} :: DescribeScalingPlanResources)

-- | The name of the scaling plan.
describeScalingPlanResources_scalingPlanName :: Lens.Lens' DescribeScalingPlanResources Prelude.Text
describeScalingPlanResources_scalingPlanName = Lens.lens (\DescribeScalingPlanResources' {scalingPlanName} -> scalingPlanName) (\s@DescribeScalingPlanResources' {} a -> s {scalingPlanName = a} :: DescribeScalingPlanResources)

-- | The version number of the scaling plan. Currently, the only valid value
-- is @1@.
describeScalingPlanResources_scalingPlanVersion :: Lens.Lens' DescribeScalingPlanResources Prelude.Integer
describeScalingPlanResources_scalingPlanVersion = Lens.lens (\DescribeScalingPlanResources' {scalingPlanVersion} -> scalingPlanVersion) (\s@DescribeScalingPlanResources' {} a -> s {scalingPlanVersion = a} :: DescribeScalingPlanResources)

instance Core.AWSPager DescribeScalingPlanResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScalingPlanResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeScalingPlanResourcesResponse_scalingPlanResources
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeScalingPlanResources_nextToken
          Lens..~ rs
          Lens.^? describeScalingPlanResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeScalingPlanResources where
  type
    AWSResponse DescribeScalingPlanResources =
      DescribeScalingPlanResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScalingPlanResourcesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ScalingPlanResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeScalingPlanResources

instance Prelude.NFData DescribeScalingPlanResources

instance Core.ToHeaders DescribeScalingPlanResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AnyScaleScalingPlannerFrontendService.DescribeScalingPlanResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeScalingPlanResources where
  toJSON DescribeScalingPlanResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("ScalingPlanName" Core..= scalingPlanName),
            Prelude.Just
              ("ScalingPlanVersion" Core..= scalingPlanVersion)
          ]
      )

instance Core.ToPath DescribeScalingPlanResources where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeScalingPlanResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeScalingPlanResourcesResponse' smart constructor.
data DescribeScalingPlanResourcesResponse = DescribeScalingPlanResourcesResponse'
  { -- | The token required to get the next set of results. This value is @null@
    -- if there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the scalable resources.
    scalingPlanResources :: Prelude.Maybe [ScalingPlanResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingPlanResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScalingPlanResourcesResponse_nextToken' - The token required to get the next set of results. This value is @null@
-- if there are no more results to return.
--
-- 'scalingPlanResources', 'describeScalingPlanResourcesResponse_scalingPlanResources' - Information about the scalable resources.
--
-- 'httpStatus', 'describeScalingPlanResourcesResponse_httpStatus' - The response's http status code.
newDescribeScalingPlanResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeScalingPlanResourcesResponse
newDescribeScalingPlanResourcesResponse pHttpStatus_ =
  DescribeScalingPlanResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      scalingPlanResources =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to get the next set of results. This value is @null@
-- if there are no more results to return.
describeScalingPlanResourcesResponse_nextToken :: Lens.Lens' DescribeScalingPlanResourcesResponse (Prelude.Maybe Prelude.Text)
describeScalingPlanResourcesResponse_nextToken = Lens.lens (\DescribeScalingPlanResourcesResponse' {nextToken} -> nextToken) (\s@DescribeScalingPlanResourcesResponse' {} a -> s {nextToken = a} :: DescribeScalingPlanResourcesResponse)

-- | Information about the scalable resources.
describeScalingPlanResourcesResponse_scalingPlanResources :: Lens.Lens' DescribeScalingPlanResourcesResponse (Prelude.Maybe [ScalingPlanResource])
describeScalingPlanResourcesResponse_scalingPlanResources = Lens.lens (\DescribeScalingPlanResourcesResponse' {scalingPlanResources} -> scalingPlanResources) (\s@DescribeScalingPlanResourcesResponse' {} a -> s {scalingPlanResources = a} :: DescribeScalingPlanResourcesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeScalingPlanResourcesResponse_httpStatus :: Lens.Lens' DescribeScalingPlanResourcesResponse Prelude.Int
describeScalingPlanResourcesResponse_httpStatus = Lens.lens (\DescribeScalingPlanResourcesResponse' {httpStatus} -> httpStatus) (\s@DescribeScalingPlanResourcesResponse' {} a -> s {httpStatus = a} :: DescribeScalingPlanResourcesResponse)

instance
  Prelude.NFData
    DescribeScalingPlanResourcesResponse
