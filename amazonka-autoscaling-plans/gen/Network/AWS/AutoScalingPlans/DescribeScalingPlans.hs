{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AutoScalingPlans.DescribeScalingPlans
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your scaling plans.
--
-- This operation returns paginated results.
module Network.AWS.AutoScalingPlans.DescribeScalingPlans
  ( -- * Creating a Request
    DescribeScalingPlans (..),
    newDescribeScalingPlans,

    -- * Request Lenses
    describeScalingPlans_nextToken,
    describeScalingPlans_scalingPlanVersion,
    describeScalingPlans_maxResults,
    describeScalingPlans_scalingPlanNames,
    describeScalingPlans_applicationSources,

    -- * Destructuring the Response
    DescribeScalingPlansResponse (..),
    newDescribeScalingPlansResponse,

    -- * Response Lenses
    describeScalingPlansResponse_nextToken,
    describeScalingPlansResponse_scalingPlans,
    describeScalingPlansResponse_httpStatus,
  )
where

import Network.AWS.AutoScalingPlans.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeScalingPlans' smart constructor.
data DescribeScalingPlans = DescribeScalingPlans'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The version number of the scaling plan. Currently, the only valid value
    -- is @1@.
    --
    -- If you specify a scaling plan version, you must also specify a scaling
    -- plan name.
    scalingPlanVersion :: Prelude.Maybe Prelude.Integer,
    -- | The maximum number of scalable resources to return. This value can be
    -- between 1 and 50. The default value is 50.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The names of the scaling plans (up to 10). If you specify application
    -- sources, you cannot specify scaling plan names.
    scalingPlanNames :: Prelude.Maybe [Prelude.Text],
    -- | The sources for the applications (up to 10). If you specify scaling plan
    -- names, you cannot specify application sources.
    applicationSources :: Prelude.Maybe [ApplicationSource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingPlans' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScalingPlans_nextToken' - The token for the next set of results.
--
-- 'scalingPlanVersion', 'describeScalingPlans_scalingPlanVersion' - The version number of the scaling plan. Currently, the only valid value
-- is @1@.
--
-- If you specify a scaling plan version, you must also specify a scaling
-- plan name.
--
-- 'maxResults', 'describeScalingPlans_maxResults' - The maximum number of scalable resources to return. This value can be
-- between 1 and 50. The default value is 50.
--
-- 'scalingPlanNames', 'describeScalingPlans_scalingPlanNames' - The names of the scaling plans (up to 10). If you specify application
-- sources, you cannot specify scaling plan names.
--
-- 'applicationSources', 'describeScalingPlans_applicationSources' - The sources for the applications (up to 10). If you specify scaling plan
-- names, you cannot specify application sources.
newDescribeScalingPlans ::
  DescribeScalingPlans
newDescribeScalingPlans =
  DescribeScalingPlans'
    { nextToken = Prelude.Nothing,
      scalingPlanVersion = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      scalingPlanNames = Prelude.Nothing,
      applicationSources = Prelude.Nothing
    }

-- | The token for the next set of results.
describeScalingPlans_nextToken :: Lens.Lens' DescribeScalingPlans (Prelude.Maybe Prelude.Text)
describeScalingPlans_nextToken = Lens.lens (\DescribeScalingPlans' {nextToken} -> nextToken) (\s@DescribeScalingPlans' {} a -> s {nextToken = a} :: DescribeScalingPlans)

-- | The version number of the scaling plan. Currently, the only valid value
-- is @1@.
--
-- If you specify a scaling plan version, you must also specify a scaling
-- plan name.
describeScalingPlans_scalingPlanVersion :: Lens.Lens' DescribeScalingPlans (Prelude.Maybe Prelude.Integer)
describeScalingPlans_scalingPlanVersion = Lens.lens (\DescribeScalingPlans' {scalingPlanVersion} -> scalingPlanVersion) (\s@DescribeScalingPlans' {} a -> s {scalingPlanVersion = a} :: DescribeScalingPlans)

-- | The maximum number of scalable resources to return. This value can be
-- between 1 and 50. The default value is 50.
describeScalingPlans_maxResults :: Lens.Lens' DescribeScalingPlans (Prelude.Maybe Prelude.Int)
describeScalingPlans_maxResults = Lens.lens (\DescribeScalingPlans' {maxResults} -> maxResults) (\s@DescribeScalingPlans' {} a -> s {maxResults = a} :: DescribeScalingPlans)

-- | The names of the scaling plans (up to 10). If you specify application
-- sources, you cannot specify scaling plan names.
describeScalingPlans_scalingPlanNames :: Lens.Lens' DescribeScalingPlans (Prelude.Maybe [Prelude.Text])
describeScalingPlans_scalingPlanNames = Lens.lens (\DescribeScalingPlans' {scalingPlanNames} -> scalingPlanNames) (\s@DescribeScalingPlans' {} a -> s {scalingPlanNames = a} :: DescribeScalingPlans) Prelude.. Lens.mapping Prelude._Coerce

-- | The sources for the applications (up to 10). If you specify scaling plan
-- names, you cannot specify application sources.
describeScalingPlans_applicationSources :: Lens.Lens' DescribeScalingPlans (Prelude.Maybe [ApplicationSource])
describeScalingPlans_applicationSources = Lens.lens (\DescribeScalingPlans' {applicationSources} -> applicationSources) (\s@DescribeScalingPlans' {} a -> s {applicationSources = a} :: DescribeScalingPlans) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager DescribeScalingPlans where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeScalingPlansResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeScalingPlansResponse_scalingPlans
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeScalingPlans_nextToken
          Lens..~ rs
          Lens.^? describeScalingPlansResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeScalingPlans where
  type
    Rs DescribeScalingPlans =
      DescribeScalingPlansResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScalingPlansResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ScalingPlans"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeScalingPlans

instance Prelude.NFData DescribeScalingPlans

instance Prelude.ToHeaders DescribeScalingPlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AnyScaleScalingPlannerFrontendService.DescribeScalingPlans" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeScalingPlans where
  toJSON DescribeScalingPlans' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("ScalingPlanVersion" Prelude..=)
              Prelude.<$> scalingPlanVersion,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("ScalingPlanNames" Prelude..=)
              Prelude.<$> scalingPlanNames,
            ("ApplicationSources" Prelude..=)
              Prelude.<$> applicationSources
          ]
      )

instance Prelude.ToPath DescribeScalingPlans where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeScalingPlans where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeScalingPlansResponse' smart constructor.
data DescribeScalingPlansResponse = DescribeScalingPlansResponse'
  { -- | The token required to get the next set of results. This value is @null@
    -- if there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the scaling plans.
    scalingPlans :: Prelude.Maybe [ScalingPlan],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingPlansResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScalingPlansResponse_nextToken' - The token required to get the next set of results. This value is @null@
-- if there are no more results to return.
--
-- 'scalingPlans', 'describeScalingPlansResponse_scalingPlans' - Information about the scaling plans.
--
-- 'httpStatus', 'describeScalingPlansResponse_httpStatus' - The response's http status code.
newDescribeScalingPlansResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeScalingPlansResponse
newDescribeScalingPlansResponse pHttpStatus_ =
  DescribeScalingPlansResponse'
    { nextToken =
        Prelude.Nothing,
      scalingPlans = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to get the next set of results. This value is @null@
-- if there are no more results to return.
describeScalingPlansResponse_nextToken :: Lens.Lens' DescribeScalingPlansResponse (Prelude.Maybe Prelude.Text)
describeScalingPlansResponse_nextToken = Lens.lens (\DescribeScalingPlansResponse' {nextToken} -> nextToken) (\s@DescribeScalingPlansResponse' {} a -> s {nextToken = a} :: DescribeScalingPlansResponse)

-- | Information about the scaling plans.
describeScalingPlansResponse_scalingPlans :: Lens.Lens' DescribeScalingPlansResponse (Prelude.Maybe [ScalingPlan])
describeScalingPlansResponse_scalingPlans = Lens.lens (\DescribeScalingPlansResponse' {scalingPlans} -> scalingPlans) (\s@DescribeScalingPlansResponse' {} a -> s {scalingPlans = a} :: DescribeScalingPlansResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeScalingPlansResponse_httpStatus :: Lens.Lens' DescribeScalingPlansResponse Prelude.Int
describeScalingPlansResponse_httpStatus = Lens.lens (\DescribeScalingPlansResponse' {httpStatus} -> httpStatus) (\s@DescribeScalingPlansResponse' {} a -> s {httpStatus = a} :: DescribeScalingPlansResponse)

instance Prelude.NFData DescribeScalingPlansResponse
