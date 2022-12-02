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
-- Module      : Amazonka.AutoScalingPlans.DescribeScalingPlans
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your scaling plans.
--
-- This operation returns paginated results.
module Amazonka.AutoScalingPlans.DescribeScalingPlans
  ( -- * Creating a Request
    DescribeScalingPlans (..),
    newDescribeScalingPlans,

    -- * Request Lenses
    describeScalingPlans_nextToken,
    describeScalingPlans_scalingPlanVersion,
    describeScalingPlans_applicationSources,
    describeScalingPlans_maxResults,
    describeScalingPlans_scalingPlanNames,

    -- * Destructuring the Response
    DescribeScalingPlansResponse (..),
    newDescribeScalingPlansResponse,

    -- * Response Lenses
    describeScalingPlansResponse_nextToken,
    describeScalingPlansResponse_scalingPlans,
    describeScalingPlansResponse_httpStatus,
  )
where

import Amazonka.AutoScalingPlans.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- | The sources for the applications (up to 10). If you specify scaling plan
    -- names, you cannot specify application sources.
    applicationSources :: Prelude.Maybe [ApplicationSource],
    -- | The maximum number of scalable resources to return. This value can be
    -- between 1 and 50. The default value is 50.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The names of the scaling plans (up to 10). If you specify application
    -- sources, you cannot specify scaling plan names.
    scalingPlanNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'applicationSources', 'describeScalingPlans_applicationSources' - The sources for the applications (up to 10). If you specify scaling plan
-- names, you cannot specify application sources.
--
-- 'maxResults', 'describeScalingPlans_maxResults' - The maximum number of scalable resources to return. This value can be
-- between 1 and 50. The default value is 50.
--
-- 'scalingPlanNames', 'describeScalingPlans_scalingPlanNames' - The names of the scaling plans (up to 10). If you specify application
-- sources, you cannot specify scaling plan names.
newDescribeScalingPlans ::
  DescribeScalingPlans
newDescribeScalingPlans =
  DescribeScalingPlans'
    { nextToken = Prelude.Nothing,
      scalingPlanVersion = Prelude.Nothing,
      applicationSources = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      scalingPlanNames = Prelude.Nothing
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

-- | The sources for the applications (up to 10). If you specify scaling plan
-- names, you cannot specify application sources.
describeScalingPlans_applicationSources :: Lens.Lens' DescribeScalingPlans (Prelude.Maybe [ApplicationSource])
describeScalingPlans_applicationSources = Lens.lens (\DescribeScalingPlans' {applicationSources} -> applicationSources) (\s@DescribeScalingPlans' {} a -> s {applicationSources = a} :: DescribeScalingPlans) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of scalable resources to return. This value can be
-- between 1 and 50. The default value is 50.
describeScalingPlans_maxResults :: Lens.Lens' DescribeScalingPlans (Prelude.Maybe Prelude.Int)
describeScalingPlans_maxResults = Lens.lens (\DescribeScalingPlans' {maxResults} -> maxResults) (\s@DescribeScalingPlans' {} a -> s {maxResults = a} :: DescribeScalingPlans)

-- | The names of the scaling plans (up to 10). If you specify application
-- sources, you cannot specify scaling plan names.
describeScalingPlans_scalingPlanNames :: Lens.Lens' DescribeScalingPlans (Prelude.Maybe [Prelude.Text])
describeScalingPlans_scalingPlanNames = Lens.lens (\DescribeScalingPlans' {scalingPlanNames} -> scalingPlanNames) (\s@DescribeScalingPlans' {} a -> s {scalingPlanNames = a} :: DescribeScalingPlans) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeScalingPlans where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScalingPlansResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeScalingPlansResponse_scalingPlans
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeScalingPlans_nextToken
          Lens..~ rs
          Lens.^? describeScalingPlansResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeScalingPlans where
  type
    AWSResponse DescribeScalingPlans =
      DescribeScalingPlansResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScalingPlansResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ScalingPlans" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeScalingPlans where
  hashWithSalt _salt DescribeScalingPlans' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` scalingPlanVersion
      `Prelude.hashWithSalt` applicationSources
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` scalingPlanNames

instance Prelude.NFData DescribeScalingPlans where
  rnf DescribeScalingPlans' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scalingPlanVersion
      `Prelude.seq` Prelude.rnf applicationSources
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf scalingPlanNames

instance Data.ToHeaders DescribeScalingPlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AnyScaleScalingPlannerFrontendService.DescribeScalingPlans" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeScalingPlans where
  toJSON DescribeScalingPlans' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ScalingPlanVersion" Data..=)
              Prelude.<$> scalingPlanVersion,
            ("ApplicationSources" Data..=)
              Prelude.<$> applicationSources,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("ScalingPlanNames" Data..=)
              Prelude.<$> scalingPlanNames
          ]
      )

instance Data.ToPath DescribeScalingPlans where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeScalingPlans where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
describeScalingPlansResponse_scalingPlans = Lens.lens (\DescribeScalingPlansResponse' {scalingPlans} -> scalingPlans) (\s@DescribeScalingPlansResponse' {} a -> s {scalingPlans = a} :: DescribeScalingPlansResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeScalingPlansResponse_httpStatus :: Lens.Lens' DescribeScalingPlansResponse Prelude.Int
describeScalingPlansResponse_httpStatus = Lens.lens (\DescribeScalingPlansResponse' {httpStatus} -> httpStatus) (\s@DescribeScalingPlansResponse' {} a -> s {httpStatus = a} :: DescribeScalingPlansResponse)

instance Prelude.NFData DescribeScalingPlansResponse where
  rnf DescribeScalingPlansResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scalingPlans
      `Prelude.seq` Prelude.rnf httpStatus
