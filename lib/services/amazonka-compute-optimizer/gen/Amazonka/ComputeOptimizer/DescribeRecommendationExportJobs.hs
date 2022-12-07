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
-- Module      : Amazonka.ComputeOptimizer.DescribeRecommendationExportJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes recommendation export jobs created in the last seven days.
--
-- Use the ExportAutoScalingGroupRecommendations or
-- ExportEC2InstanceRecommendations actions to request an export of your
-- recommendations. Then use the DescribeRecommendationExportJobs action to
-- view your export jobs.
module Amazonka.ComputeOptimizer.DescribeRecommendationExportJobs
  ( -- * Creating a Request
    DescribeRecommendationExportJobs (..),
    newDescribeRecommendationExportJobs,

    -- * Request Lenses
    describeRecommendationExportJobs_nextToken,
    describeRecommendationExportJobs_filters,
    describeRecommendationExportJobs_maxResults,
    describeRecommendationExportJobs_jobIds,

    -- * Destructuring the Response
    DescribeRecommendationExportJobsResponse (..),
    newDescribeRecommendationExportJobsResponse,

    -- * Response Lenses
    describeRecommendationExportJobsResponse_nextToken,
    describeRecommendationExportJobsResponse_recommendationExportJobs,
    describeRecommendationExportJobsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRecommendationExportJobs' smart constructor.
data DescribeRecommendationExportJobs = DescribeRecommendationExportJobs'
  { -- | The token to advance to the next page of export jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects to specify a filter that returns a more specific
    -- list of export jobs.
    filters :: Prelude.Maybe [JobFilter],
    -- | The maximum number of export jobs to return with a single request.
    --
    -- To retrieve the remaining results, make another request with the
    -- returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The identification numbers of the export jobs to return.
    --
    -- An export job ID is returned when you create an export using the
    -- ExportAutoScalingGroupRecommendations or
    -- ExportEC2InstanceRecommendations actions.
    --
    -- All export jobs created in the last seven days are returned if this
    -- parameter is omitted.
    jobIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecommendationExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRecommendationExportJobs_nextToken' - The token to advance to the next page of export jobs.
--
-- 'filters', 'describeRecommendationExportJobs_filters' - An array of objects to specify a filter that returns a more specific
-- list of export jobs.
--
-- 'maxResults', 'describeRecommendationExportJobs_maxResults' - The maximum number of export jobs to return with a single request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
--
-- 'jobIds', 'describeRecommendationExportJobs_jobIds' - The identification numbers of the export jobs to return.
--
-- An export job ID is returned when you create an export using the
-- ExportAutoScalingGroupRecommendations or
-- ExportEC2InstanceRecommendations actions.
--
-- All export jobs created in the last seven days are returned if this
-- parameter is omitted.
newDescribeRecommendationExportJobs ::
  DescribeRecommendationExportJobs
newDescribeRecommendationExportJobs =
  DescribeRecommendationExportJobs'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobIds = Prelude.Nothing
    }

-- | The token to advance to the next page of export jobs.
describeRecommendationExportJobs_nextToken :: Lens.Lens' DescribeRecommendationExportJobs (Prelude.Maybe Prelude.Text)
describeRecommendationExportJobs_nextToken = Lens.lens (\DescribeRecommendationExportJobs' {nextToken} -> nextToken) (\s@DescribeRecommendationExportJobs' {} a -> s {nextToken = a} :: DescribeRecommendationExportJobs)

-- | An array of objects to specify a filter that returns a more specific
-- list of export jobs.
describeRecommendationExportJobs_filters :: Lens.Lens' DescribeRecommendationExportJobs (Prelude.Maybe [JobFilter])
describeRecommendationExportJobs_filters = Lens.lens (\DescribeRecommendationExportJobs' {filters} -> filters) (\s@DescribeRecommendationExportJobs' {} a -> s {filters = a} :: DescribeRecommendationExportJobs) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of export jobs to return with a single request.
--
-- To retrieve the remaining results, make another request with the
-- returned @nextToken@ value.
describeRecommendationExportJobs_maxResults :: Lens.Lens' DescribeRecommendationExportJobs (Prelude.Maybe Prelude.Int)
describeRecommendationExportJobs_maxResults = Lens.lens (\DescribeRecommendationExportJobs' {maxResults} -> maxResults) (\s@DescribeRecommendationExportJobs' {} a -> s {maxResults = a} :: DescribeRecommendationExportJobs)

-- | The identification numbers of the export jobs to return.
--
-- An export job ID is returned when you create an export using the
-- ExportAutoScalingGroupRecommendations or
-- ExportEC2InstanceRecommendations actions.
--
-- All export jobs created in the last seven days are returned if this
-- parameter is omitted.
describeRecommendationExportJobs_jobIds :: Lens.Lens' DescribeRecommendationExportJobs (Prelude.Maybe [Prelude.Text])
describeRecommendationExportJobs_jobIds = Lens.lens (\DescribeRecommendationExportJobs' {jobIds} -> jobIds) (\s@DescribeRecommendationExportJobs' {} a -> s {jobIds = a} :: DescribeRecommendationExportJobs) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    DescribeRecommendationExportJobs
  where
  type
    AWSResponse DescribeRecommendationExportJobs =
      DescribeRecommendationExportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecommendationExportJobsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "recommendationExportJobs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRecommendationExportJobs
  where
  hashWithSalt
    _salt
    DescribeRecommendationExportJobs' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` jobIds

instance
  Prelude.NFData
    DescribeRecommendationExportJobs
  where
  rnf DescribeRecommendationExportJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf jobIds

instance
  Data.ToHeaders
    DescribeRecommendationExportJobs
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.DescribeRecommendationExportJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRecommendationExportJobs where
  toJSON DescribeRecommendationExportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("jobIds" Data..=) Prelude.<$> jobIds
          ]
      )

instance Data.ToPath DescribeRecommendationExportJobs where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeRecommendationExportJobs
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRecommendationExportJobsResponse' smart constructor.
data DescribeRecommendationExportJobsResponse = DescribeRecommendationExportJobsResponse'
  { -- | The token to use to advance to the next page of export jobs.
    --
    -- This value is null when there are no more pages of export jobs to
    -- return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe recommendation export jobs.
    recommendationExportJobs :: Prelude.Maybe [RecommendationExportJob],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecommendationExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRecommendationExportJobsResponse_nextToken' - The token to use to advance to the next page of export jobs.
--
-- This value is null when there are no more pages of export jobs to
-- return.
--
-- 'recommendationExportJobs', 'describeRecommendationExportJobsResponse_recommendationExportJobs' - An array of objects that describe recommendation export jobs.
--
-- 'httpStatus', 'describeRecommendationExportJobsResponse_httpStatus' - The response's http status code.
newDescribeRecommendationExportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRecommendationExportJobsResponse
newDescribeRecommendationExportJobsResponse
  pHttpStatus_ =
    DescribeRecommendationExportJobsResponse'
      { nextToken =
          Prelude.Nothing,
        recommendationExportJobs =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to advance to the next page of export jobs.
--
-- This value is null when there are no more pages of export jobs to
-- return.
describeRecommendationExportJobsResponse_nextToken :: Lens.Lens' DescribeRecommendationExportJobsResponse (Prelude.Maybe Prelude.Text)
describeRecommendationExportJobsResponse_nextToken = Lens.lens (\DescribeRecommendationExportJobsResponse' {nextToken} -> nextToken) (\s@DescribeRecommendationExportJobsResponse' {} a -> s {nextToken = a} :: DescribeRecommendationExportJobsResponse)

-- | An array of objects that describe recommendation export jobs.
describeRecommendationExportJobsResponse_recommendationExportJobs :: Lens.Lens' DescribeRecommendationExportJobsResponse (Prelude.Maybe [RecommendationExportJob])
describeRecommendationExportJobsResponse_recommendationExportJobs = Lens.lens (\DescribeRecommendationExportJobsResponse' {recommendationExportJobs} -> recommendationExportJobs) (\s@DescribeRecommendationExportJobsResponse' {} a -> s {recommendationExportJobs = a} :: DescribeRecommendationExportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRecommendationExportJobsResponse_httpStatus :: Lens.Lens' DescribeRecommendationExportJobsResponse Prelude.Int
describeRecommendationExportJobsResponse_httpStatus = Lens.lens (\DescribeRecommendationExportJobsResponse' {httpStatus} -> httpStatus) (\s@DescribeRecommendationExportJobsResponse' {} a -> s {httpStatus = a} :: DescribeRecommendationExportJobsResponse)

instance
  Prelude.NFData
    DescribeRecommendationExportJobsResponse
  where
  rnf DescribeRecommendationExportJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendationExportJobs
      `Prelude.seq` Prelude.rnf httpStatus
