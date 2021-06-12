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
-- Module      : Network.AWS.Inspector.DescribeAssessmentRuns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment runs that are specified by the ARNs of the
-- assessment runs.
module Network.AWS.Inspector.DescribeAssessmentRuns
  ( -- * Creating a Request
    DescribeAssessmentRuns (..),
    newDescribeAssessmentRuns,

    -- * Request Lenses
    describeAssessmentRuns_assessmentRunArns,

    -- * Destructuring the Response
    DescribeAssessmentRunsResponse (..),
    newDescribeAssessmentRunsResponse,

    -- * Response Lenses
    describeAssessmentRunsResponse_httpStatus,
    describeAssessmentRunsResponse_assessmentRuns,
    describeAssessmentRunsResponse_failedItems,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAssessmentRuns' smart constructor.
data DescribeAssessmentRuns = DescribeAssessmentRuns'
  { -- | The ARN that specifies the assessment run that you want to describe.
    assessmentRunArns :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAssessmentRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentRunArns', 'describeAssessmentRuns_assessmentRunArns' - The ARN that specifies the assessment run that you want to describe.
newDescribeAssessmentRuns ::
  -- | 'assessmentRunArns'
  Core.NonEmpty Core.Text ->
  DescribeAssessmentRuns
newDescribeAssessmentRuns pAssessmentRunArns_ =
  DescribeAssessmentRuns'
    { assessmentRunArns =
        Lens._Coerce Lens.# pAssessmentRunArns_
    }

-- | The ARN that specifies the assessment run that you want to describe.
describeAssessmentRuns_assessmentRunArns :: Lens.Lens' DescribeAssessmentRuns (Core.NonEmpty Core.Text)
describeAssessmentRuns_assessmentRunArns = Lens.lens (\DescribeAssessmentRuns' {assessmentRunArns} -> assessmentRunArns) (\s@DescribeAssessmentRuns' {} a -> s {assessmentRunArns = a} :: DescribeAssessmentRuns) Core.. Lens._Coerce

instance Core.AWSRequest DescribeAssessmentRuns where
  type
    AWSResponse DescribeAssessmentRuns =
      DescribeAssessmentRunsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssessmentRunsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "assessmentRuns" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failedItems" Core..!@ Core.mempty)
      )

instance Core.Hashable DescribeAssessmentRuns

instance Core.NFData DescribeAssessmentRuns

instance Core.ToHeaders DescribeAssessmentRuns where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DescribeAssessmentRuns" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAssessmentRuns where
  toJSON DescribeAssessmentRuns' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("assessmentRunArns" Core..= assessmentRunArns)
          ]
      )

instance Core.ToPath DescribeAssessmentRuns where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAssessmentRuns where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAssessmentRunsResponse' smart constructor.
data DescribeAssessmentRunsResponse = DescribeAssessmentRunsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Information about the assessment run.
    assessmentRuns :: [AssessmentRun],
    -- | Assessment run details that cannot be described. An error code is
    -- provided for each failed item.
    failedItems :: Core.HashMap Core.Text FailedItemDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAssessmentRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAssessmentRunsResponse_httpStatus' - The response's http status code.
--
-- 'assessmentRuns', 'describeAssessmentRunsResponse_assessmentRuns' - Information about the assessment run.
--
-- 'failedItems', 'describeAssessmentRunsResponse_failedItems' - Assessment run details that cannot be described. An error code is
-- provided for each failed item.
newDescribeAssessmentRunsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAssessmentRunsResponse
newDescribeAssessmentRunsResponse pHttpStatus_ =
  DescribeAssessmentRunsResponse'
    { httpStatus =
        pHttpStatus_,
      assessmentRuns = Core.mempty,
      failedItems = Core.mempty
    }

-- | The response's http status code.
describeAssessmentRunsResponse_httpStatus :: Lens.Lens' DescribeAssessmentRunsResponse Core.Int
describeAssessmentRunsResponse_httpStatus = Lens.lens (\DescribeAssessmentRunsResponse' {httpStatus} -> httpStatus) (\s@DescribeAssessmentRunsResponse' {} a -> s {httpStatus = a} :: DescribeAssessmentRunsResponse)

-- | Information about the assessment run.
describeAssessmentRunsResponse_assessmentRuns :: Lens.Lens' DescribeAssessmentRunsResponse [AssessmentRun]
describeAssessmentRunsResponse_assessmentRuns = Lens.lens (\DescribeAssessmentRunsResponse' {assessmentRuns} -> assessmentRuns) (\s@DescribeAssessmentRunsResponse' {} a -> s {assessmentRuns = a} :: DescribeAssessmentRunsResponse) Core.. Lens._Coerce

-- | Assessment run details that cannot be described. An error code is
-- provided for each failed item.
describeAssessmentRunsResponse_failedItems :: Lens.Lens' DescribeAssessmentRunsResponse (Core.HashMap Core.Text FailedItemDetails)
describeAssessmentRunsResponse_failedItems = Lens.lens (\DescribeAssessmentRunsResponse' {failedItems} -> failedItems) (\s@DescribeAssessmentRunsResponse' {} a -> s {failedItems = a} :: DescribeAssessmentRunsResponse) Core.. Lens._Coerce

instance Core.NFData DescribeAssessmentRunsResponse
