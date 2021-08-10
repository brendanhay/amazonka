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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAssessmentRuns' smart constructor.
data DescribeAssessmentRuns = DescribeAssessmentRuns'
  { -- | The ARN that specifies the assessment run that you want to describe.
    assessmentRunArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  DescribeAssessmentRuns
newDescribeAssessmentRuns pAssessmentRunArns_ =
  DescribeAssessmentRuns'
    { assessmentRunArns =
        Lens._Coerce Lens.# pAssessmentRunArns_
    }

-- | The ARN that specifies the assessment run that you want to describe.
describeAssessmentRuns_assessmentRunArns :: Lens.Lens' DescribeAssessmentRuns (Prelude.NonEmpty Prelude.Text)
describeAssessmentRuns_assessmentRunArns = Lens.lens (\DescribeAssessmentRuns' {assessmentRunArns} -> assessmentRunArns) (\s@DescribeAssessmentRuns' {} a -> s {assessmentRunArns = a} :: DescribeAssessmentRuns) Prelude.. Lens._Coerce

instance Core.AWSRequest DescribeAssessmentRuns where
  type
    AWSResponse DescribeAssessmentRuns =
      DescribeAssessmentRunsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssessmentRunsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "assessmentRuns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "failedItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeAssessmentRuns

instance Prelude.NFData DescribeAssessmentRuns

instance Core.ToHeaders DescribeAssessmentRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DescribeAssessmentRuns" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAssessmentRuns where
  toJSON DescribeAssessmentRuns' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("assessmentRunArns" Core..= assessmentRunArns)
          ]
      )

instance Core.ToPath DescribeAssessmentRuns where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAssessmentRuns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAssessmentRunsResponse' smart constructor.
data DescribeAssessmentRunsResponse = DescribeAssessmentRunsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the assessment run.
    assessmentRuns :: [AssessmentRun],
    -- | Assessment run details that cannot be described. An error code is
    -- provided for each failed item.
    failedItems :: Prelude.HashMap Prelude.Text FailedItemDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeAssessmentRunsResponse
newDescribeAssessmentRunsResponse pHttpStatus_ =
  DescribeAssessmentRunsResponse'
    { httpStatus =
        pHttpStatus_,
      assessmentRuns = Prelude.mempty,
      failedItems = Prelude.mempty
    }

-- | The response's http status code.
describeAssessmentRunsResponse_httpStatus :: Lens.Lens' DescribeAssessmentRunsResponse Prelude.Int
describeAssessmentRunsResponse_httpStatus = Lens.lens (\DescribeAssessmentRunsResponse' {httpStatus} -> httpStatus) (\s@DescribeAssessmentRunsResponse' {} a -> s {httpStatus = a} :: DescribeAssessmentRunsResponse)

-- | Information about the assessment run.
describeAssessmentRunsResponse_assessmentRuns :: Lens.Lens' DescribeAssessmentRunsResponse [AssessmentRun]
describeAssessmentRunsResponse_assessmentRuns = Lens.lens (\DescribeAssessmentRunsResponse' {assessmentRuns} -> assessmentRuns) (\s@DescribeAssessmentRunsResponse' {} a -> s {assessmentRuns = a} :: DescribeAssessmentRunsResponse) Prelude.. Lens._Coerce

-- | Assessment run details that cannot be described. An error code is
-- provided for each failed item.
describeAssessmentRunsResponse_failedItems :: Lens.Lens' DescribeAssessmentRunsResponse (Prelude.HashMap Prelude.Text FailedItemDetails)
describeAssessmentRunsResponse_failedItems = Lens.lens (\DescribeAssessmentRunsResponse' {failedItems} -> failedItems) (\s@DescribeAssessmentRunsResponse' {} a -> s {failedItems = a} :: DescribeAssessmentRunsResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    DescribeAssessmentRunsResponse
