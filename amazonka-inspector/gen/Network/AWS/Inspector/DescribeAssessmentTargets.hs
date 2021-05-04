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
-- Module      : Network.AWS.Inspector.DescribeAssessmentTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment targets that are specified by the ARNs of the
-- assessment targets.
module Network.AWS.Inspector.DescribeAssessmentTargets
  ( -- * Creating a Request
    DescribeAssessmentTargets (..),
    newDescribeAssessmentTargets,

    -- * Request Lenses
    describeAssessmentTargets_assessmentTargetArns,

    -- * Destructuring the Response
    DescribeAssessmentTargetsResponse (..),
    newDescribeAssessmentTargetsResponse,

    -- * Response Lenses
    describeAssessmentTargetsResponse_httpStatus,
    describeAssessmentTargetsResponse_assessmentTargets,
    describeAssessmentTargetsResponse_failedItems,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAssessmentTargets' smart constructor.
data DescribeAssessmentTargets = DescribeAssessmentTargets'
  { -- | The ARNs that specifies the assessment targets that you want to
    -- describe.
    assessmentTargetArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssessmentTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentTargetArns', 'describeAssessmentTargets_assessmentTargetArns' - The ARNs that specifies the assessment targets that you want to
-- describe.
newDescribeAssessmentTargets ::
  -- | 'assessmentTargetArns'
  Prelude.NonEmpty Prelude.Text ->
  DescribeAssessmentTargets
newDescribeAssessmentTargets pAssessmentTargetArns_ =
  DescribeAssessmentTargets'
    { assessmentTargetArns =
        Prelude._Coerce Lens.# pAssessmentTargetArns_
    }

-- | The ARNs that specifies the assessment targets that you want to
-- describe.
describeAssessmentTargets_assessmentTargetArns :: Lens.Lens' DescribeAssessmentTargets (Prelude.NonEmpty Prelude.Text)
describeAssessmentTargets_assessmentTargetArns = Lens.lens (\DescribeAssessmentTargets' {assessmentTargetArns} -> assessmentTargetArns) (\s@DescribeAssessmentTargets' {} a -> s {assessmentTargetArns = a} :: DescribeAssessmentTargets) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DescribeAssessmentTargets where
  type
    Rs DescribeAssessmentTargets =
      DescribeAssessmentTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssessmentTargetsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "assessmentTargets"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..?> "failedItems"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable DescribeAssessmentTargets

instance Prelude.NFData DescribeAssessmentTargets

instance Prelude.ToHeaders DescribeAssessmentTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "InspectorService.DescribeAssessmentTargets" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeAssessmentTargets where
  toJSON DescribeAssessmentTargets' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "assessmentTargetArns"
                  Prelude..= assessmentTargetArns
              )
          ]
      )

instance Prelude.ToPath DescribeAssessmentTargets where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeAssessmentTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAssessmentTargetsResponse' smart constructor.
data DescribeAssessmentTargetsResponse = DescribeAssessmentTargetsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the assessment targets.
    assessmentTargets :: [AssessmentTarget],
    -- | Assessment target details that cannot be described. An error code is
    -- provided for each failed item.
    failedItems :: Prelude.HashMap Prelude.Text FailedItemDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssessmentTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAssessmentTargetsResponse_httpStatus' - The response's http status code.
--
-- 'assessmentTargets', 'describeAssessmentTargetsResponse_assessmentTargets' - Information about the assessment targets.
--
-- 'failedItems', 'describeAssessmentTargetsResponse_failedItems' - Assessment target details that cannot be described. An error code is
-- provided for each failed item.
newDescribeAssessmentTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAssessmentTargetsResponse
newDescribeAssessmentTargetsResponse pHttpStatus_ =
  DescribeAssessmentTargetsResponse'
    { httpStatus =
        pHttpStatus_,
      assessmentTargets = Prelude.mempty,
      failedItems = Prelude.mempty
    }

-- | The response's http status code.
describeAssessmentTargetsResponse_httpStatus :: Lens.Lens' DescribeAssessmentTargetsResponse Prelude.Int
describeAssessmentTargetsResponse_httpStatus = Lens.lens (\DescribeAssessmentTargetsResponse' {httpStatus} -> httpStatus) (\s@DescribeAssessmentTargetsResponse' {} a -> s {httpStatus = a} :: DescribeAssessmentTargetsResponse)

-- | Information about the assessment targets.
describeAssessmentTargetsResponse_assessmentTargets :: Lens.Lens' DescribeAssessmentTargetsResponse [AssessmentTarget]
describeAssessmentTargetsResponse_assessmentTargets = Lens.lens (\DescribeAssessmentTargetsResponse' {assessmentTargets} -> assessmentTargets) (\s@DescribeAssessmentTargetsResponse' {} a -> s {assessmentTargets = a} :: DescribeAssessmentTargetsResponse) Prelude.. Prelude._Coerce

-- | Assessment target details that cannot be described. An error code is
-- provided for each failed item.
describeAssessmentTargetsResponse_failedItems :: Lens.Lens' DescribeAssessmentTargetsResponse (Prelude.HashMap Prelude.Text FailedItemDetails)
describeAssessmentTargetsResponse_failedItems = Lens.lens (\DescribeAssessmentTargetsResponse' {failedItems} -> failedItems) (\s@DescribeAssessmentTargetsResponse' {} a -> s {failedItems = a} :: DescribeAssessmentTargetsResponse) Prelude.. Prelude._Coerce

instance
  Prelude.NFData
    DescribeAssessmentTargetsResponse
