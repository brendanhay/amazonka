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
-- Module      : Amazonka.ResilienceHub.DescribeAppAssessment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an assessment for an AWS Resilience Hub application.
module Amazonka.ResilienceHub.DescribeAppAssessment
  ( -- * Creating a Request
    DescribeAppAssessment (..),
    newDescribeAppAssessment,

    -- * Request Lenses
    describeAppAssessment_assessmentArn,

    -- * Destructuring the Response
    DescribeAppAssessmentResponse (..),
    newDescribeAppAssessmentResponse,

    -- * Response Lenses
    describeAppAssessmentResponse_httpStatus,
    describeAppAssessmentResponse_assessment,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAppAssessment' smart constructor.
data DescribeAppAssessment = DescribeAppAssessment'
  { -- | The Amazon Resource Name (ARN) of the assessment. The format for this
    -- ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    assessmentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentArn', 'describeAppAssessment_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newDescribeAppAssessment ::
  -- | 'assessmentArn'
  Prelude.Text ->
  DescribeAppAssessment
newDescribeAppAssessment pAssessmentArn_ =
  DescribeAppAssessment'
    { assessmentArn =
        pAssessmentArn_
    }

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
describeAppAssessment_assessmentArn :: Lens.Lens' DescribeAppAssessment Prelude.Text
describeAppAssessment_assessmentArn = Lens.lens (\DescribeAppAssessment' {assessmentArn} -> assessmentArn) (\s@DescribeAppAssessment' {} a -> s {assessmentArn = a} :: DescribeAppAssessment)

instance Core.AWSRequest DescribeAppAssessment where
  type
    AWSResponse DescribeAppAssessment =
      DescribeAppAssessmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppAssessmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assessment")
      )

instance Prelude.Hashable DescribeAppAssessment where
  hashWithSalt _salt DescribeAppAssessment' {..} =
    _salt `Prelude.hashWithSalt` assessmentArn

instance Prelude.NFData DescribeAppAssessment where
  rnf DescribeAppAssessment' {..} =
    Prelude.rnf assessmentArn

instance Data.ToHeaders DescribeAppAssessment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAppAssessment where
  toJSON DescribeAppAssessment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("assessmentArn" Data..= assessmentArn)
          ]
      )

instance Data.ToPath DescribeAppAssessment where
  toPath = Prelude.const "/describe-app-assessment"

instance Data.ToQuery DescribeAppAssessment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppAssessmentResponse' smart constructor.
data DescribeAppAssessmentResponse = DescribeAppAssessmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The assessment for an AWS Resilience Hub application, returned as an
    -- object. This object includes Amazon Resource Names (ARNs), compliance
    -- information, compliance status, cost, messages, resiliency scores, and
    -- more.
    assessment :: AppAssessment
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAppAssessmentResponse_httpStatus' - The response's http status code.
--
-- 'assessment', 'describeAppAssessmentResponse_assessment' - The assessment for an AWS Resilience Hub application, returned as an
-- object. This object includes Amazon Resource Names (ARNs), compliance
-- information, compliance status, cost, messages, resiliency scores, and
-- more.
newDescribeAppAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assessment'
  AppAssessment ->
  DescribeAppAssessmentResponse
newDescribeAppAssessmentResponse
  pHttpStatus_
  pAssessment_ =
    DescribeAppAssessmentResponse'
      { httpStatus =
          pHttpStatus_,
        assessment = pAssessment_
      }

-- | The response's http status code.
describeAppAssessmentResponse_httpStatus :: Lens.Lens' DescribeAppAssessmentResponse Prelude.Int
describeAppAssessmentResponse_httpStatus = Lens.lens (\DescribeAppAssessmentResponse' {httpStatus} -> httpStatus) (\s@DescribeAppAssessmentResponse' {} a -> s {httpStatus = a} :: DescribeAppAssessmentResponse)

-- | The assessment for an AWS Resilience Hub application, returned as an
-- object. This object includes Amazon Resource Names (ARNs), compliance
-- information, compliance status, cost, messages, resiliency scores, and
-- more.
describeAppAssessmentResponse_assessment :: Lens.Lens' DescribeAppAssessmentResponse AppAssessment
describeAppAssessmentResponse_assessment = Lens.lens (\DescribeAppAssessmentResponse' {assessment} -> assessment) (\s@DescribeAppAssessmentResponse' {} a -> s {assessment = a} :: DescribeAppAssessmentResponse)

instance Prelude.NFData DescribeAppAssessmentResponse where
  rnf DescribeAppAssessmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assessment
