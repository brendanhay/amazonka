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
-- Module      : Amazonka.CodeGuruReviewer.DescribeRecommendationFeedback
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the customer feedback for a CodeGuru Reviewer recommendation.
module Amazonka.CodeGuruReviewer.DescribeRecommendationFeedback
  ( -- * Creating a Request
    DescribeRecommendationFeedback (..),
    newDescribeRecommendationFeedback,

    -- * Request Lenses
    describeRecommendationFeedback_userId,
    describeRecommendationFeedback_codeReviewArn,
    describeRecommendationFeedback_recommendationId,

    -- * Destructuring the Response
    DescribeRecommendationFeedbackResponse (..),
    newDescribeRecommendationFeedbackResponse,

    -- * Response Lenses
    describeRecommendationFeedbackResponse_recommendationFeedback,
    describeRecommendationFeedbackResponse_httpStatus,
  )
where

import Amazonka.CodeGuruReviewer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRecommendationFeedback' smart constructor.
data DescribeRecommendationFeedback = DescribeRecommendationFeedback'
  { -- | Optional parameter to describe the feedback for a given user. If this is
    -- not supplied, it defaults to the user making the request.
    --
    -- The @UserId@ is an IAM principal that can be specified as an Amazon Web
    -- Services account ID or an Amazon Resource Name (ARN). For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
    -- in the /Amazon Web Services Identity and Access Management User Guide/.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
    -- object.
    codeReviewArn :: Prelude.Text,
    -- | The recommendation ID that can be used to track the provided
    -- recommendations and then to collect the feedback.
    recommendationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecommendationFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'describeRecommendationFeedback_userId' - Optional parameter to describe the feedback for a given user. If this is
-- not supplied, it defaults to the user making the request.
--
-- The @UserId@ is an IAM principal that can be specified as an Amazon Web
-- Services account ID or an Amazon Resource Name (ARN). For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
--
-- 'codeReviewArn', 'describeRecommendationFeedback_codeReviewArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
--
-- 'recommendationId', 'describeRecommendationFeedback_recommendationId' - The recommendation ID that can be used to track the provided
-- recommendations and then to collect the feedback.
newDescribeRecommendationFeedback ::
  -- | 'codeReviewArn'
  Prelude.Text ->
  -- | 'recommendationId'
  Prelude.Text ->
  DescribeRecommendationFeedback
newDescribeRecommendationFeedback
  pCodeReviewArn_
  pRecommendationId_ =
    DescribeRecommendationFeedback'
      { userId =
          Prelude.Nothing,
        codeReviewArn = pCodeReviewArn_,
        recommendationId = pRecommendationId_
      }

-- | Optional parameter to describe the feedback for a given user. If this is
-- not supplied, it defaults to the user making the request.
--
-- The @UserId@ is an IAM principal that can be specified as an Amazon Web
-- Services account ID or an Amazon Resource Name (ARN). For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#Principal_specifying Specifying a Principal>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
describeRecommendationFeedback_userId :: Lens.Lens' DescribeRecommendationFeedback (Prelude.Maybe Prelude.Text)
describeRecommendationFeedback_userId = Lens.lens (\DescribeRecommendationFeedback' {userId} -> userId) (\s@DescribeRecommendationFeedback' {} a -> s {userId = a} :: DescribeRecommendationFeedback)

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
describeRecommendationFeedback_codeReviewArn :: Lens.Lens' DescribeRecommendationFeedback Prelude.Text
describeRecommendationFeedback_codeReviewArn = Lens.lens (\DescribeRecommendationFeedback' {codeReviewArn} -> codeReviewArn) (\s@DescribeRecommendationFeedback' {} a -> s {codeReviewArn = a} :: DescribeRecommendationFeedback)

-- | The recommendation ID that can be used to track the provided
-- recommendations and then to collect the feedback.
describeRecommendationFeedback_recommendationId :: Lens.Lens' DescribeRecommendationFeedback Prelude.Text
describeRecommendationFeedback_recommendationId = Lens.lens (\DescribeRecommendationFeedback' {recommendationId} -> recommendationId) (\s@DescribeRecommendationFeedback' {} a -> s {recommendationId = a} :: DescribeRecommendationFeedback)

instance
  Core.AWSRequest
    DescribeRecommendationFeedback
  where
  type
    AWSResponse DescribeRecommendationFeedback =
      DescribeRecommendationFeedbackResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecommendationFeedbackResponse'
            Prelude.<$> (x Data..?> "RecommendationFeedback")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRecommendationFeedback
  where
  hashWithSalt
    _salt
    DescribeRecommendationFeedback' {..} =
      _salt `Prelude.hashWithSalt` userId
        `Prelude.hashWithSalt` codeReviewArn
        `Prelude.hashWithSalt` recommendationId

instance
  Prelude.NFData
    DescribeRecommendationFeedback
  where
  rnf DescribeRecommendationFeedback' {..} =
    Prelude.rnf userId
      `Prelude.seq` Prelude.rnf codeReviewArn
      `Prelude.seq` Prelude.rnf recommendationId

instance
  Data.ToHeaders
    DescribeRecommendationFeedback
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeRecommendationFeedback where
  toPath DescribeRecommendationFeedback' {..} =
    Prelude.mconcat
      ["/feedback/", Data.toBS codeReviewArn]

instance Data.ToQuery DescribeRecommendationFeedback where
  toQuery DescribeRecommendationFeedback' {..} =
    Prelude.mconcat
      [ "UserId" Data.=: userId,
        "RecommendationId" Data.=: recommendationId
      ]

-- | /See:/ 'newDescribeRecommendationFeedbackResponse' smart constructor.
data DescribeRecommendationFeedbackResponse = DescribeRecommendationFeedbackResponse'
  { -- | The recommendation feedback given by the user.
    recommendationFeedback :: Prelude.Maybe RecommendationFeedback,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecommendationFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationFeedback', 'describeRecommendationFeedbackResponse_recommendationFeedback' - The recommendation feedback given by the user.
--
-- 'httpStatus', 'describeRecommendationFeedbackResponse_httpStatus' - The response's http status code.
newDescribeRecommendationFeedbackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRecommendationFeedbackResponse
newDescribeRecommendationFeedbackResponse
  pHttpStatus_ =
    DescribeRecommendationFeedbackResponse'
      { recommendationFeedback =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The recommendation feedback given by the user.
describeRecommendationFeedbackResponse_recommendationFeedback :: Lens.Lens' DescribeRecommendationFeedbackResponse (Prelude.Maybe RecommendationFeedback)
describeRecommendationFeedbackResponse_recommendationFeedback = Lens.lens (\DescribeRecommendationFeedbackResponse' {recommendationFeedback} -> recommendationFeedback) (\s@DescribeRecommendationFeedbackResponse' {} a -> s {recommendationFeedback = a} :: DescribeRecommendationFeedbackResponse)

-- | The response's http status code.
describeRecommendationFeedbackResponse_httpStatus :: Lens.Lens' DescribeRecommendationFeedbackResponse Prelude.Int
describeRecommendationFeedbackResponse_httpStatus = Lens.lens (\DescribeRecommendationFeedbackResponse' {httpStatus} -> httpStatus) (\s@DescribeRecommendationFeedbackResponse' {} a -> s {httpStatus = a} :: DescribeRecommendationFeedbackResponse)

instance
  Prelude.NFData
    DescribeRecommendationFeedbackResponse
  where
  rnf DescribeRecommendationFeedbackResponse' {..} =
    Prelude.rnf recommendationFeedback
      `Prelude.seq` Prelude.rnf httpStatus
