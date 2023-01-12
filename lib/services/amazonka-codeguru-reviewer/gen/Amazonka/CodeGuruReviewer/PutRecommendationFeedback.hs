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
-- Module      : Amazonka.CodeGuruReviewer.PutRecommendationFeedback
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores customer feedback for a CodeGuru Reviewer recommendation. When
-- this API is called again with different reactions the previous feedback
-- is overwritten.
module Amazonka.CodeGuruReviewer.PutRecommendationFeedback
  ( -- * Creating a Request
    PutRecommendationFeedback (..),
    newPutRecommendationFeedback,

    -- * Request Lenses
    putRecommendationFeedback_codeReviewArn,
    putRecommendationFeedback_recommendationId,
    putRecommendationFeedback_reactions,

    -- * Destructuring the Response
    PutRecommendationFeedbackResponse (..),
    newPutRecommendationFeedbackResponse,

    -- * Response Lenses
    putRecommendationFeedbackResponse_httpStatus,
  )
where

import Amazonka.CodeGuruReviewer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRecommendationFeedback' smart constructor.
data PutRecommendationFeedback = PutRecommendationFeedback'
  { -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
    -- object.
    codeReviewArn :: Prelude.Text,
    -- | The recommendation ID that can be used to track the provided
    -- recommendations and then to collect the feedback.
    recommendationId :: Prelude.Text,
    -- | List for storing reactions. Reactions are utf-8 text code for emojis. If
    -- you send an empty list it clears all your feedback.
    reactions :: [Reaction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecommendationFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeReviewArn', 'putRecommendationFeedback_codeReviewArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
--
-- 'recommendationId', 'putRecommendationFeedback_recommendationId' - The recommendation ID that can be used to track the provided
-- recommendations and then to collect the feedback.
--
-- 'reactions', 'putRecommendationFeedback_reactions' - List for storing reactions. Reactions are utf-8 text code for emojis. If
-- you send an empty list it clears all your feedback.
newPutRecommendationFeedback ::
  -- | 'codeReviewArn'
  Prelude.Text ->
  -- | 'recommendationId'
  Prelude.Text ->
  PutRecommendationFeedback
newPutRecommendationFeedback
  pCodeReviewArn_
  pRecommendationId_ =
    PutRecommendationFeedback'
      { codeReviewArn =
          pCodeReviewArn_,
        recommendationId = pRecommendationId_,
        reactions = Prelude.mempty
      }

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
putRecommendationFeedback_codeReviewArn :: Lens.Lens' PutRecommendationFeedback Prelude.Text
putRecommendationFeedback_codeReviewArn = Lens.lens (\PutRecommendationFeedback' {codeReviewArn} -> codeReviewArn) (\s@PutRecommendationFeedback' {} a -> s {codeReviewArn = a} :: PutRecommendationFeedback)

-- | The recommendation ID that can be used to track the provided
-- recommendations and then to collect the feedback.
putRecommendationFeedback_recommendationId :: Lens.Lens' PutRecommendationFeedback Prelude.Text
putRecommendationFeedback_recommendationId = Lens.lens (\PutRecommendationFeedback' {recommendationId} -> recommendationId) (\s@PutRecommendationFeedback' {} a -> s {recommendationId = a} :: PutRecommendationFeedback)

-- | List for storing reactions. Reactions are utf-8 text code for emojis. If
-- you send an empty list it clears all your feedback.
putRecommendationFeedback_reactions :: Lens.Lens' PutRecommendationFeedback [Reaction]
putRecommendationFeedback_reactions = Lens.lens (\PutRecommendationFeedback' {reactions} -> reactions) (\s@PutRecommendationFeedback' {} a -> s {reactions = a} :: PutRecommendationFeedback) Prelude.. Lens.coerced

instance Core.AWSRequest PutRecommendationFeedback where
  type
    AWSResponse PutRecommendationFeedback =
      PutRecommendationFeedbackResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutRecommendationFeedbackResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRecommendationFeedback where
  hashWithSalt _salt PutRecommendationFeedback' {..} =
    _salt `Prelude.hashWithSalt` codeReviewArn
      `Prelude.hashWithSalt` recommendationId
      `Prelude.hashWithSalt` reactions

instance Prelude.NFData PutRecommendationFeedback where
  rnf PutRecommendationFeedback' {..} =
    Prelude.rnf codeReviewArn
      `Prelude.seq` Prelude.rnf recommendationId
      `Prelude.seq` Prelude.rnf reactions

instance Data.ToHeaders PutRecommendationFeedback where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRecommendationFeedback where
  toJSON PutRecommendationFeedback' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CodeReviewArn" Data..= codeReviewArn),
            Prelude.Just
              ("RecommendationId" Data..= recommendationId),
            Prelude.Just ("Reactions" Data..= reactions)
          ]
      )

instance Data.ToPath PutRecommendationFeedback where
  toPath = Prelude.const "/feedback"

instance Data.ToQuery PutRecommendationFeedback where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRecommendationFeedbackResponse' smart constructor.
data PutRecommendationFeedbackResponse = PutRecommendationFeedbackResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecommendationFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putRecommendationFeedbackResponse_httpStatus' - The response's http status code.
newPutRecommendationFeedbackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRecommendationFeedbackResponse
newPutRecommendationFeedbackResponse pHttpStatus_ =
  PutRecommendationFeedbackResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putRecommendationFeedbackResponse_httpStatus :: Lens.Lens' PutRecommendationFeedbackResponse Prelude.Int
putRecommendationFeedbackResponse_httpStatus = Lens.lens (\PutRecommendationFeedbackResponse' {httpStatus} -> httpStatus) (\s@PutRecommendationFeedbackResponse' {} a -> s {httpStatus = a} :: PutRecommendationFeedbackResponse)

instance
  Prelude.NFData
    PutRecommendationFeedbackResponse
  where
  rnf PutRecommendationFeedbackResponse' {..} =
    Prelude.rnf httpStatus
