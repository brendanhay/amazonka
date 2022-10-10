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
-- Module      : Amazonka.Wisdom.PutFeedback
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits feedback to Wisdom. The feedback is used to improve future
-- recommendations from
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_GetRecommendations.html GetRecommendations>
-- or results from
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_QueryAssistant.html QueryAssistant>.
-- Feedback can be resubmitted up to 6 hours after submission.
module Amazonka.Wisdom.PutFeedback
  ( -- * Creating a Request
    PutFeedback (..),
    newPutFeedback,

    -- * Request Lenses
    putFeedback_assistantId,
    putFeedback_feedback,
    putFeedback_targetId,
    putFeedback_targetType,

    -- * Destructuring the Response
    PutFeedbackResponse (..),
    newPutFeedbackResponse,

    -- * Response Lenses
    putFeedbackResponse_httpStatus,
    putFeedbackResponse_assistantArn,
    putFeedbackResponse_assistantId,
    putFeedbackResponse_feedback,
    putFeedbackResponse_targetId,
    putFeedbackResponse_targetType,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newPutFeedback' smart constructor.
data PutFeedback = PutFeedback'
  { -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text,
    -- | The feedback.
    feedback :: FeedbackData,
    -- | The identifier of a recommendation. or The identifier of the result
    -- data.
    targetId :: Prelude.Text,
    -- | The type of the targetId for which The feedback. is targeted.
    targetType :: TargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistantId', 'putFeedback_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
--
-- 'feedback', 'putFeedback_feedback' - The feedback.
--
-- 'targetId', 'putFeedback_targetId' - The identifier of a recommendation. or The identifier of the result
-- data.
--
-- 'targetType', 'putFeedback_targetType' - The type of the targetId for which The feedback. is targeted.
newPutFeedback ::
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'feedback'
  FeedbackData ->
  -- | 'targetId'
  Prelude.Text ->
  -- | 'targetType'
  TargetType ->
  PutFeedback
newPutFeedback
  pAssistantId_
  pFeedback_
  pTargetId_
  pTargetType_ =
    PutFeedback'
      { assistantId = pAssistantId_,
        feedback = pFeedback_,
        targetId = pTargetId_,
        targetType = pTargetType_
      }

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
putFeedback_assistantId :: Lens.Lens' PutFeedback Prelude.Text
putFeedback_assistantId = Lens.lens (\PutFeedback' {assistantId} -> assistantId) (\s@PutFeedback' {} a -> s {assistantId = a} :: PutFeedback)

-- | The feedback.
putFeedback_feedback :: Lens.Lens' PutFeedback FeedbackData
putFeedback_feedback = Lens.lens (\PutFeedback' {feedback} -> feedback) (\s@PutFeedback' {} a -> s {feedback = a} :: PutFeedback)

-- | The identifier of a recommendation. or The identifier of the result
-- data.
putFeedback_targetId :: Lens.Lens' PutFeedback Prelude.Text
putFeedback_targetId = Lens.lens (\PutFeedback' {targetId} -> targetId) (\s@PutFeedback' {} a -> s {targetId = a} :: PutFeedback)

-- | The type of the targetId for which The feedback. is targeted.
putFeedback_targetType :: Lens.Lens' PutFeedback TargetType
putFeedback_targetType = Lens.lens (\PutFeedback' {targetType} -> targetType) (\s@PutFeedback' {} a -> s {targetType = a} :: PutFeedback)

instance Core.AWSRequest PutFeedback where
  type AWSResponse PutFeedback = PutFeedbackResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutFeedbackResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "assistantArn")
            Prelude.<*> (x Core..:> "assistantId")
            Prelude.<*> (x Core..:> "feedback")
            Prelude.<*> (x Core..:> "targetId")
            Prelude.<*> (x Core..:> "targetType")
      )

instance Prelude.Hashable PutFeedback where
  hashWithSalt _salt PutFeedback' {..} =
    _salt `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` feedback
      `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` targetType

instance Prelude.NFData PutFeedback where
  rnf PutFeedback' {..} =
    Prelude.rnf assistantId
      `Prelude.seq` Prelude.rnf feedback
      `Prelude.seq` Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf targetType

instance Core.ToHeaders PutFeedback where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutFeedback where
  toJSON PutFeedback' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("feedback" Core..= feedback),
            Prelude.Just ("targetId" Core..= targetId),
            Prelude.Just ("targetType" Core..= targetType)
          ]
      )

instance Core.ToPath PutFeedback where
  toPath PutFeedback' {..} =
    Prelude.mconcat
      ["/assistants/", Core.toBS assistantId, "/feedback"]

instance Core.ToQuery PutFeedback where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutFeedbackResponse' smart constructor.
data PutFeedbackResponse = PutFeedbackResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Wisdom assistant.
    assistantArn :: Prelude.Text,
    -- | The identifier of the Wisdom assistant.
    assistantId :: Prelude.Text,
    -- | The feedback.
    feedback :: FeedbackData,
    -- | The identifier of a recommendation. or The identifier of the result
    -- data.
    targetId :: Prelude.Text,
    -- | The type of the targetId for which The feedback. is targeted.
    targetType :: TargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putFeedbackResponse_httpStatus' - The response's http status code.
--
-- 'assistantArn', 'putFeedbackResponse_assistantArn' - The Amazon Resource Name (ARN) of the Wisdom assistant.
--
-- 'assistantId', 'putFeedbackResponse_assistantId' - The identifier of the Wisdom assistant.
--
-- 'feedback', 'putFeedbackResponse_feedback' - The feedback.
--
-- 'targetId', 'putFeedbackResponse_targetId' - The identifier of a recommendation. or The identifier of the result
-- data.
--
-- 'targetType', 'putFeedbackResponse_targetType' - The type of the targetId for which The feedback. is targeted.
newPutFeedbackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assistantArn'
  Prelude.Text ->
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'feedback'
  FeedbackData ->
  -- | 'targetId'
  Prelude.Text ->
  -- | 'targetType'
  TargetType ->
  PutFeedbackResponse
newPutFeedbackResponse
  pHttpStatus_
  pAssistantArn_
  pAssistantId_
  pFeedback_
  pTargetId_
  pTargetType_ =
    PutFeedbackResponse'
      { httpStatus = pHttpStatus_,
        assistantArn = pAssistantArn_,
        assistantId = pAssistantId_,
        feedback = pFeedback_,
        targetId = pTargetId_,
        targetType = pTargetType_
      }

-- | The response's http status code.
putFeedbackResponse_httpStatus :: Lens.Lens' PutFeedbackResponse Prelude.Int
putFeedbackResponse_httpStatus = Lens.lens (\PutFeedbackResponse' {httpStatus} -> httpStatus) (\s@PutFeedbackResponse' {} a -> s {httpStatus = a} :: PutFeedbackResponse)

-- | The Amazon Resource Name (ARN) of the Wisdom assistant.
putFeedbackResponse_assistantArn :: Lens.Lens' PutFeedbackResponse Prelude.Text
putFeedbackResponse_assistantArn = Lens.lens (\PutFeedbackResponse' {assistantArn} -> assistantArn) (\s@PutFeedbackResponse' {} a -> s {assistantArn = a} :: PutFeedbackResponse)

-- | The identifier of the Wisdom assistant.
putFeedbackResponse_assistantId :: Lens.Lens' PutFeedbackResponse Prelude.Text
putFeedbackResponse_assistantId = Lens.lens (\PutFeedbackResponse' {assistantId} -> assistantId) (\s@PutFeedbackResponse' {} a -> s {assistantId = a} :: PutFeedbackResponse)

-- | The feedback.
putFeedbackResponse_feedback :: Lens.Lens' PutFeedbackResponse FeedbackData
putFeedbackResponse_feedback = Lens.lens (\PutFeedbackResponse' {feedback} -> feedback) (\s@PutFeedbackResponse' {} a -> s {feedback = a} :: PutFeedbackResponse)

-- | The identifier of a recommendation. or The identifier of the result
-- data.
putFeedbackResponse_targetId :: Lens.Lens' PutFeedbackResponse Prelude.Text
putFeedbackResponse_targetId = Lens.lens (\PutFeedbackResponse' {targetId} -> targetId) (\s@PutFeedbackResponse' {} a -> s {targetId = a} :: PutFeedbackResponse)

-- | The type of the targetId for which The feedback. is targeted.
putFeedbackResponse_targetType :: Lens.Lens' PutFeedbackResponse TargetType
putFeedbackResponse_targetType = Lens.lens (\PutFeedbackResponse' {targetType} -> targetType) (\s@PutFeedbackResponse' {} a -> s {targetType = a} :: PutFeedbackResponse)

instance Prelude.NFData PutFeedbackResponse where
  rnf PutFeedbackResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assistantArn
      `Prelude.seq` Prelude.rnf assistantId
      `Prelude.seq` Prelude.rnf feedback
      `Prelude.seq` Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf targetType
