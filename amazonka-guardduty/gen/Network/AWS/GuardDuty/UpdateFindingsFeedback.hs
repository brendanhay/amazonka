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
-- Module      : Network.AWS.GuardDuty.UpdateFindingsFeedback
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks the specified GuardDuty findings as useful or not useful.
module Network.AWS.GuardDuty.UpdateFindingsFeedback
  ( -- * Creating a Request
    UpdateFindingsFeedback (..),
    newUpdateFindingsFeedback,

    -- * Request Lenses
    updateFindingsFeedback_comments,
    updateFindingsFeedback_detectorId,
    updateFindingsFeedback_findingIds,
    updateFindingsFeedback_feedback,

    -- * Destructuring the Response
    UpdateFindingsFeedbackResponse (..),
    newUpdateFindingsFeedbackResponse,

    -- * Response Lenses
    updateFindingsFeedbackResponse_httpStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFindingsFeedback' smart constructor.
data UpdateFindingsFeedback = UpdateFindingsFeedback'
  { -- | Additional feedback about the GuardDuty findings.
    comments :: Prelude.Maybe Prelude.Text,
    -- | The ID of the detector associated with the findings to update feedback
    -- for.
    detectorId :: Prelude.Text,
    -- | The IDs of the findings that you want to mark as useful or not useful.
    findingIds :: [Prelude.Text],
    -- | The feedback for the finding.
    feedback :: Feedback
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateFindingsFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comments', 'updateFindingsFeedback_comments' - Additional feedback about the GuardDuty findings.
--
-- 'detectorId', 'updateFindingsFeedback_detectorId' - The ID of the detector associated with the findings to update feedback
-- for.
--
-- 'findingIds', 'updateFindingsFeedback_findingIds' - The IDs of the findings that you want to mark as useful or not useful.
--
-- 'feedback', 'updateFindingsFeedback_feedback' - The feedback for the finding.
newUpdateFindingsFeedback ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'feedback'
  Feedback ->
  UpdateFindingsFeedback
newUpdateFindingsFeedback pDetectorId_ pFeedback_ =
  UpdateFindingsFeedback'
    { comments = Prelude.Nothing,
      detectorId = pDetectorId_,
      findingIds = Prelude.mempty,
      feedback = pFeedback_
    }

-- | Additional feedback about the GuardDuty findings.
updateFindingsFeedback_comments :: Lens.Lens' UpdateFindingsFeedback (Prelude.Maybe Prelude.Text)
updateFindingsFeedback_comments = Lens.lens (\UpdateFindingsFeedback' {comments} -> comments) (\s@UpdateFindingsFeedback' {} a -> s {comments = a} :: UpdateFindingsFeedback)

-- | The ID of the detector associated with the findings to update feedback
-- for.
updateFindingsFeedback_detectorId :: Lens.Lens' UpdateFindingsFeedback Prelude.Text
updateFindingsFeedback_detectorId = Lens.lens (\UpdateFindingsFeedback' {detectorId} -> detectorId) (\s@UpdateFindingsFeedback' {} a -> s {detectorId = a} :: UpdateFindingsFeedback)

-- | The IDs of the findings that you want to mark as useful or not useful.
updateFindingsFeedback_findingIds :: Lens.Lens' UpdateFindingsFeedback [Prelude.Text]
updateFindingsFeedback_findingIds = Lens.lens (\UpdateFindingsFeedback' {findingIds} -> findingIds) (\s@UpdateFindingsFeedback' {} a -> s {findingIds = a} :: UpdateFindingsFeedback) Prelude.. Prelude._Coerce

-- | The feedback for the finding.
updateFindingsFeedback_feedback :: Lens.Lens' UpdateFindingsFeedback Feedback
updateFindingsFeedback_feedback = Lens.lens (\UpdateFindingsFeedback' {feedback} -> feedback) (\s@UpdateFindingsFeedback' {} a -> s {feedback = a} :: UpdateFindingsFeedback)

instance Prelude.AWSRequest UpdateFindingsFeedback where
  type
    Rs UpdateFindingsFeedback =
      UpdateFindingsFeedbackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFindingsFeedbackResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFindingsFeedback

instance Prelude.NFData UpdateFindingsFeedback

instance Prelude.ToHeaders UpdateFindingsFeedback where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateFindingsFeedback where
  toJSON UpdateFindingsFeedback' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("comments" Prelude..=) Prelude.<$> comments,
            Prelude.Just ("findingIds" Prelude..= findingIds),
            Prelude.Just ("feedback" Prelude..= feedback)
          ]
      )

instance Prelude.ToPath UpdateFindingsFeedback where
  toPath UpdateFindingsFeedback' {..} =
    Prelude.mconcat
      [ "/detector/",
        Prelude.toBS detectorId,
        "/findings/feedback"
      ]

instance Prelude.ToQuery UpdateFindingsFeedback where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFindingsFeedbackResponse' smart constructor.
data UpdateFindingsFeedbackResponse = UpdateFindingsFeedbackResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateFindingsFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFindingsFeedbackResponse_httpStatus' - The response's http status code.
newUpdateFindingsFeedbackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFindingsFeedbackResponse
newUpdateFindingsFeedbackResponse pHttpStatus_ =
  UpdateFindingsFeedbackResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateFindingsFeedbackResponse_httpStatus :: Lens.Lens' UpdateFindingsFeedbackResponse Prelude.Int
updateFindingsFeedbackResponse_httpStatus = Lens.lens (\UpdateFindingsFeedbackResponse' {httpStatus} -> httpStatus) (\s@UpdateFindingsFeedbackResponse' {} a -> s {httpStatus = a} :: UpdateFindingsFeedbackResponse)

instance
  Prelude.NFData
    UpdateFindingsFeedbackResponse
