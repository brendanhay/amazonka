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
-- Module      : Network.AWS.CostExplorer.ProvideAnomalyFeedback
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the feedback property of a given cost anomaly.
module Network.AWS.CostExplorer.ProvideAnomalyFeedback
  ( -- * Creating a Request
    ProvideAnomalyFeedback (..),
    newProvideAnomalyFeedback,

    -- * Request Lenses
    provideAnomalyFeedback_anomalyId,
    provideAnomalyFeedback_feedback,

    -- * Destructuring the Response
    ProvideAnomalyFeedbackResponse (..),
    newProvideAnomalyFeedbackResponse,

    -- * Response Lenses
    provideAnomalyFeedbackResponse_httpStatus,
    provideAnomalyFeedbackResponse_anomalyId,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newProvideAnomalyFeedback' smart constructor.
data ProvideAnomalyFeedback = ProvideAnomalyFeedback'
  { -- | A cost anomaly ID.
    anomalyId :: Core.Text,
    -- | Describes whether the cost anomaly was a planned activity or you
    -- considered it an anomaly.
    feedback :: AnomalyFeedbackType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvideAnomalyFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyId', 'provideAnomalyFeedback_anomalyId' - A cost anomaly ID.
--
-- 'feedback', 'provideAnomalyFeedback_feedback' - Describes whether the cost anomaly was a planned activity or you
-- considered it an anomaly.
newProvideAnomalyFeedback ::
  -- | 'anomalyId'
  Core.Text ->
  -- | 'feedback'
  AnomalyFeedbackType ->
  ProvideAnomalyFeedback
newProvideAnomalyFeedback pAnomalyId_ pFeedback_ =
  ProvideAnomalyFeedback'
    { anomalyId = pAnomalyId_,
      feedback = pFeedback_
    }

-- | A cost anomaly ID.
provideAnomalyFeedback_anomalyId :: Lens.Lens' ProvideAnomalyFeedback Core.Text
provideAnomalyFeedback_anomalyId = Lens.lens (\ProvideAnomalyFeedback' {anomalyId} -> anomalyId) (\s@ProvideAnomalyFeedback' {} a -> s {anomalyId = a} :: ProvideAnomalyFeedback)

-- | Describes whether the cost anomaly was a planned activity or you
-- considered it an anomaly.
provideAnomalyFeedback_feedback :: Lens.Lens' ProvideAnomalyFeedback AnomalyFeedbackType
provideAnomalyFeedback_feedback = Lens.lens (\ProvideAnomalyFeedback' {feedback} -> feedback) (\s@ProvideAnomalyFeedback' {} a -> s {feedback = a} :: ProvideAnomalyFeedback)

instance Core.AWSRequest ProvideAnomalyFeedback where
  type
    AWSResponse ProvideAnomalyFeedback =
      ProvideAnomalyFeedbackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ProvideAnomalyFeedbackResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "AnomalyId")
      )

instance Core.Hashable ProvideAnomalyFeedback

instance Core.NFData ProvideAnomalyFeedback

instance Core.ToHeaders ProvideAnomalyFeedback where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.ProvideAnomalyFeedback" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ProvideAnomalyFeedback where
  toJSON ProvideAnomalyFeedback' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AnomalyId" Core..= anomalyId),
            Core.Just ("Feedback" Core..= feedback)
          ]
      )

instance Core.ToPath ProvideAnomalyFeedback where
  toPath = Core.const "/"

instance Core.ToQuery ProvideAnomalyFeedback where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newProvideAnomalyFeedbackResponse' smart constructor.
data ProvideAnomalyFeedbackResponse = ProvideAnomalyFeedbackResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ID of the modified cost anomaly.
    anomalyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvideAnomalyFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'provideAnomalyFeedbackResponse_httpStatus' - The response's http status code.
--
-- 'anomalyId', 'provideAnomalyFeedbackResponse_anomalyId' - The ID of the modified cost anomaly.
newProvideAnomalyFeedbackResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'anomalyId'
  Core.Text ->
  ProvideAnomalyFeedbackResponse
newProvideAnomalyFeedbackResponse
  pHttpStatus_
  pAnomalyId_ =
    ProvideAnomalyFeedbackResponse'
      { httpStatus =
          pHttpStatus_,
        anomalyId = pAnomalyId_
      }

-- | The response's http status code.
provideAnomalyFeedbackResponse_httpStatus :: Lens.Lens' ProvideAnomalyFeedbackResponse Core.Int
provideAnomalyFeedbackResponse_httpStatus = Lens.lens (\ProvideAnomalyFeedbackResponse' {httpStatus} -> httpStatus) (\s@ProvideAnomalyFeedbackResponse' {} a -> s {httpStatus = a} :: ProvideAnomalyFeedbackResponse)

-- | The ID of the modified cost anomaly.
provideAnomalyFeedbackResponse_anomalyId :: Lens.Lens' ProvideAnomalyFeedbackResponse Core.Text
provideAnomalyFeedbackResponse_anomalyId = Lens.lens (\ProvideAnomalyFeedbackResponse' {anomalyId} -> anomalyId) (\s@ProvideAnomalyFeedbackResponse' {} a -> s {anomalyId = a} :: ProvideAnomalyFeedbackResponse)

instance Core.NFData ProvideAnomalyFeedbackResponse
