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

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newProvideAnomalyFeedback' smart constructor.
data ProvideAnomalyFeedback = ProvideAnomalyFeedback'
  { -- | A cost anomaly ID.
    anomalyId :: Prelude.Text,
    -- | Describes whether the cost anomaly was a planned activity or you
    -- considered it an anomaly.
    feedback :: AnomalyFeedbackType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'feedback'
  AnomalyFeedbackType ->
  ProvideAnomalyFeedback
newProvideAnomalyFeedback pAnomalyId_ pFeedback_ =
  ProvideAnomalyFeedback'
    { anomalyId = pAnomalyId_,
      feedback = pFeedback_
    }

-- | A cost anomaly ID.
provideAnomalyFeedback_anomalyId :: Lens.Lens' ProvideAnomalyFeedback Prelude.Text
provideAnomalyFeedback_anomalyId = Lens.lens (\ProvideAnomalyFeedback' {anomalyId} -> anomalyId) (\s@ProvideAnomalyFeedback' {} a -> s {anomalyId = a} :: ProvideAnomalyFeedback)

-- | Describes whether the cost anomaly was a planned activity or you
-- considered it an anomaly.
provideAnomalyFeedback_feedback :: Lens.Lens' ProvideAnomalyFeedback AnomalyFeedbackType
provideAnomalyFeedback_feedback = Lens.lens (\ProvideAnomalyFeedback' {feedback} -> feedback) (\s@ProvideAnomalyFeedback' {} a -> s {feedback = a} :: ProvideAnomalyFeedback)

instance Prelude.AWSRequest ProvideAnomalyFeedback where
  type
    Rs ProvideAnomalyFeedback =
      ProvideAnomalyFeedbackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ProvideAnomalyFeedbackResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "AnomalyId")
      )

instance Prelude.Hashable ProvideAnomalyFeedback

instance Prelude.NFData ProvideAnomalyFeedback

instance Prelude.ToHeaders ProvideAnomalyFeedback where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSInsightsIndexService.ProvideAnomalyFeedback" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ProvideAnomalyFeedback where
  toJSON ProvideAnomalyFeedback' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AnomalyId" Prelude..= anomalyId),
            Prelude.Just ("Feedback" Prelude..= feedback)
          ]
      )

instance Prelude.ToPath ProvideAnomalyFeedback where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ProvideAnomalyFeedback where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newProvideAnomalyFeedbackResponse' smart constructor.
data ProvideAnomalyFeedbackResponse = ProvideAnomalyFeedbackResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the modified cost anomaly.
    anomalyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'anomalyId'
  Prelude.Text ->
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
provideAnomalyFeedbackResponse_httpStatus :: Lens.Lens' ProvideAnomalyFeedbackResponse Prelude.Int
provideAnomalyFeedbackResponse_httpStatus = Lens.lens (\ProvideAnomalyFeedbackResponse' {httpStatus} -> httpStatus) (\s@ProvideAnomalyFeedbackResponse' {} a -> s {httpStatus = a} :: ProvideAnomalyFeedbackResponse)

-- | The ID of the modified cost anomaly.
provideAnomalyFeedbackResponse_anomalyId :: Lens.Lens' ProvideAnomalyFeedbackResponse Prelude.Text
provideAnomalyFeedbackResponse_anomalyId = Lens.lens (\ProvideAnomalyFeedbackResponse' {anomalyId} -> anomalyId) (\s@ProvideAnomalyFeedbackResponse' {} a -> s {anomalyId = a} :: ProvideAnomalyFeedbackResponse)

instance
  Prelude.NFData
    ProvideAnomalyFeedbackResponse
