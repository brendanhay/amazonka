{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.ProvideAnomalyFeedback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the feedback property of a given cost anomaly.
module Network.AWS.CostExplorer.ProvideAnomalyFeedback
  ( -- * Creating a request
    ProvideAnomalyFeedback (..),
    mkProvideAnomalyFeedback,

    -- ** Request lenses
    pafAnomalyId,
    pafFeedback,

    -- * Destructuring the response
    ProvideAnomalyFeedbackResponse (..),
    mkProvideAnomalyFeedbackResponse,

    -- ** Response lenses
    pafrsResponseStatus,
    pafrsAnomalyId,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkProvideAnomalyFeedback' smart constructor.
data ProvideAnomalyFeedback = ProvideAnomalyFeedback'
  { anomalyId ::
      Lude.Text,
    feedback :: AnomalyFeedbackType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvideAnomalyFeedback' with the minimum fields required to make a request.
--
-- * 'anomalyId' - A cost anomaly ID.
-- * 'feedback' - Describes whether the cost anomaly was a planned activity or you considered it an anomaly.
mkProvideAnomalyFeedback ::
  -- | 'anomalyId'
  Lude.Text ->
  -- | 'feedback'
  AnomalyFeedbackType ->
  ProvideAnomalyFeedback
mkProvideAnomalyFeedback pAnomalyId_ pFeedback_ =
  ProvideAnomalyFeedback'
    { anomalyId = pAnomalyId_,
      feedback = pFeedback_
    }

-- | A cost anomaly ID.
--
-- /Note:/ Consider using 'anomalyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafAnomalyId :: Lens.Lens' ProvideAnomalyFeedback Lude.Text
pafAnomalyId = Lens.lens (anomalyId :: ProvideAnomalyFeedback -> Lude.Text) (\s a -> s {anomalyId = a} :: ProvideAnomalyFeedback)
{-# DEPRECATED pafAnomalyId "Use generic-lens or generic-optics with 'anomalyId' instead." #-}

-- | Describes whether the cost anomaly was a planned activity or you considered it an anomaly.
--
-- /Note:/ Consider using 'feedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafFeedback :: Lens.Lens' ProvideAnomalyFeedback AnomalyFeedbackType
pafFeedback = Lens.lens (feedback :: ProvideAnomalyFeedback -> AnomalyFeedbackType) (\s a -> s {feedback = a} :: ProvideAnomalyFeedback)
{-# DEPRECATED pafFeedback "Use generic-lens or generic-optics with 'feedback' instead." #-}

instance Lude.AWSRequest ProvideAnomalyFeedback where
  type Rs ProvideAnomalyFeedback = ProvideAnomalyFeedbackResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ProvideAnomalyFeedbackResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "AnomalyId")
      )

instance Lude.ToHeaders ProvideAnomalyFeedback where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.ProvideAnomalyFeedback" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ProvideAnomalyFeedback where
  toJSON ProvideAnomalyFeedback' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AnomalyId" Lude..= anomalyId),
            Lude.Just ("Feedback" Lude..= feedback)
          ]
      )

instance Lude.ToPath ProvideAnomalyFeedback where
  toPath = Lude.const "/"

instance Lude.ToQuery ProvideAnomalyFeedback where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkProvideAnomalyFeedbackResponse' smart constructor.
data ProvideAnomalyFeedbackResponse = ProvideAnomalyFeedbackResponse'
  { responseStatus ::
      Lude.Int,
    anomalyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvideAnomalyFeedbackResponse' with the minimum fields required to make a request.
--
-- * 'anomalyId' - The ID of the modified cost anomaly.
-- * 'responseStatus' - The response status code.
mkProvideAnomalyFeedbackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'anomalyId'
  Lude.Text ->
  ProvideAnomalyFeedbackResponse
mkProvideAnomalyFeedbackResponse pResponseStatus_ pAnomalyId_ =
  ProvideAnomalyFeedbackResponse'
    { responseStatus =
        pResponseStatus_,
      anomalyId = pAnomalyId_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafrsResponseStatus :: Lens.Lens' ProvideAnomalyFeedbackResponse Lude.Int
pafrsResponseStatus = Lens.lens (responseStatus :: ProvideAnomalyFeedbackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ProvideAnomalyFeedbackResponse)
{-# DEPRECATED pafrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ID of the modified cost anomaly.
--
-- /Note:/ Consider using 'anomalyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pafrsAnomalyId :: Lens.Lens' ProvideAnomalyFeedbackResponse Lude.Text
pafrsAnomalyId = Lens.lens (anomalyId :: ProvideAnomalyFeedbackResponse -> Lude.Text) (\s a -> s {anomalyId = a} :: ProvideAnomalyFeedbackResponse)
{-# DEPRECATED pafrsAnomalyId "Use generic-lens or generic-optics with 'anomalyId' instead." #-}
