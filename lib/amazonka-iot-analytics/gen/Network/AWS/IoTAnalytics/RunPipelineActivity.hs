{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.RunPipelineActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulates the results of running a pipeline activity on a message payload.
module Network.AWS.IoTAnalytics.RunPipelineActivity
  ( -- * Creating a request
    RunPipelineActivity (..),
    mkRunPipelineActivity,

    -- ** Request lenses
    rpaPipelineActivity,
    rpaPayloads,

    -- * Destructuring the response
    RunPipelineActivityResponse (..),
    mkRunPipelineActivityResponse,

    -- ** Response lenses
    rparsLogResult,
    rparsPayloads,
    rparsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRunPipelineActivity' smart constructor.
data RunPipelineActivity = RunPipelineActivity'
  { -- | The pipeline activity that is run. This must not be a channel activity or a datastore activity because these activities are used in a pipeline only to load the original message and to store the (possibly) transformed message. If a lambda activity is specified, only short-running Lambda functions (those with a timeout of less than 30 seconds or less) can be used.
    pipelineActivity :: PipelineActivity,
    -- | The sample message payloads on which the pipeline activity is run.
    payloads :: Lude.NonEmpty Lude.Base64
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RunPipelineActivity' with the minimum fields required to make a request.
--
-- * 'pipelineActivity' - The pipeline activity that is run. This must not be a channel activity or a datastore activity because these activities are used in a pipeline only to load the original message and to store the (possibly) transformed message. If a lambda activity is specified, only short-running Lambda functions (those with a timeout of less than 30 seconds or less) can be used.
-- * 'payloads' - The sample message payloads on which the pipeline activity is run.
mkRunPipelineActivity ::
  -- | 'pipelineActivity'
  PipelineActivity ->
  -- | 'payloads'
  Lude.NonEmpty Lude.Base64 ->
  RunPipelineActivity
mkRunPipelineActivity pPipelineActivity_ pPayloads_ =
  RunPipelineActivity'
    { pipelineActivity = pPipelineActivity_,
      payloads = pPayloads_
    }

-- | The pipeline activity that is run. This must not be a channel activity or a datastore activity because these activities are used in a pipeline only to load the original message and to store the (possibly) transformed message. If a lambda activity is specified, only short-running Lambda functions (those with a timeout of less than 30 seconds or less) can be used.
--
-- /Note:/ Consider using 'pipelineActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpaPipelineActivity :: Lens.Lens' RunPipelineActivity PipelineActivity
rpaPipelineActivity = Lens.lens (pipelineActivity :: RunPipelineActivity -> PipelineActivity) (\s a -> s {pipelineActivity = a} :: RunPipelineActivity)
{-# DEPRECATED rpaPipelineActivity "Use generic-lens or generic-optics with 'pipelineActivity' instead." #-}

-- | The sample message payloads on which the pipeline activity is run.
--
-- /Note:/ Consider using 'payloads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpaPayloads :: Lens.Lens' RunPipelineActivity (Lude.NonEmpty Lude.Base64)
rpaPayloads = Lens.lens (payloads :: RunPipelineActivity -> Lude.NonEmpty Lude.Base64) (\s a -> s {payloads = a} :: RunPipelineActivity)
{-# DEPRECATED rpaPayloads "Use generic-lens or generic-optics with 'payloads' instead." #-}

instance Lude.AWSRequest RunPipelineActivity where
  type Rs RunPipelineActivity = RunPipelineActivityResponse
  request = Req.postJSON ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          RunPipelineActivityResponse'
            Lude.<$> (x Lude..?> "logResult")
            Lude.<*> (x Lude..?> "payloads")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RunPipelineActivity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON RunPipelineActivity where
  toJSON RunPipelineActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineActivity" Lude..= pipelineActivity),
            Lude.Just ("payloads" Lude..= payloads)
          ]
      )

instance Lude.ToPath RunPipelineActivity where
  toPath = Lude.const "/pipelineactivities/run"

instance Lude.ToQuery RunPipelineActivity where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRunPipelineActivityResponse' smart constructor.
data RunPipelineActivityResponse = RunPipelineActivityResponse'
  { -- | In case the pipeline activity fails, the log message that is generated.
    logResult :: Lude.Maybe Lude.Text,
    -- | The enriched or transformed sample message payloads as base64-encoded strings. (The results of running the pipeline activity on each input sample message payload, encoded in base64.)
    payloads :: Lude.Maybe (Lude.NonEmpty Lude.Base64),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RunPipelineActivityResponse' with the minimum fields required to make a request.
--
-- * 'logResult' - In case the pipeline activity fails, the log message that is generated.
-- * 'payloads' - The enriched or transformed sample message payloads as base64-encoded strings. (The results of running the pipeline activity on each input sample message payload, encoded in base64.)
-- * 'responseStatus' - The response status code.
mkRunPipelineActivityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RunPipelineActivityResponse
mkRunPipelineActivityResponse pResponseStatus_ =
  RunPipelineActivityResponse'
    { logResult = Lude.Nothing,
      payloads = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | In case the pipeline activity fails, the log message that is generated.
--
-- /Note:/ Consider using 'logResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rparsLogResult :: Lens.Lens' RunPipelineActivityResponse (Lude.Maybe Lude.Text)
rparsLogResult = Lens.lens (logResult :: RunPipelineActivityResponse -> Lude.Maybe Lude.Text) (\s a -> s {logResult = a} :: RunPipelineActivityResponse)
{-# DEPRECATED rparsLogResult "Use generic-lens or generic-optics with 'logResult' instead." #-}

-- | The enriched or transformed sample message payloads as base64-encoded strings. (The results of running the pipeline activity on each input sample message payload, encoded in base64.)
--
-- /Note:/ Consider using 'payloads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rparsPayloads :: Lens.Lens' RunPipelineActivityResponse (Lude.Maybe (Lude.NonEmpty Lude.Base64))
rparsPayloads = Lens.lens (payloads :: RunPipelineActivityResponse -> Lude.Maybe (Lude.NonEmpty Lude.Base64)) (\s a -> s {payloads = a} :: RunPipelineActivityResponse)
{-# DEPRECATED rparsPayloads "Use generic-lens or generic-optics with 'payloads' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rparsResponseStatus :: Lens.Lens' RunPipelineActivityResponse Lude.Int
rparsResponseStatus = Lens.lens (responseStatus :: RunPipelineActivityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RunPipelineActivityResponse)
{-# DEPRECATED rparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
