{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.StopExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an execution.
--
-- This API action is not supported by @EXPRESS@ state machines.
module Network.AWS.StepFunctions.StopExecution
  ( -- * Creating a request
    StopExecution (..),
    mkStopExecution,

    -- ** Request lenses
    seError,
    seCause,
    seExecutionARN,

    -- * Destructuring the response
    StopExecutionResponse (..),
    mkStopExecutionResponse,

    -- ** Response lenses
    sersResponseStatus,
    sersStopDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkStopExecution' smart constructor.
data StopExecution = StopExecution'
  { error ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    cause :: Lude.Maybe (Lude.Sensitive Lude.Text),
    executionARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopExecution' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'error' - The error code of the failure.
-- * 'executionARN' - The Amazon Resource Name (ARN) of the execution to stop.
mkStopExecution ::
  -- | 'executionARN'
  Lude.Text ->
  StopExecution
mkStopExecution pExecutionARN_ =
  StopExecution'
    { error = Lude.Nothing,
      cause = Lude.Nothing,
      executionARN = pExecutionARN_
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seError :: Lens.Lens' StopExecution (Lude.Maybe (Lude.Sensitive Lude.Text))
seError = Lens.lens (error :: StopExecution -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: StopExecution)
{-# DEPRECATED seError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seCause :: Lens.Lens' StopExecution (Lude.Maybe (Lude.Sensitive Lude.Text))
seCause = Lens.lens (cause :: StopExecution -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: StopExecution)
{-# DEPRECATED seCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The Amazon Resource Name (ARN) of the execution to stop.
--
-- /Note:/ Consider using 'executionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seExecutionARN :: Lens.Lens' StopExecution Lude.Text
seExecutionARN = Lens.lens (executionARN :: StopExecution -> Lude.Text) (\s a -> s {executionARN = a} :: StopExecution)
{-# DEPRECATED seExecutionARN "Use generic-lens or generic-optics with 'executionARN' instead." #-}

instance Lude.AWSRequest StopExecution where
  type Rs StopExecution = StopExecutionResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopExecutionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "stopDate")
      )

instance Lude.ToHeaders StopExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.StopExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopExecution where
  toJSON StopExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("error" Lude..=) Lude.<$> error,
            ("cause" Lude..=) Lude.<$> cause,
            Lude.Just ("executionArn" Lude..= executionARN)
          ]
      )

instance Lude.ToPath StopExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StopExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopExecutionResponse' smart constructor.
data StopExecutionResponse = StopExecutionResponse'
  { responseStatus ::
      Lude.Int,
    stopDate :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopExecutionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'stopDate' - The date the execution is stopped.
mkStopExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'stopDate'
  Lude.Timestamp ->
  StopExecutionResponse
mkStopExecutionResponse pResponseStatus_ pStopDate_ =
  StopExecutionResponse'
    { responseStatus = pResponseStatus_,
      stopDate = pStopDate_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sersResponseStatus :: Lens.Lens' StopExecutionResponse Lude.Int
sersResponseStatus = Lens.lens (responseStatus :: StopExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopExecutionResponse)
{-# DEPRECATED sersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The date the execution is stopped.
--
-- /Note:/ Consider using 'stopDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sersStopDate :: Lens.Lens' StopExecutionResponse Lude.Timestamp
sersStopDate = Lens.lens (stopDate :: StopExecutionResponse -> Lude.Timestamp) (\s a -> s {stopDate = a} :: StopExecutionResponse)
{-# DEPRECATED sersStopDate "Use generic-lens or generic-optics with 'stopDate' instead." #-}
