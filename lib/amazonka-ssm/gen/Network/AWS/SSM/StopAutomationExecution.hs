{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.StopAutomationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop an Automation that is currently running.
module Network.AWS.SSM.StopAutomationExecution
  ( -- * Creating a request
    StopAutomationExecution (..),
    mkStopAutomationExecution,

    -- ** Request lenses
    saeType,
    saeAutomationExecutionId,

    -- * Destructuring the response
    StopAutomationExecutionResponse (..),
    mkStopAutomationExecutionResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkStopAutomationExecution' smart constructor.
data StopAutomationExecution = StopAutomationExecution'
  { -- | The stop request type. Valid types include the following: Cancel and Complete. The default type is Cancel.
    type' :: Lude.Maybe StopType,
    -- | The execution ID of the Automation to stop.
    automationExecutionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopAutomationExecution' with the minimum fields required to make a request.
--
-- * 'type'' - The stop request type. Valid types include the following: Cancel and Complete. The default type is Cancel.
-- * 'automationExecutionId' - The execution ID of the Automation to stop.
mkStopAutomationExecution ::
  -- | 'automationExecutionId'
  Lude.Text ->
  StopAutomationExecution
mkStopAutomationExecution pAutomationExecutionId_ =
  StopAutomationExecution'
    { type' = Lude.Nothing,
      automationExecutionId = pAutomationExecutionId_
    }

-- | The stop request type. Valid types include the following: Cancel and Complete. The default type is Cancel.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeType :: Lens.Lens' StopAutomationExecution (Lude.Maybe StopType)
saeType = Lens.lens (type' :: StopAutomationExecution -> Lude.Maybe StopType) (\s a -> s {type' = a} :: StopAutomationExecution)
{-# DEPRECATED saeType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The execution ID of the Automation to stop.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeAutomationExecutionId :: Lens.Lens' StopAutomationExecution Lude.Text
saeAutomationExecutionId = Lens.lens (automationExecutionId :: StopAutomationExecution -> Lude.Text) (\s a -> s {automationExecutionId = a} :: StopAutomationExecution)
{-# DEPRECATED saeAutomationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead." #-}

instance Lude.AWSRequest StopAutomationExecution where
  type Rs StopAutomationExecution = StopAutomationExecutionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopAutomationExecutionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopAutomationExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.StopAutomationExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopAutomationExecution where
  toJSON StopAutomationExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Type" Lude..=) Lude.<$> type',
            Lude.Just ("AutomationExecutionId" Lude..= automationExecutionId)
          ]
      )

instance Lude.ToPath StopAutomationExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StopAutomationExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopAutomationExecutionResponse' smart constructor.
newtype StopAutomationExecutionResponse = StopAutomationExecutionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopAutomationExecutionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopAutomationExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopAutomationExecutionResponse
mkStopAutomationExecutionResponse pResponseStatus_ =
  StopAutomationExecutionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopAutomationExecutionResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopAutomationExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopAutomationExecutionResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
