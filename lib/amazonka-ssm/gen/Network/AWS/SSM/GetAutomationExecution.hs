{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetAutomationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detailed information about a particular Automation execution.
module Network.AWS.SSM.GetAutomationExecution
  ( -- * Creating a request
    GetAutomationExecution (..),
    mkGetAutomationExecution,

    -- ** Request lenses
    gaeAutomationExecutionId,

    -- * Destructuring the response
    GetAutomationExecutionResponse (..),
    mkGetAutomationExecutionResponse,

    -- ** Response lenses
    gaersAutomationExecution,
    gaersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetAutomationExecution' smart constructor.
newtype GetAutomationExecution = GetAutomationExecution'
  { automationExecutionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAutomationExecution' with the minimum fields required to make a request.
--
-- * 'automationExecutionId' - The unique identifier for an existing automation execution to examine. The execution ID is returned by StartAutomationExecution when the execution of an Automation document is initiated.
mkGetAutomationExecution ::
  -- | 'automationExecutionId'
  Lude.Text ->
  GetAutomationExecution
mkGetAutomationExecution pAutomationExecutionId_ =
  GetAutomationExecution'
    { automationExecutionId =
        pAutomationExecutionId_
    }

-- | The unique identifier for an existing automation execution to examine. The execution ID is returned by StartAutomationExecution when the execution of an Automation document is initiated.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaeAutomationExecutionId :: Lens.Lens' GetAutomationExecution Lude.Text
gaeAutomationExecutionId = Lens.lens (automationExecutionId :: GetAutomationExecution -> Lude.Text) (\s a -> s {automationExecutionId = a} :: GetAutomationExecution)
{-# DEPRECATED gaeAutomationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead." #-}

instance Lude.AWSRequest GetAutomationExecution where
  type Rs GetAutomationExecution = GetAutomationExecutionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAutomationExecutionResponse'
            Lude.<$> (x Lude..?> "AutomationExecution")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAutomationExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetAutomationExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAutomationExecution where
  toJSON GetAutomationExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("AutomationExecutionId" Lude..= automationExecutionId)
          ]
      )

instance Lude.ToPath GetAutomationExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAutomationExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAutomationExecutionResponse' smart constructor.
data GetAutomationExecutionResponse = GetAutomationExecutionResponse'
  { automationExecution ::
      Lude.Maybe
        AutomationExecution,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAutomationExecutionResponse' with the minimum fields required to make a request.
--
-- * 'automationExecution' - Detailed information about the current state of an automation execution.
-- * 'responseStatus' - The response status code.
mkGetAutomationExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAutomationExecutionResponse
mkGetAutomationExecutionResponse pResponseStatus_ =
  GetAutomationExecutionResponse'
    { automationExecution =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Detailed information about the current state of an automation execution.
--
-- /Note:/ Consider using 'automationExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaersAutomationExecution :: Lens.Lens' GetAutomationExecutionResponse (Lude.Maybe AutomationExecution)
gaersAutomationExecution = Lens.lens (automationExecution :: GetAutomationExecutionResponse -> Lude.Maybe AutomationExecution) (\s a -> s {automationExecution = a} :: GetAutomationExecutionResponse)
{-# DEPRECATED gaersAutomationExecution "Use generic-lens or generic-optics with 'automationExecution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaersResponseStatus :: Lens.Lens' GetAutomationExecutionResponse Lude.Int
gaersResponseStatus = Lens.lens (responseStatus :: GetAutomationExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAutomationExecutionResponse)
{-# DEPRECATED gaersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
