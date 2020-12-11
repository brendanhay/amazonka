{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.SendAutomationSignal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a signal to an Automation execution to change the current behavior or status of the execution.
module Network.AWS.SSM.SendAutomationSignal
  ( -- * Creating a request
    SendAutomationSignal (..),
    mkSendAutomationSignal,

    -- ** Request lenses
    sasPayload,
    sasAutomationExecutionId,
    sasSignalType,

    -- * Destructuring the response
    SendAutomationSignalResponse (..),
    mkSendAutomationSignalResponse,

    -- ** Response lenses
    sasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkSendAutomationSignal' smart constructor.
data SendAutomationSignal = SendAutomationSignal'
  { payload ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    automationExecutionId :: Lude.Text,
    signalType :: SignalType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendAutomationSignal' with the minimum fields required to make a request.
--
-- * 'automationExecutionId' - The unique identifier for an existing Automation execution that you want to send the signal to.
-- * 'payload' - The data sent with the signal. The data schema depends on the type of signal used in the request.
--
-- For @Approve@ and @Reject@ signal types, the payload is an optional comment that you can send with the signal type. For example:
-- @Comment="Looks good"@
-- For @StartStep@ and @Resume@ signal types, you must send the name of the Automation step to start or resume as the payload. For example:
-- @StepName="step1"@
-- For the @StopStep@ signal type, you must send the step execution ID as the payload. For example:
-- @StepExecutionId="97fff367-fc5a-4299-aed8-0123456789ab"@
-- * 'signalType' - The type of signal to send to an Automation execution.
mkSendAutomationSignal ::
  -- | 'automationExecutionId'
  Lude.Text ->
  -- | 'signalType'
  SignalType ->
  SendAutomationSignal
mkSendAutomationSignal pAutomationExecutionId_ pSignalType_ =
  SendAutomationSignal'
    { payload = Lude.Nothing,
      automationExecutionId = pAutomationExecutionId_,
      signalType = pSignalType_
    }

-- | The data sent with the signal. The data schema depends on the type of signal used in the request.
--
-- For @Approve@ and @Reject@ signal types, the payload is an optional comment that you can send with the signal type. For example:
-- @Comment="Looks good"@
-- For @StartStep@ and @Resume@ signal types, you must send the name of the Automation step to start or resume as the payload. For example:
-- @StepName="step1"@
-- For the @StopStep@ signal type, you must send the step execution ID as the payload. For example:
-- @StepExecutionId="97fff367-fc5a-4299-aed8-0123456789ab"@
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasPayload :: Lens.Lens' SendAutomationSignal (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
sasPayload = Lens.lens (payload :: SendAutomationSignal -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {payload = a} :: SendAutomationSignal)
{-# DEPRECATED sasPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The unique identifier for an existing Automation execution that you want to send the signal to.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasAutomationExecutionId :: Lens.Lens' SendAutomationSignal Lude.Text
sasAutomationExecutionId = Lens.lens (automationExecutionId :: SendAutomationSignal -> Lude.Text) (\s a -> s {automationExecutionId = a} :: SendAutomationSignal)
{-# DEPRECATED sasAutomationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead." #-}

-- | The type of signal to send to an Automation execution.
--
-- /Note:/ Consider using 'signalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasSignalType :: Lens.Lens' SendAutomationSignal SignalType
sasSignalType = Lens.lens (signalType :: SendAutomationSignal -> SignalType) (\s a -> s {signalType = a} :: SendAutomationSignal)
{-# DEPRECATED sasSignalType "Use generic-lens or generic-optics with 'signalType' instead." #-}

instance Lude.AWSRequest SendAutomationSignal where
  type Rs SendAutomationSignal = SendAutomationSignalResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          SendAutomationSignalResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendAutomationSignal where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.SendAutomationSignal" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SendAutomationSignal where
  toJSON SendAutomationSignal' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Payload" Lude..=) Lude.<$> payload,
            Lude.Just ("AutomationExecutionId" Lude..= automationExecutionId),
            Lude.Just ("SignalType" Lude..= signalType)
          ]
      )

instance Lude.ToPath SendAutomationSignal where
  toPath = Lude.const "/"

instance Lude.ToQuery SendAutomationSignal where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSendAutomationSignalResponse' smart constructor.
newtype SendAutomationSignalResponse = SendAutomationSignalResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendAutomationSignalResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSendAutomationSignalResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendAutomationSignalResponse
mkSendAutomationSignalResponse pResponseStatus_ =
  SendAutomationSignalResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasrsResponseStatus :: Lens.Lens' SendAutomationSignalResponse Lude.Int
sasrsResponseStatus = Lens.lens (responseStatus :: SendAutomationSignalResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendAutomationSignalResponse)
{-# DEPRECATED sasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
