{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.UpdateStateMachine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing state machine by modifying its @definition@ , @roleArn@ , or @loggingConfiguration@ . Running executions will continue to use the previous @definition@ and @roleArn@ . You must include at least one of @definition@ or @roleArn@ or you will receive a @MissingRequiredParameter@ error.
module Network.AWS.StepFunctions.UpdateStateMachine
  ( -- * Creating a request
    UpdateStateMachine (..),
    mkUpdateStateMachine,

    -- ** Request lenses
    usmDefinition,
    usmTracingConfiguration,
    usmLoggingConfiguration,
    usmRoleARN,
    usmStateMachineARN,

    -- * Destructuring the response
    UpdateStateMachineResponse (..),
    mkUpdateStateMachineResponse,

    -- ** Response lenses
    usmrsResponseStatus,
    usmrsUpdateDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkUpdateStateMachine' smart constructor.
data UpdateStateMachine = UpdateStateMachine'
  { definition ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    tracingConfiguration ::
      Lude.Maybe TracingConfiguration,
    loggingConfiguration ::
      Lude.Maybe LoggingConfiguration,
    roleARN :: Lude.Maybe Lude.Text,
    stateMachineARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStateMachine' with the minimum fields required to make a request.
--
-- * 'definition' - The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
-- * 'loggingConfiguration' - The @LoggingConfiguration@ data type is used to set CloudWatch Logs options.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role of the state machine.
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) of the state machine.
-- * 'tracingConfiguration' - Selects whether AWS X-Ray tracing is enabled.
mkUpdateStateMachine ::
  -- | 'stateMachineARN'
  Lude.Text ->
  UpdateStateMachine
mkUpdateStateMachine pStateMachineARN_ =
  UpdateStateMachine'
    { definition = Lude.Nothing,
      tracingConfiguration = Lude.Nothing,
      loggingConfiguration = Lude.Nothing,
      roleARN = Lude.Nothing,
      stateMachineARN = pStateMachineARN_
    }

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmDefinition :: Lens.Lens' UpdateStateMachine (Lude.Maybe (Lude.Sensitive Lude.Text))
usmDefinition = Lens.lens (definition :: UpdateStateMachine -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {definition = a} :: UpdateStateMachine)
{-# DEPRECATED usmDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | Selects whether AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'tracingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmTracingConfiguration :: Lens.Lens' UpdateStateMachine (Lude.Maybe TracingConfiguration)
usmTracingConfiguration = Lens.lens (tracingConfiguration :: UpdateStateMachine -> Lude.Maybe TracingConfiguration) (\s a -> s {tracingConfiguration = a} :: UpdateStateMachine)
{-# DEPRECATED usmTracingConfiguration "Use generic-lens or generic-optics with 'tracingConfiguration' instead." #-}

-- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs options.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmLoggingConfiguration :: Lens.Lens' UpdateStateMachine (Lude.Maybe LoggingConfiguration)
usmLoggingConfiguration = Lens.lens (loggingConfiguration :: UpdateStateMachine -> Lude.Maybe LoggingConfiguration) (\s a -> s {loggingConfiguration = a} :: UpdateStateMachine)
{-# DEPRECATED usmLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role of the state machine.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmRoleARN :: Lens.Lens' UpdateStateMachine (Lude.Maybe Lude.Text)
usmRoleARN = Lens.lens (roleARN :: UpdateStateMachine -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateStateMachine)
{-# DEPRECATED usmRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the state machine.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmStateMachineARN :: Lens.Lens' UpdateStateMachine Lude.Text
usmStateMachineARN = Lens.lens (stateMachineARN :: UpdateStateMachine -> Lude.Text) (\s a -> s {stateMachineARN = a} :: UpdateStateMachine)
{-# DEPRECATED usmStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

instance Lude.AWSRequest UpdateStateMachine where
  type Rs UpdateStateMachine = UpdateStateMachineResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateStateMachineResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "updateDate")
      )

instance Lude.ToHeaders UpdateStateMachine where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.UpdateStateMachine" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateStateMachine where
  toJSON UpdateStateMachine' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("definition" Lude..=) Lude.<$> definition,
            ("tracingConfiguration" Lude..=) Lude.<$> tracingConfiguration,
            ("loggingConfiguration" Lude..=) Lude.<$> loggingConfiguration,
            ("roleArn" Lude..=) Lude.<$> roleARN,
            Lude.Just ("stateMachineArn" Lude..= stateMachineARN)
          ]
      )

instance Lude.ToPath UpdateStateMachine where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateStateMachine where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateStateMachineResponse' smart constructor.
data UpdateStateMachineResponse = UpdateStateMachineResponse'
  { responseStatus ::
      Lude.Int,
    updateDate :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStateMachineResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'updateDate' - The date and time the state machine was updated.
mkUpdateStateMachineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'updateDate'
  Lude.Timestamp ->
  UpdateStateMachineResponse
mkUpdateStateMachineResponse pResponseStatus_ pUpdateDate_ =
  UpdateStateMachineResponse'
    { responseStatus = pResponseStatus_,
      updateDate = pUpdateDate_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmrsResponseStatus :: Lens.Lens' UpdateStateMachineResponse Lude.Int
usmrsResponseStatus = Lens.lens (responseStatus :: UpdateStateMachineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateStateMachineResponse)
{-# DEPRECATED usmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The date and time the state machine was updated.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmrsUpdateDate :: Lens.Lens' UpdateStateMachineResponse Lude.Timestamp
usmrsUpdateDate = Lens.lens (updateDate :: UpdateStateMachineResponse -> Lude.Timestamp) (\s a -> s {updateDate = a} :: UpdateStateMachineResponse)
{-# DEPRECATED usmrsUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}
