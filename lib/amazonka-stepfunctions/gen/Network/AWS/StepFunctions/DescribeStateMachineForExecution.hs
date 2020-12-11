{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.DescribeStateMachineForExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state machine associated with a specific execution.
--
-- This API action is not supported by @EXPRESS@ state machines.
module Network.AWS.StepFunctions.DescribeStateMachineForExecution
  ( -- * Creating a request
    DescribeStateMachineForExecution (..),
    mkDescribeStateMachineForExecution,

    -- ** Request lenses
    dsmfeExecutionARN,

    -- * Destructuring the response
    DescribeStateMachineForExecutionResponse (..),
    mkDescribeStateMachineForExecutionResponse,

    -- ** Response lenses
    dsmfersTracingConfiguration,
    dsmfersLoggingConfiguration,
    dsmfersResponseStatus,
    dsmfersStateMachineARN,
    dsmfersName,
    dsmfersDefinition,
    dsmfersRoleARN,
    dsmfersUpdateDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkDescribeStateMachineForExecution' smart constructor.
newtype DescribeStateMachineForExecution = DescribeStateMachineForExecution'
  { executionARN ::
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

-- | Creates a value of 'DescribeStateMachineForExecution' with the minimum fields required to make a request.
--
-- * 'executionARN' - The Amazon Resource Name (ARN) of the execution you want state machine information for.
mkDescribeStateMachineForExecution ::
  -- | 'executionARN'
  Lude.Text ->
  DescribeStateMachineForExecution
mkDescribeStateMachineForExecution pExecutionARN_ =
  DescribeStateMachineForExecution' {executionARN = pExecutionARN_}

-- | The Amazon Resource Name (ARN) of the execution you want state machine information for.
--
-- /Note:/ Consider using 'executionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmfeExecutionARN :: Lens.Lens' DescribeStateMachineForExecution Lude.Text
dsmfeExecutionARN = Lens.lens (executionARN :: DescribeStateMachineForExecution -> Lude.Text) (\s a -> s {executionARN = a} :: DescribeStateMachineForExecution)
{-# DEPRECATED dsmfeExecutionARN "Use generic-lens or generic-optics with 'executionARN' instead." #-}

instance Lude.AWSRequest DescribeStateMachineForExecution where
  type
    Rs DescribeStateMachineForExecution =
      DescribeStateMachineForExecutionResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStateMachineForExecutionResponse'
            Lude.<$> (x Lude..?> "tracingConfiguration")
            Lude.<*> (x Lude..?> "loggingConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "stateMachineArn")
            Lude.<*> (x Lude..:> "name")
            Lude.<*> (x Lude..:> "definition")
            Lude.<*> (x Lude..:> "roleArn")
            Lude.<*> (x Lude..:> "updateDate")
      )

instance Lude.ToHeaders DescribeStateMachineForExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSStepFunctions.DescribeStateMachineForExecution" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStateMachineForExecution where
  toJSON DescribeStateMachineForExecution' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("executionArn" Lude..= executionARN)])

instance Lude.ToPath DescribeStateMachineForExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStateMachineForExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeStateMachineForExecutionResponse' smart constructor.
data DescribeStateMachineForExecutionResponse = DescribeStateMachineForExecutionResponse'
  { tracingConfiguration ::
      Lude.Maybe
        TracingConfiguration,
    loggingConfiguration ::
      Lude.Maybe
        LoggingConfiguration,
    responseStatus ::
      Lude.Int,
    stateMachineARN ::
      Lude.Text,
    name ::
      Lude.Text,
    definition ::
      Lude.Sensitive
        Lude.Text,
    roleARN ::
      Lude.Text,
    updateDate ::
      Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStateMachineForExecutionResponse' with the minimum fields required to make a request.
--
-- * 'definition' - The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
-- * 'loggingConfiguration' - Undocumented field.
-- * 'name' - The name of the state machine associated with the execution.
-- * 'responseStatus' - The response status code.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role of the State Machine for the execution.
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) of the state machine associated with the execution.
-- * 'tracingConfiguration' - Selects whether AWS X-Ray tracing is enabled.
-- * 'updateDate' - The date and time the state machine associated with an execution was updated. For a newly created state machine, this is the creation date.
mkDescribeStateMachineForExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'stateMachineARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'definition'
  Lude.Sensitive Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'updateDate'
  Lude.Timestamp ->
  DescribeStateMachineForExecutionResponse
mkDescribeStateMachineForExecutionResponse
  pResponseStatus_
  pStateMachineARN_
  pName_
  pDefinition_
  pRoleARN_
  pUpdateDate_ =
    DescribeStateMachineForExecutionResponse'
      { tracingConfiguration =
          Lude.Nothing,
        loggingConfiguration = Lude.Nothing,
        responseStatus = pResponseStatus_,
        stateMachineARN = pStateMachineARN_,
        name = pName_,
        definition = pDefinition_,
        roleARN = pRoleARN_,
        updateDate = pUpdateDate_
      }

-- | Selects whether AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'tracingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmfersTracingConfiguration :: Lens.Lens' DescribeStateMachineForExecutionResponse (Lude.Maybe TracingConfiguration)
dsmfersTracingConfiguration = Lens.lens (tracingConfiguration :: DescribeStateMachineForExecutionResponse -> Lude.Maybe TracingConfiguration) (\s a -> s {tracingConfiguration = a} :: DescribeStateMachineForExecutionResponse)
{-# DEPRECATED dsmfersTracingConfiguration "Use generic-lens or generic-optics with 'tracingConfiguration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmfersLoggingConfiguration :: Lens.Lens' DescribeStateMachineForExecutionResponse (Lude.Maybe LoggingConfiguration)
dsmfersLoggingConfiguration = Lens.lens (loggingConfiguration :: DescribeStateMachineForExecutionResponse -> Lude.Maybe LoggingConfiguration) (\s a -> s {loggingConfiguration = a} :: DescribeStateMachineForExecutionResponse)
{-# DEPRECATED dsmfersLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmfersResponseStatus :: Lens.Lens' DescribeStateMachineForExecutionResponse Lude.Int
dsmfersResponseStatus = Lens.lens (responseStatus :: DescribeStateMachineForExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStateMachineForExecutionResponse)
{-# DEPRECATED dsmfersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the state machine associated with the execution.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmfersStateMachineARN :: Lens.Lens' DescribeStateMachineForExecutionResponse Lude.Text
dsmfersStateMachineARN = Lens.lens (stateMachineARN :: DescribeStateMachineForExecutionResponse -> Lude.Text) (\s a -> s {stateMachineARN = a} :: DescribeStateMachineForExecutionResponse)
{-# DEPRECATED dsmfersStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

-- | The name of the state machine associated with the execution.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmfersName :: Lens.Lens' DescribeStateMachineForExecutionResponse Lude.Text
dsmfersName = Lens.lens (name :: DescribeStateMachineForExecutionResponse -> Lude.Text) (\s a -> s {name = a} :: DescribeStateMachineForExecutionResponse)
{-# DEPRECATED dsmfersName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmfersDefinition :: Lens.Lens' DescribeStateMachineForExecutionResponse (Lude.Sensitive Lude.Text)
dsmfersDefinition = Lens.lens (definition :: DescribeStateMachineForExecutionResponse -> Lude.Sensitive Lude.Text) (\s a -> s {definition = a} :: DescribeStateMachineForExecutionResponse)
{-# DEPRECATED dsmfersDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role of the State Machine for the execution.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmfersRoleARN :: Lens.Lens' DescribeStateMachineForExecutionResponse Lude.Text
dsmfersRoleARN = Lens.lens (roleARN :: DescribeStateMachineForExecutionResponse -> Lude.Text) (\s a -> s {roleARN = a} :: DescribeStateMachineForExecutionResponse)
{-# DEPRECATED dsmfersRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The date and time the state machine associated with an execution was updated. For a newly created state machine, this is the creation date.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmfersUpdateDate :: Lens.Lens' DescribeStateMachineForExecutionResponse Lude.Timestamp
dsmfersUpdateDate = Lens.lens (updateDate :: DescribeStateMachineForExecutionResponse -> Lude.Timestamp) (\s a -> s {updateDate = a} :: DescribeStateMachineForExecutionResponse)
{-# DEPRECATED dsmfersUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}
