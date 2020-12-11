{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.DescribeStateMachine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a state machine.
module Network.AWS.StepFunctions.DescribeStateMachine
  ( -- * Creating a request
    DescribeStateMachine (..),
    mkDescribeStateMachine,

    -- ** Request lenses
    dsmStateMachineARN,

    -- * Destructuring the response
    DescribeStateMachineResponse (..),
    mkDescribeStateMachineResponse,

    -- ** Response lenses
    dsmrsStatus,
    dsmrsTracingConfiguration,
    dsmrsLoggingConfiguration,
    dsmrsResponseStatus,
    dsmrsStateMachineARN,
    dsmrsName,
    dsmrsDefinition,
    dsmrsRoleARN,
    dsmrsType,
    dsmrsCreationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkDescribeStateMachine' smart constructor.
newtype DescribeStateMachine = DescribeStateMachine'
  { stateMachineARN ::
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

-- | Creates a value of 'DescribeStateMachine' with the minimum fields required to make a request.
--
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) of the state machine to describe.
mkDescribeStateMachine ::
  -- | 'stateMachineARN'
  Lude.Text ->
  DescribeStateMachine
mkDescribeStateMachine pStateMachineARN_ =
  DescribeStateMachine' {stateMachineARN = pStateMachineARN_}

-- | The Amazon Resource Name (ARN) of the state machine to describe.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmStateMachineARN :: Lens.Lens' DescribeStateMachine Lude.Text
dsmStateMachineARN = Lens.lens (stateMachineARN :: DescribeStateMachine -> Lude.Text) (\s a -> s {stateMachineARN = a} :: DescribeStateMachine)
{-# DEPRECATED dsmStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

instance Lude.AWSRequest DescribeStateMachine where
  type Rs DescribeStateMachine = DescribeStateMachineResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStateMachineResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "tracingConfiguration")
            Lude.<*> (x Lude..?> "loggingConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "stateMachineArn")
            Lude.<*> (x Lude..:> "name")
            Lude.<*> (x Lude..:> "definition")
            Lude.<*> (x Lude..:> "roleArn")
            Lude.<*> (x Lude..:> "type")
            Lude.<*> (x Lude..:> "creationDate")
      )

instance Lude.ToHeaders DescribeStateMachine where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.DescribeStateMachine" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStateMachine where
  toJSON DescribeStateMachine' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("stateMachineArn" Lude..= stateMachineARN)]
      )

instance Lude.ToPath DescribeStateMachine where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStateMachine where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeStateMachineResponse' smart constructor.
data DescribeStateMachineResponse = DescribeStateMachineResponse'
  { status ::
      Lude.Maybe StateMachineStatus,
    tracingConfiguration ::
      Lude.Maybe TracingConfiguration,
    loggingConfiguration ::
      Lude.Maybe LoggingConfiguration,
    responseStatus :: Lude.Int,
    stateMachineARN :: Lude.Text,
    name :: Lude.Text,
    definition ::
      Lude.Sensitive Lude.Text,
    roleARN :: Lude.Text,
    type' :: StateMachineType,
    creationDate :: Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStateMachineResponse' with the minimum fields required to make a request.
--
-- * 'creationDate' - The date the state machine is created.
-- * 'definition' - The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
-- * 'loggingConfiguration' - Undocumented field.
-- * 'name' - The name of the state machine.
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
-- * 'responseStatus' - The response status code.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role used when creating this state machine. (The IAM role maintains security by granting Step Functions access to AWS resources.)
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) that identifies the state machine.
-- * 'status' - The current status of the state machine.
-- * 'tracingConfiguration' - Selects whether AWS X-Ray tracing is enabled.
-- * 'type'' - The @type@ of the state machine (@STANDARD@ or @EXPRESS@ ).
mkDescribeStateMachineResponse ::
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
  -- | 'type''
  StateMachineType ->
  -- | 'creationDate'
  Lude.Timestamp ->
  DescribeStateMachineResponse
mkDescribeStateMachineResponse
  pResponseStatus_
  pStateMachineARN_
  pName_
  pDefinition_
  pRoleARN_
  pType_
  pCreationDate_ =
    DescribeStateMachineResponse'
      { status = Lude.Nothing,
        tracingConfiguration = Lude.Nothing,
        loggingConfiguration = Lude.Nothing,
        responseStatus = pResponseStatus_,
        stateMachineARN = pStateMachineARN_,
        name = pName_,
        definition = pDefinition_,
        roleARN = pRoleARN_,
        type' = pType_,
        creationDate = pCreationDate_
      }

-- | The current status of the state machine.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrsStatus :: Lens.Lens' DescribeStateMachineResponse (Lude.Maybe StateMachineStatus)
dsmrsStatus = Lens.lens (status :: DescribeStateMachineResponse -> Lude.Maybe StateMachineStatus) (\s a -> s {status = a} :: DescribeStateMachineResponse)
{-# DEPRECATED dsmrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Selects whether AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'tracingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrsTracingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Lude.Maybe TracingConfiguration)
dsmrsTracingConfiguration = Lens.lens (tracingConfiguration :: DescribeStateMachineResponse -> Lude.Maybe TracingConfiguration) (\s a -> s {tracingConfiguration = a} :: DescribeStateMachineResponse)
{-# DEPRECATED dsmrsTracingConfiguration "Use generic-lens or generic-optics with 'tracingConfiguration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrsLoggingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Lude.Maybe LoggingConfiguration)
dsmrsLoggingConfiguration = Lens.lens (loggingConfiguration :: DescribeStateMachineResponse -> Lude.Maybe LoggingConfiguration) (\s a -> s {loggingConfiguration = a} :: DescribeStateMachineResponse)
{-# DEPRECATED dsmrsLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrsResponseStatus :: Lens.Lens' DescribeStateMachineResponse Lude.Int
dsmrsResponseStatus = Lens.lens (responseStatus :: DescribeStateMachineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStateMachineResponse)
{-# DEPRECATED dsmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the state machine.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrsStateMachineARN :: Lens.Lens' DescribeStateMachineResponse Lude.Text
dsmrsStateMachineARN = Lens.lens (stateMachineARN :: DescribeStateMachineResponse -> Lude.Text) (\s a -> s {stateMachineARN = a} :: DescribeStateMachineResponse)
{-# DEPRECATED dsmrsStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

-- | The name of the state machine.
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrsName :: Lens.Lens' DescribeStateMachineResponse Lude.Text
dsmrsName = Lens.lens (name :: DescribeStateMachineResponse -> Lude.Text) (\s a -> s {name = a} :: DescribeStateMachineResponse)
{-# DEPRECATED dsmrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrsDefinition :: Lens.Lens' DescribeStateMachineResponse (Lude.Sensitive Lude.Text)
dsmrsDefinition = Lens.lens (definition :: DescribeStateMachineResponse -> Lude.Sensitive Lude.Text) (\s a -> s {definition = a} :: DescribeStateMachineResponse)
{-# DEPRECATED dsmrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role used when creating this state machine. (The IAM role maintains security by granting Step Functions access to AWS resources.)
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrsRoleARN :: Lens.Lens' DescribeStateMachineResponse Lude.Text
dsmrsRoleARN = Lens.lens (roleARN :: DescribeStateMachineResponse -> Lude.Text) (\s a -> s {roleARN = a} :: DescribeStateMachineResponse)
{-# DEPRECATED dsmrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The @type@ of the state machine (@STANDARD@ or @EXPRESS@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrsType :: Lens.Lens' DescribeStateMachineResponse StateMachineType
dsmrsType = Lens.lens (type' :: DescribeStateMachineResponse -> StateMachineType) (\s a -> s {type' = a} :: DescribeStateMachineResponse)
{-# DEPRECATED dsmrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date the state machine is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrsCreationDate :: Lens.Lens' DescribeStateMachineResponse Lude.Timestamp
dsmrsCreationDate = Lens.lens (creationDate :: DescribeStateMachineResponse -> Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeStateMachineResponse)
{-# DEPRECATED dsmrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}
