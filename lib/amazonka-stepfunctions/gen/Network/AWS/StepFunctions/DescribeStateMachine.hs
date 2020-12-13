{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dStateMachineARN,

    -- * Destructuring the response
    DescribeStateMachineResponse (..),
    mkDescribeStateMachineResponse,

    -- ** Response lenses
    drsStatus,
    drsDefinition,
    drsTracingConfiguration,
    drsName,
    drsStateMachineARN,
    drsCreationDate,
    drsType,
    drsLoggingConfiguration,
    drsRoleARN,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkDescribeStateMachine' smart constructor.
newtype DescribeStateMachine = DescribeStateMachine'
  { -- | The Amazon Resource Name (ARN) of the state machine to describe.
    stateMachineARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
dStateMachineARN :: Lens.Lens' DescribeStateMachine Lude.Text
dStateMachineARN = Lens.lens (stateMachineARN :: DescribeStateMachine -> Lude.Text) (\s a -> s {stateMachineARN = a} :: DescribeStateMachine)
{-# DEPRECATED dStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

instance Lude.AWSRequest DescribeStateMachine where
  type Rs DescribeStateMachine = DescribeStateMachineResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStateMachineResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..:> "definition")
            Lude.<*> (x Lude..?> "tracingConfiguration")
            Lude.<*> (x Lude..:> "name")
            Lude.<*> (x Lude..:> "stateMachineArn")
            Lude.<*> (x Lude..:> "creationDate")
            Lude.<*> (x Lude..:> "type")
            Lude.<*> (x Lude..?> "loggingConfiguration")
            Lude.<*> (x Lude..:> "roleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | The current status of the state machine.
    status :: Lude.Maybe StateMachineStatus,
    -- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
    definition :: Lude.Sensitive Lude.Text,
    -- | Selects whether AWS X-Ray tracing is enabled.
    tracingConfiguration :: Lude.Maybe TracingConfiguration,
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
    name :: Lude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the state machine.
    stateMachineARN :: Lude.Text,
    -- | The date the state machine is created.
    creationDate :: Lude.Timestamp,
    -- | The @type@ of the state machine (@STANDARD@ or @EXPRESS@ ).
    type' :: StateMachineType,
    loggingConfiguration :: Lude.Maybe LoggingConfiguration,
    -- | The Amazon Resource Name (ARN) of the IAM role used when creating this state machine. (The IAM role maintains security by granting Step Functions access to AWS resources.)
    roleARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStateMachineResponse' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the state machine.
-- * 'definition' - The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
-- * 'tracingConfiguration' - Selects whether AWS X-Ray tracing is enabled.
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
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) that identifies the state machine.
-- * 'creationDate' - The date the state machine is created.
-- * 'type'' - The @type@ of the state machine (@STANDARD@ or @EXPRESS@ ).
-- * 'loggingConfiguration' -
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role used when creating this state machine. (The IAM role maintains security by granting Step Functions access to AWS resources.)
-- * 'responseStatus' - The response status code.
mkDescribeStateMachineResponse ::
  -- | 'definition'
  Lude.Sensitive Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'stateMachineARN'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Timestamp ->
  -- | 'type''
  StateMachineType ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStateMachineResponse
mkDescribeStateMachineResponse
  pDefinition_
  pName_
  pStateMachineARN_
  pCreationDate_
  pType_
  pRoleARN_
  pResponseStatus_ =
    DescribeStateMachineResponse'
      { status = Lude.Nothing,
        definition = pDefinition_,
        tracingConfiguration = Lude.Nothing,
        name = pName_,
        stateMachineARN = pStateMachineARN_,
        creationDate = pCreationDate_,
        type' = pType_,
        loggingConfiguration = Lude.Nothing,
        roleARN = pRoleARN_,
        responseStatus = pResponseStatus_
      }

-- | The current status of the state machine.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStatus :: Lens.Lens' DescribeStateMachineResponse (Lude.Maybe StateMachineStatus)
drsStatus = Lens.lens (status :: DescribeStateMachineResponse -> Lude.Maybe StateMachineStatus) (\s a -> s {status = a} :: DescribeStateMachineResponse)
{-# DEPRECATED drsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDefinition :: Lens.Lens' DescribeStateMachineResponse (Lude.Sensitive Lude.Text)
drsDefinition = Lens.lens (definition :: DescribeStateMachineResponse -> Lude.Sensitive Lude.Text) (\s a -> s {definition = a} :: DescribeStateMachineResponse)
{-# DEPRECATED drsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | Selects whether AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'tracingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTracingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Lude.Maybe TracingConfiguration)
drsTracingConfiguration = Lens.lens (tracingConfiguration :: DescribeStateMachineResponse -> Lude.Maybe TracingConfiguration) (\s a -> s {tracingConfiguration = a} :: DescribeStateMachineResponse)
{-# DEPRECATED drsTracingConfiguration "Use generic-lens or generic-optics with 'tracingConfiguration' instead." #-}

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
drsName :: Lens.Lens' DescribeStateMachineResponse Lude.Text
drsName = Lens.lens (name :: DescribeStateMachineResponse -> Lude.Text) (\s a -> s {name = a} :: DescribeStateMachineResponse)
{-# DEPRECATED drsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the state machine.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStateMachineARN :: Lens.Lens' DescribeStateMachineResponse Lude.Text
drsStateMachineARN = Lens.lens (stateMachineARN :: DescribeStateMachineResponse -> Lude.Text) (\s a -> s {stateMachineARN = a} :: DescribeStateMachineResponse)
{-# DEPRECATED drsStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

-- | The date the state machine is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCreationDate :: Lens.Lens' DescribeStateMachineResponse Lude.Timestamp
drsCreationDate = Lens.lens (creationDate :: DescribeStateMachineResponse -> Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeStateMachineResponse)
{-# DEPRECATED drsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The @type@ of the state machine (@STANDARD@ or @EXPRESS@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsType :: Lens.Lens' DescribeStateMachineResponse StateMachineType
drsType = Lens.lens (type' :: DescribeStateMachineResponse -> StateMachineType) (\s a -> s {type' = a} :: DescribeStateMachineResponse)
{-# DEPRECATED drsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLoggingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Lude.Maybe LoggingConfiguration)
drsLoggingConfiguration = Lens.lens (loggingConfiguration :: DescribeStateMachineResponse -> Lude.Maybe LoggingConfiguration) (\s a -> s {loggingConfiguration = a} :: DescribeStateMachineResponse)
{-# DEPRECATED drsLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role used when creating this state machine. (The IAM role maintains security by granting Step Functions access to AWS resources.)
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRoleARN :: Lens.Lens' DescribeStateMachineResponse Lude.Text
drsRoleARN = Lens.lens (roleARN :: DescribeStateMachineResponse -> Lude.Text) (\s a -> s {roleARN = a} :: DescribeStateMachineResponse)
{-# DEPRECATED drsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeStateMachineResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeStateMachineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStateMachineResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
