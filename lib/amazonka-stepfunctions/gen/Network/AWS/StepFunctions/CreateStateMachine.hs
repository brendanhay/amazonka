{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.CreateStateMachine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a state machine. A state machine consists of a collection of states that can do work (@Task@ states), determine to which states to transition next (@Choice@ states), stop an execution with an error (@Fail@ states), and so on. State machines are specified using a JSON-based, structured language. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> in the AWS Step Functions User Guide.
module Network.AWS.StepFunctions.CreateStateMachine
  ( -- * Creating a request
    CreateStateMachine (..),
    mkCreateStateMachine,

    -- ** Request lenses
    csmDefinition,
    csmTracingConfiguration,
    csmName,
    csmType,
    csmLoggingConfiguration,
    csmTags,
    csmRoleARN,

    -- * Destructuring the response
    CreateStateMachineResponse (..),
    mkCreateStateMachineResponse,

    -- ** Response lenses
    csmrsStateMachineARN,
    csmrsCreationDate,
    csmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkCreateStateMachine' smart constructor.
data CreateStateMachine = CreateStateMachine'
  { -- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
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
    -- | Determines whether a Standard or Express state machine is created. The default is @STANDARD@ . You cannot update the @type@ of a state machine once it has been created.
    type' :: Lude.Maybe StateMachineType,
    -- | Defines what execution history events are logged and where they are logged.
    loggingConfiguration :: Lude.Maybe LoggingConfiguration,
    -- | Tags to be added when creating a state machine.
    --
    -- An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
    -- Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
    tags :: Lude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the IAM role to use for this state machine.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStateMachine' with the minimum fields required to make a request.
--
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
-- * 'type'' - Determines whether a Standard or Express state machine is created. The default is @STANDARD@ . You cannot update the @type@ of a state machine once it has been created.
-- * 'loggingConfiguration' - Defines what execution history events are logged and where they are logged.
-- * 'tags' - Tags to be added when creating a state machine.
--
-- An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
-- Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role to use for this state machine.
mkCreateStateMachine ::
  -- | 'definition'
  Lude.Sensitive Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CreateStateMachine
mkCreateStateMachine pDefinition_ pName_ pRoleARN_ =
  CreateStateMachine'
    { definition = pDefinition_,
      tracingConfiguration = Lude.Nothing,
      name = pName_,
      type' = Lude.Nothing,
      loggingConfiguration = Lude.Nothing,
      tags = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmDefinition :: Lens.Lens' CreateStateMachine (Lude.Sensitive Lude.Text)
csmDefinition = Lens.lens (definition :: CreateStateMachine -> Lude.Sensitive Lude.Text) (\s a -> s {definition = a} :: CreateStateMachine)
{-# DEPRECATED csmDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | Selects whether AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'tracingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmTracingConfiguration :: Lens.Lens' CreateStateMachine (Lude.Maybe TracingConfiguration)
csmTracingConfiguration = Lens.lens (tracingConfiguration :: CreateStateMachine -> Lude.Maybe TracingConfiguration) (\s a -> s {tracingConfiguration = a} :: CreateStateMachine)
{-# DEPRECATED csmTracingConfiguration "Use generic-lens or generic-optics with 'tracingConfiguration' instead." #-}

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
csmName :: Lens.Lens' CreateStateMachine Lude.Text
csmName = Lens.lens (name :: CreateStateMachine -> Lude.Text) (\s a -> s {name = a} :: CreateStateMachine)
{-# DEPRECATED csmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Determines whether a Standard or Express state machine is created. The default is @STANDARD@ . You cannot update the @type@ of a state machine once it has been created.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmType :: Lens.Lens' CreateStateMachine (Lude.Maybe StateMachineType)
csmType = Lens.lens (type' :: CreateStateMachine -> Lude.Maybe StateMachineType) (\s a -> s {type' = a} :: CreateStateMachine)
{-# DEPRECATED csmType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Defines what execution history events are logged and where they are logged.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmLoggingConfiguration :: Lens.Lens' CreateStateMachine (Lude.Maybe LoggingConfiguration)
csmLoggingConfiguration = Lens.lens (loggingConfiguration :: CreateStateMachine -> Lude.Maybe LoggingConfiguration) (\s a -> s {loggingConfiguration = a} :: CreateStateMachine)
{-# DEPRECATED csmLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

-- | Tags to be added when creating a state machine.
--
-- An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
-- Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmTags :: Lens.Lens' CreateStateMachine (Lude.Maybe [Tag])
csmTags = Lens.lens (tags :: CreateStateMachine -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateStateMachine)
{-# DEPRECATED csmTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to use for this state machine.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmRoleARN :: Lens.Lens' CreateStateMachine Lude.Text
csmRoleARN = Lens.lens (roleARN :: CreateStateMachine -> Lude.Text) (\s a -> s {roleARN = a} :: CreateStateMachine)
{-# DEPRECATED csmRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateStateMachine where
  type Rs CreateStateMachine = CreateStateMachineResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateStateMachineResponse'
            Lude.<$> (x Lude..:> "stateMachineArn")
            Lude.<*> (x Lude..:> "creationDate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStateMachine where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.CreateStateMachine" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateStateMachine where
  toJSON CreateStateMachine' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("definition" Lude..= definition),
            ("tracingConfiguration" Lude..=) Lude.<$> tracingConfiguration,
            Lude.Just ("name" Lude..= name),
            ("type" Lude..=) Lude.<$> type',
            ("loggingConfiguration" Lude..=) Lude.<$> loggingConfiguration,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateStateMachine where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStateMachine where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateStateMachineResponse' smart constructor.
data CreateStateMachineResponse = CreateStateMachineResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the created state machine.
    stateMachineARN :: Lude.Text,
    -- | The date the state machine is created.
    creationDate :: Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStateMachineResponse' with the minimum fields required to make a request.
--
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) that identifies the created state machine.
-- * 'creationDate' - The date the state machine is created.
-- * 'responseStatus' - The response status code.
mkCreateStateMachineResponse ::
  -- | 'stateMachineARN'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Timestamp ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateStateMachineResponse
mkCreateStateMachineResponse
  pStateMachineARN_
  pCreationDate_
  pResponseStatus_ =
    CreateStateMachineResponse'
      { stateMachineARN = pStateMachineARN_,
        creationDate = pCreationDate_,
        responseStatus = pResponseStatus_
      }

-- | The Amazon Resource Name (ARN) that identifies the created state machine.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmrsStateMachineARN :: Lens.Lens' CreateStateMachineResponse Lude.Text
csmrsStateMachineARN = Lens.lens (stateMachineARN :: CreateStateMachineResponse -> Lude.Text) (\s a -> s {stateMachineARN = a} :: CreateStateMachineResponse)
{-# DEPRECATED csmrsStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

-- | The date the state machine is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmrsCreationDate :: Lens.Lens' CreateStateMachineResponse Lude.Timestamp
csmrsCreationDate = Lens.lens (creationDate :: CreateStateMachineResponse -> Lude.Timestamp) (\s a -> s {creationDate = a} :: CreateStateMachineResponse)
{-# DEPRECATED csmrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmrsResponseStatus :: Lens.Lens' CreateStateMachineResponse Lude.Int
csmrsResponseStatus = Lens.lens (responseStatus :: CreateStateMachineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStateMachineResponse)
{-# DEPRECATED csmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
