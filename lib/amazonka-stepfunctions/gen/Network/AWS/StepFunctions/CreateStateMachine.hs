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
    csmName,
    csmDefinition,
    csmRoleArn,
    csmLoggingConfiguration,
    csmTags,
    csmTracingConfiguration,
    csmType,

    -- * Destructuring the response
    CreateStateMachineResponse (..),
    mkCreateStateMachineResponse,

    -- ** Response lenses
    csmrrsStateMachineArn,
    csmrrsCreationDate,
    csmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkCreateStateMachine' smart constructor.
data CreateStateMachine = CreateStateMachine'
  { -- | The name of the state machine.
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
    name :: Types.Name,
    -- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
    definition :: Types.Definition,
    -- | The Amazon Resource Name (ARN) of the IAM role to use for this state machine.
    roleArn :: Types.Arn,
    -- | Defines what execution history events are logged and where they are logged.
    loggingConfiguration :: Core.Maybe Types.LoggingConfiguration,
    -- | Tags to be added when creating a state machine.
    --
    -- An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
    -- Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
    tags :: Core.Maybe [Types.Tag],
    -- | Selects whether AWS X-Ray tracing is enabled.
    tracingConfiguration :: Core.Maybe Types.TracingConfiguration,
    -- | Determines whether a Standard or Express state machine is created. The default is @STANDARD@ . You cannot update the @type@ of a state machine once it has been created.
    type' :: Core.Maybe Types.StateMachineType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStateMachine' value with any optional fields omitted.
mkCreateStateMachine ::
  -- | 'name'
  Types.Name ->
  -- | 'definition'
  Types.Definition ->
  -- | 'roleArn'
  Types.Arn ->
  CreateStateMachine
mkCreateStateMachine name definition roleArn =
  CreateStateMachine'
    { name,
      definition,
      roleArn,
      loggingConfiguration = Core.Nothing,
      tags = Core.Nothing,
      tracingConfiguration = Core.Nothing,
      type' = Core.Nothing
    }

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
csmName :: Lens.Lens' CreateStateMachine Types.Name
csmName = Lens.field @"name"
{-# DEPRECATED csmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmDefinition :: Lens.Lens' CreateStateMachine Types.Definition
csmDefinition = Lens.field @"definition"
{-# DEPRECATED csmDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to use for this state machine.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmRoleArn :: Lens.Lens' CreateStateMachine Types.Arn
csmRoleArn = Lens.field @"roleArn"
{-# DEPRECATED csmRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Defines what execution history events are logged and where they are logged.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmLoggingConfiguration :: Lens.Lens' CreateStateMachine (Core.Maybe Types.LoggingConfiguration)
csmLoggingConfiguration = Lens.field @"loggingConfiguration"
{-# DEPRECATED csmLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

-- | Tags to be added when creating a state machine.
--
-- An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
-- Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmTags :: Lens.Lens' CreateStateMachine (Core.Maybe [Types.Tag])
csmTags = Lens.field @"tags"
{-# DEPRECATED csmTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Selects whether AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'tracingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmTracingConfiguration :: Lens.Lens' CreateStateMachine (Core.Maybe Types.TracingConfiguration)
csmTracingConfiguration = Lens.field @"tracingConfiguration"
{-# DEPRECATED csmTracingConfiguration "Use generic-lens or generic-optics with 'tracingConfiguration' instead." #-}

-- | Determines whether a Standard or Express state machine is created. The default is @STANDARD@ . You cannot update the @type@ of a state machine once it has been created.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmType :: Lens.Lens' CreateStateMachine (Core.Maybe Types.StateMachineType)
csmType = Lens.field @"type'"
{-# DEPRECATED csmType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON CreateStateMachine where
  toJSON CreateStateMachine {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("definition" Core..= definition),
            Core.Just ("roleArn" Core..= roleArn),
            ("loggingConfiguration" Core..=) Core.<$> loggingConfiguration,
            ("tags" Core..=) Core.<$> tags,
            ("tracingConfiguration" Core..=) Core.<$> tracingConfiguration,
            ("type" Core..=) Core.<$> type'
          ]
      )

instance Core.AWSRequest CreateStateMachine where
  type Rs CreateStateMachine = CreateStateMachineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSStepFunctions.CreateStateMachine")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStateMachineResponse'
            Core.<$> (x Core..: "stateMachineArn")
            Core.<*> (x Core..: "creationDate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateStateMachineResponse' smart constructor.
data CreateStateMachineResponse = CreateStateMachineResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the created state machine.
    stateMachineArn :: Types.Arn,
    -- | The date the state machine is created.
    creationDate :: Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateStateMachineResponse' value with any optional fields omitted.
mkCreateStateMachineResponse ::
  -- | 'stateMachineArn'
  Types.Arn ->
  -- | 'creationDate'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  CreateStateMachineResponse
mkCreateStateMachineResponse
  stateMachineArn
  creationDate
  responseStatus =
    CreateStateMachineResponse'
      { stateMachineArn,
        creationDate,
        responseStatus
      }

-- | The Amazon Resource Name (ARN) that identifies the created state machine.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmrrsStateMachineArn :: Lens.Lens' CreateStateMachineResponse Types.Arn
csmrrsStateMachineArn = Lens.field @"stateMachineArn"
{-# DEPRECATED csmrrsStateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead." #-}

-- | The date the state machine is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmrrsCreationDate :: Lens.Lens' CreateStateMachineResponse Core.NominalDiffTime
csmrrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED csmrrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmrrsResponseStatus :: Lens.Lens' CreateStateMachineResponse Core.Int
csmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
