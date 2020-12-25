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
    dsmStateMachineArn,

    -- * Destructuring the response
    DescribeStateMachineResponse (..),
    mkDescribeStateMachineResponse,

    -- ** Response lenses
    dsmrrsStateMachineArn,
    dsmrrsName,
    dsmrrsDefinition,
    dsmrrsRoleArn,
    dsmrrsType,
    dsmrrsCreationDate,
    dsmrrsLoggingConfiguration,
    dsmrrsStatus,
    dsmrrsTracingConfiguration,
    dsmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkDescribeStateMachine' smart constructor.
newtype DescribeStateMachine = DescribeStateMachine'
  { -- | The Amazon Resource Name (ARN) of the state machine to describe.
    stateMachineArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStateMachine' value with any optional fields omitted.
mkDescribeStateMachine ::
  -- | 'stateMachineArn'
  Types.Arn ->
  DescribeStateMachine
mkDescribeStateMachine stateMachineArn =
  DescribeStateMachine' {stateMachineArn}

-- | The Amazon Resource Name (ARN) of the state machine to describe.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmStateMachineArn :: Lens.Lens' DescribeStateMachine Types.Arn
dsmStateMachineArn = Lens.field @"stateMachineArn"
{-# DEPRECATED dsmStateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead." #-}

instance Core.FromJSON DescribeStateMachine where
  toJSON DescribeStateMachine {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("stateMachineArn" Core..= stateMachineArn)]
      )

instance Core.AWSRequest DescribeStateMachine where
  type Rs DescribeStateMachine = DescribeStateMachineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSStepFunctions.DescribeStateMachine")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStateMachineResponse'
            Core.<$> (x Core..: "stateMachineArn")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "definition")
            Core.<*> (x Core..: "roleArn")
            Core.<*> (x Core..: "type")
            Core.<*> (x Core..: "creationDate")
            Core.<*> (x Core..:? "loggingConfiguration")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "tracingConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeStateMachineResponse' smart constructor.
data DescribeStateMachineResponse = DescribeStateMachineResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the state machine.
    stateMachineArn :: Types.Arn,
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
    name :: Types.Name,
    -- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
    definition :: Types.Definition,
    -- | The Amazon Resource Name (ARN) of the IAM role used when creating this state machine. (The IAM role maintains security by granting Step Functions access to AWS resources.)
    roleArn :: Types.Arn,
    -- | The @type@ of the state machine (@STANDARD@ or @EXPRESS@ ).
    type' :: Types.StateMachineType,
    -- | The date the state machine is created.
    creationDate :: Core.NominalDiffTime,
    loggingConfiguration :: Core.Maybe Types.LoggingConfiguration,
    -- | The current status of the state machine.
    status :: Core.Maybe Types.StateMachineStatus,
    -- | Selects whether AWS X-Ray tracing is enabled.
    tracingConfiguration :: Core.Maybe Types.TracingConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStateMachineResponse' value with any optional fields omitted.
mkDescribeStateMachineResponse ::
  -- | 'stateMachineArn'
  Types.Arn ->
  -- | 'name'
  Types.Name ->
  -- | 'definition'
  Types.Definition ->
  -- | 'roleArn'
  Types.Arn ->
  -- | 'type\''
  Types.StateMachineType ->
  -- | 'creationDate'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeStateMachineResponse
mkDescribeStateMachineResponse
  stateMachineArn
  name
  definition
  roleArn
  type'
  creationDate
  responseStatus =
    DescribeStateMachineResponse'
      { stateMachineArn,
        name,
        definition,
        roleArn,
        type',
        creationDate,
        loggingConfiguration = Core.Nothing,
        status = Core.Nothing,
        tracingConfiguration = Core.Nothing,
        responseStatus
      }

-- | The Amazon Resource Name (ARN) that identifies the state machine.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsStateMachineArn :: Lens.Lens' DescribeStateMachineResponse Types.Arn
dsmrrsStateMachineArn = Lens.field @"stateMachineArn"
{-# DEPRECATED dsmrrsStateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead." #-}

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
dsmrrsName :: Lens.Lens' DescribeStateMachineResponse Types.Name
dsmrrsName = Lens.field @"name"
{-# DEPRECATED dsmrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsDefinition :: Lens.Lens' DescribeStateMachineResponse Types.Definition
dsmrrsDefinition = Lens.field @"definition"
{-# DEPRECATED dsmrrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role used when creating this state machine. (The IAM role maintains security by granting Step Functions access to AWS resources.)
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsRoleArn :: Lens.Lens' DescribeStateMachineResponse Types.Arn
dsmrrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dsmrrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The @type@ of the state machine (@STANDARD@ or @EXPRESS@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsType :: Lens.Lens' DescribeStateMachineResponse Types.StateMachineType
dsmrrsType = Lens.field @"type'"
{-# DEPRECATED dsmrrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date the state machine is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsCreationDate :: Lens.Lens' DescribeStateMachineResponse Core.NominalDiffTime
dsmrrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED dsmrrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsLoggingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Core.Maybe Types.LoggingConfiguration)
dsmrrsLoggingConfiguration = Lens.field @"loggingConfiguration"
{-# DEPRECATED dsmrrsLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

-- | The current status of the state machine.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsStatus :: Lens.Lens' DescribeStateMachineResponse (Core.Maybe Types.StateMachineStatus)
dsmrrsStatus = Lens.field @"status"
{-# DEPRECATED dsmrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Selects whether AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'tracingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsTracingConfiguration :: Lens.Lens' DescribeStateMachineResponse (Core.Maybe Types.TracingConfiguration)
dsmrrsTracingConfiguration = Lens.field @"tracingConfiguration"
{-# DEPRECATED dsmrrsTracingConfiguration "Use generic-lens or generic-optics with 'tracingConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrrsResponseStatus :: Lens.Lens' DescribeStateMachineResponse Core.Int
dsmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
