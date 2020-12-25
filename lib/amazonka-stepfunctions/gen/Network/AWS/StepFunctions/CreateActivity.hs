{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.CreateActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an activity. An activity is a task that you write in any programming language and host on any machine that has access to AWS Step Functions. Activities must poll Step Functions using the @GetActivityTask@ API action and respond using @SendTask*@ API actions. This function lets Step Functions know the existence of your activity and returns an identifier for use in a state machine and when polling from the activity.
module Network.AWS.StepFunctions.CreateActivity
  ( -- * Creating a request
    CreateActivity (..),
    mkCreateActivity,

    -- ** Request lenses
    caName,
    caTags,

    -- * Destructuring the response
    CreateActivityResponse (..),
    mkCreateActivityResponse,

    -- ** Response lenses
    carrsActivityArn,
    carrsCreationDate,
    carrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkCreateActivity' smart constructor.
data CreateActivity = CreateActivity'
  { -- | The name of the activity to create. This name must be unique for your AWS account and region for 90 days. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ .
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
    -- | The list of tags to add to a resource.
    --
    -- An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
    -- Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateActivity' value with any optional fields omitted.
mkCreateActivity ::
  -- | 'name'
  Types.Name ->
  CreateActivity
mkCreateActivity name = CreateActivity' {name, tags = Core.Nothing}

-- | The name of the activity to create. This name must be unique for your AWS account and region for 90 days. For more information, see <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ .
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
caName :: Lens.Lens' CreateActivity Types.Name
caName = Lens.field @"name"
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The list of tags to add to a resource.
--
-- An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ , and <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
-- Tags may only contain Unicode letters, digits, white space, or these symbols: @_ . : / = + - @@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateActivity (Core.Maybe [Types.Tag])
caTags = Lens.field @"tags"
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateActivity where
  toJSON CreateActivity {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("name" Core..= name), ("tags" Core..=) Core.<$> tags]
      )

instance Core.AWSRequest CreateActivity where
  type Rs CreateActivity = CreateActivityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSStepFunctions.CreateActivity")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateActivityResponse'
            Core.<$> (x Core..: "activityArn")
            Core.<*> (x Core..: "creationDate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateActivityResponse' smart constructor.
data CreateActivityResponse = CreateActivityResponse'
  { -- | The Amazon Resource Name (ARN) that identifies the created activity.
    activityArn :: Types.Arn,
    -- | The date the activity is created.
    creationDate :: Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateActivityResponse' value with any optional fields omitted.
mkCreateActivityResponse ::
  -- | 'activityArn'
  Types.Arn ->
  -- | 'creationDate'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  CreateActivityResponse
mkCreateActivityResponse activityArn creationDate responseStatus =
  CreateActivityResponse'
    { activityArn,
      creationDate,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) that identifies the created activity.
--
-- /Note:/ Consider using 'activityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsActivityArn :: Lens.Lens' CreateActivityResponse Types.Arn
carrsActivityArn = Lens.field @"activityArn"
{-# DEPRECATED carrsActivityArn "Use generic-lens or generic-optics with 'activityArn' instead." #-}

-- | The date the activity is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsCreationDate :: Lens.Lens' CreateActivityResponse Core.NominalDiffTime
carrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED carrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateActivityResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
