{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new trigger.
module Network.AWS.Glue.CreateTrigger
    (
    -- * Creating a request
      CreateTrigger (..)
    , mkCreateTrigger
    -- ** Request lenses
    , ctName
    , ctType
    , ctActions
    , ctDescription
    , ctPredicate
    , ctSchedule
    , ctStartOnCreation
    , ctTags
    , ctWorkflowName

    -- * Destructuring the response
    , CreateTriggerResponse (..)
    , mkCreateTriggerResponse
    -- ** Response lenses
    , ctrrsName
    , ctrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTrigger' smart constructor.
data CreateTrigger = CreateTrigger'
  { name :: Types.NameString
    -- ^ The name of the trigger.
  , type' :: Types.TriggerType
    -- ^ The type of the new trigger.
  , actions :: [Types.Action]
    -- ^ The actions initiated by this trigger when it fires.
  , description :: Core.Maybe Types.DescriptionString
    -- ^ A description of the new trigger.
  , predicate :: Core.Maybe Types.Predicate
    -- ^ A predicate to specify when the new trigger should fire.
--
-- This field is required when the trigger type is @CONDITIONAL@ .
  , schedule :: Core.Maybe Types.GenericString
    -- ^ A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- This field is required when the trigger type is SCHEDULED.
  , startOnCreation :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when created. True is not supported for @ON_DEMAND@ triggers.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags to use with this trigger. You may use tags to limit access to the trigger. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide. 
  , workflowName :: Core.Maybe Types.NameString
    -- ^ The name of the workflow associated with the trigger.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrigger' value with any optional fields omitted.
mkCreateTrigger
    :: Types.NameString -- ^ 'name'
    -> Types.TriggerType -- ^ 'type\''
    -> CreateTrigger
mkCreateTrigger name type'
  = CreateTrigger'{name, type', actions = Core.mempty,
                   description = Core.Nothing, predicate = Core.Nothing,
                   schedule = Core.Nothing, startOnCreation = Core.Nothing,
                   tags = Core.Nothing, workflowName = Core.Nothing}

-- | The name of the trigger.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctName :: Lens.Lens' CreateTrigger Types.NameString
ctName = Lens.field @"name"
{-# INLINEABLE ctName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of the new trigger.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctType :: Lens.Lens' CreateTrigger Types.TriggerType
ctType = Lens.field @"type'"
{-# INLINEABLE ctType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The actions initiated by this trigger when it fires.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctActions :: Lens.Lens' CreateTrigger [Types.Action]
ctActions = Lens.field @"actions"
{-# INLINEABLE ctActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | A description of the new trigger.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDescription :: Lens.Lens' CreateTrigger (Core.Maybe Types.DescriptionString)
ctDescription = Lens.field @"description"
{-# INLINEABLE ctDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A predicate to specify when the new trigger should fire.
--
-- This field is required when the trigger type is @CONDITIONAL@ .
--
-- /Note:/ Consider using 'predicate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctPredicate :: Lens.Lens' CreateTrigger (Core.Maybe Types.Predicate)
ctPredicate = Lens.field @"predicate"
{-# INLINEABLE ctPredicate #-}
{-# DEPRECATED predicate "Use generic-lens or generic-optics with 'predicate' instead"  #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- This field is required when the trigger type is SCHEDULED.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctSchedule :: Lens.Lens' CreateTrigger (Core.Maybe Types.GenericString)
ctSchedule = Lens.field @"schedule"
{-# INLINEABLE ctSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when created. True is not supported for @ON_DEMAND@ triggers.
--
-- /Note:/ Consider using 'startOnCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctStartOnCreation :: Lens.Lens' CreateTrigger (Core.Maybe Core.Bool)
ctStartOnCreation = Lens.field @"startOnCreation"
{-# INLINEABLE ctStartOnCreation #-}
{-# DEPRECATED startOnCreation "Use generic-lens or generic-optics with 'startOnCreation' instead"  #-}

-- | The tags to use with this trigger. You may use tags to limit access to the trigger. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide. 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTrigger (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ctTags = Lens.field @"tags"
{-# INLINEABLE ctTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The name of the workflow associated with the trigger.
--
-- /Note:/ Consider using 'workflowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctWorkflowName :: Lens.Lens' CreateTrigger (Core.Maybe Types.NameString)
ctWorkflowName = Lens.field @"workflowName"
{-# INLINEABLE ctWorkflowName #-}
{-# DEPRECATED workflowName "Use generic-lens or generic-optics with 'workflowName' instead"  #-}

instance Core.ToQuery CreateTrigger where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTrigger where
        toHeaders CreateTrigger{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.CreateTrigger") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateTrigger where
        toJSON CreateTrigger{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name), Core.Just ("Type" Core..= type'),
                  Core.Just ("Actions" Core..= actions),
                  ("Description" Core..=) Core.<$> description,
                  ("Predicate" Core..=) Core.<$> predicate,
                  ("Schedule" Core..=) Core.<$> schedule,
                  ("StartOnCreation" Core..=) Core.<$> startOnCreation,
                  ("Tags" Core..=) Core.<$> tags,
                  ("WorkflowName" Core..=) Core.<$> workflowName])

instance Core.AWSRequest CreateTrigger where
        type Rs CreateTrigger = CreateTriggerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateTriggerResponse' Core.<$>
                   (x Core..:? "Name") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTriggerResponse' smart constructor.
data CreateTriggerResponse = CreateTriggerResponse'
  { name :: Core.Maybe Types.Name
    -- ^ The name of the trigger.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTriggerResponse' value with any optional fields omitted.
mkCreateTriggerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTriggerResponse
mkCreateTriggerResponse responseStatus
  = CreateTriggerResponse'{name = Core.Nothing, responseStatus}

-- | The name of the trigger.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsName :: Lens.Lens' CreateTriggerResponse (Core.Maybe Types.Name)
ctrrsName = Lens.field @"name"
{-# INLINEABLE ctrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTriggerResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
