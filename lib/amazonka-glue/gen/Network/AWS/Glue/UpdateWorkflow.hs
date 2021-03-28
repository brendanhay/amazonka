{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing workflow.
module Network.AWS.Glue.UpdateWorkflow
    (
    -- * Creating a request
      UpdateWorkflow (..)
    , mkUpdateWorkflow
    -- ** Request lenses
    , uwName
    , uwDefaultRunProperties
    , uwDescription
    , uwMaxConcurrentRuns

    -- * Destructuring the response
    , UpdateWorkflowResponse (..)
    , mkUpdateWorkflowResponse
    -- ** Response lenses
    , uwrrsName
    , uwrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateWorkflow' smart constructor.
data UpdateWorkflow = UpdateWorkflow'
  { name :: Types.NameString
    -- ^ Name of the workflow to be updated.
  , defaultRunProperties :: Core.Maybe (Core.HashMap Types.IdString Types.GenericString)
    -- ^ A collection of properties to be used as part of each execution of the workflow.
  , description :: Core.Maybe Types.GenericString
    -- ^ The description of the workflow.
  , maxConcurrentRuns :: Core.Maybe Core.Int
    -- ^ You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateWorkflow' value with any optional fields omitted.
mkUpdateWorkflow
    :: Types.NameString -- ^ 'name'
    -> UpdateWorkflow
mkUpdateWorkflow name
  = UpdateWorkflow'{name, defaultRunProperties = Core.Nothing,
                    description = Core.Nothing, maxConcurrentRuns = Core.Nothing}

-- | Name of the workflow to be updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwName :: Lens.Lens' UpdateWorkflow Types.NameString
uwName = Lens.field @"name"
{-# INLINEABLE uwName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A collection of properties to be used as part of each execution of the workflow.
--
-- /Note:/ Consider using 'defaultRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwDefaultRunProperties :: Lens.Lens' UpdateWorkflow (Core.Maybe (Core.HashMap Types.IdString Types.GenericString))
uwDefaultRunProperties = Lens.field @"defaultRunProperties"
{-# INLINEABLE uwDefaultRunProperties #-}
{-# DEPRECATED defaultRunProperties "Use generic-lens or generic-optics with 'defaultRunProperties' instead"  #-}

-- | The description of the workflow.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwDescription :: Lens.Lens' UpdateWorkflow (Core.Maybe Types.GenericString)
uwDescription = Lens.field @"description"
{-# INLINEABLE uwDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
--
-- /Note:/ Consider using 'maxConcurrentRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwMaxConcurrentRuns :: Lens.Lens' UpdateWorkflow (Core.Maybe Core.Int)
uwMaxConcurrentRuns = Lens.field @"maxConcurrentRuns"
{-# INLINEABLE uwMaxConcurrentRuns #-}
{-# DEPRECATED maxConcurrentRuns "Use generic-lens or generic-optics with 'maxConcurrentRuns' instead"  #-}

instance Core.ToQuery UpdateWorkflow where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateWorkflow where
        toHeaders UpdateWorkflow{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.UpdateWorkflow") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateWorkflow where
        toJSON UpdateWorkflow{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("DefaultRunProperties" Core..=) Core.<$> defaultRunProperties,
                  ("Description" Core..=) Core.<$> description,
                  ("MaxConcurrentRuns" Core..=) Core.<$> maxConcurrentRuns])

instance Core.AWSRequest UpdateWorkflow where
        type Rs UpdateWorkflow = UpdateWorkflowResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateWorkflowResponse' Core.<$>
                   (x Core..:? "Name") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateWorkflowResponse' smart constructor.
data UpdateWorkflowResponse = UpdateWorkflowResponse'
  { name :: Core.Maybe Types.Name
    -- ^ The name of the workflow which was specified in input.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateWorkflowResponse' value with any optional fields omitted.
mkUpdateWorkflowResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateWorkflowResponse
mkUpdateWorkflowResponse responseStatus
  = UpdateWorkflowResponse'{name = Core.Nothing, responseStatus}

-- | The name of the workflow which was specified in input.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwrrsName :: Lens.Lens' UpdateWorkflowResponse (Core.Maybe Types.Name)
uwrrsName = Lens.field @"name"
{-# INLINEABLE uwrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwrrsResponseStatus :: Lens.Lens' UpdateWorkflowResponse Core.Int
uwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
