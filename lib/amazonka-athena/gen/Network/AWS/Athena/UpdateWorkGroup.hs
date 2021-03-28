{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.UpdateWorkGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the workgroup with the specified name. The workgroup's name cannot be changed.
module Network.AWS.Athena.UpdateWorkGroup
    (
    -- * Creating a request
      UpdateWorkGroup (..)
    , mkUpdateWorkGroup
    -- ** Request lenses
    , uwgWorkGroup
    , uwgConfigurationUpdates
    , uwgDescription
    , uwgState

    -- * Destructuring the response
    , UpdateWorkGroupResponse (..)
    , mkUpdateWorkGroupResponse
    -- ** Response lenses
    , uwgrrsResponseStatus
    ) where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateWorkGroup' smart constructor.
data UpdateWorkGroup = UpdateWorkGroup'
  { workGroup :: Types.WorkGroupName
    -- ^ The specified workgroup that will be updated.
  , configurationUpdates :: Core.Maybe Types.WorkGroupConfigurationUpdates
    -- ^ The workgroup configuration that will be updated for the given workgroup.
  , description :: Core.Maybe Types.WorkGroupDescriptionString
    -- ^ The workgroup description.
  , state :: Core.Maybe Types.WorkGroupState
    -- ^ The workgroup state that will be updated for the given workgroup.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateWorkGroup' value with any optional fields omitted.
mkUpdateWorkGroup
    :: Types.WorkGroupName -- ^ 'workGroup'
    -> UpdateWorkGroup
mkUpdateWorkGroup workGroup
  = UpdateWorkGroup'{workGroup, configurationUpdates = Core.Nothing,
                     description = Core.Nothing, state = Core.Nothing}

-- | The specified workgroup that will be updated.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwgWorkGroup :: Lens.Lens' UpdateWorkGroup Types.WorkGroupName
uwgWorkGroup = Lens.field @"workGroup"
{-# INLINEABLE uwgWorkGroup #-}
{-# DEPRECATED workGroup "Use generic-lens or generic-optics with 'workGroup' instead"  #-}

-- | The workgroup configuration that will be updated for the given workgroup.
--
-- /Note:/ Consider using 'configurationUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwgConfigurationUpdates :: Lens.Lens' UpdateWorkGroup (Core.Maybe Types.WorkGroupConfigurationUpdates)
uwgConfigurationUpdates = Lens.field @"configurationUpdates"
{-# INLINEABLE uwgConfigurationUpdates #-}
{-# DEPRECATED configurationUpdates "Use generic-lens or generic-optics with 'configurationUpdates' instead"  #-}

-- | The workgroup description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwgDescription :: Lens.Lens' UpdateWorkGroup (Core.Maybe Types.WorkGroupDescriptionString)
uwgDescription = Lens.field @"description"
{-# INLINEABLE uwgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The workgroup state that will be updated for the given workgroup.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwgState :: Lens.Lens' UpdateWorkGroup (Core.Maybe Types.WorkGroupState)
uwgState = Lens.field @"state"
{-# INLINEABLE uwgState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.ToQuery UpdateWorkGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateWorkGroup where
        toHeaders UpdateWorkGroup{..}
          = Core.pure ("X-Amz-Target", "AmazonAthena.UpdateWorkGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateWorkGroup where
        toJSON UpdateWorkGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WorkGroup" Core..= workGroup),
                  ("ConfigurationUpdates" Core..=) Core.<$> configurationUpdates,
                  ("Description" Core..=) Core.<$> description,
                  ("State" Core..=) Core.<$> state])

instance Core.AWSRequest UpdateWorkGroup where
        type Rs UpdateWorkGroup = UpdateWorkGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateWorkGroupResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateWorkGroupResponse' smart constructor.
newtype UpdateWorkGroupResponse = UpdateWorkGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateWorkGroupResponse' value with any optional fields omitted.
mkUpdateWorkGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateWorkGroupResponse
mkUpdateWorkGroupResponse responseStatus
  = UpdateWorkGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwgrrsResponseStatus :: Lens.Lens' UpdateWorkGroupResponse Core.Int
uwgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uwgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
