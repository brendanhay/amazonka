{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteMatchmakingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently removes a FlexMatch matchmaking configuration. To delete, specify the configuration name. A matchmaking configuration cannot be deleted if it is being used in any active matchmaking tickets.
--
-- __Related operations__ 
--
--     * 'CreateMatchmakingConfiguration' 
--
--
--     * 'DescribeMatchmakingConfigurations' 
--
--
--     * 'UpdateMatchmakingConfiguration' 
--
--
--     * 'DeleteMatchmakingConfiguration' 
--
--
--     * 'CreateMatchmakingRuleSet' 
--
--
--     * 'DescribeMatchmakingRuleSets' 
--
--
--     * 'ValidateMatchmakingRuleSet' 
--
--
--     * 'DeleteMatchmakingRuleSet' 
--
--
module Network.AWS.GameLift.DeleteMatchmakingConfiguration
    (
    -- * Creating a request
      DeleteMatchmakingConfiguration (..)
    , mkDeleteMatchmakingConfiguration
    -- ** Request lenses
    , dmcName

    -- * Destructuring the response
    , DeleteMatchmakingConfigurationResponse (..)
    , mkDeleteMatchmakingConfigurationResponse
    -- ** Response lenses
    , dmcrfrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteMatchmakingConfiguration' smart constructor.
newtype DeleteMatchmakingConfiguration = DeleteMatchmakingConfiguration'
  { name :: Types.Name
    -- ^ A unique identifier for a matchmaking configuration. You can use either the configuration name or ARN value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMatchmakingConfiguration' value with any optional fields omitted.
mkDeleteMatchmakingConfiguration
    :: Types.Name -- ^ 'name'
    -> DeleteMatchmakingConfiguration
mkDeleteMatchmakingConfiguration name
  = DeleteMatchmakingConfiguration'{name}

-- | A unique identifier for a matchmaking configuration. You can use either the configuration name or ARN value.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcName :: Lens.Lens' DeleteMatchmakingConfiguration Types.Name
dmcName = Lens.field @"name"
{-# INLINEABLE dmcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteMatchmakingConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteMatchmakingConfiguration where
        toHeaders DeleteMatchmakingConfiguration{..}
          = Core.pure
              ("X-Amz-Target", "GameLift.DeleteMatchmakingConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteMatchmakingConfiguration where
        toJSON DeleteMatchmakingConfiguration{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteMatchmakingConfiguration where
        type Rs DeleteMatchmakingConfiguration =
             DeleteMatchmakingConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteMatchmakingConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteMatchmakingConfigurationResponse' smart constructor.
newtype DeleteMatchmakingConfigurationResponse = DeleteMatchmakingConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMatchmakingConfigurationResponse' value with any optional fields omitted.
mkDeleteMatchmakingConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteMatchmakingConfigurationResponse
mkDeleteMatchmakingConfigurationResponse responseStatus
  = DeleteMatchmakingConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrfrsResponseStatus :: Lens.Lens' DeleteMatchmakingConfigurationResponse Core.Int
dmcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
