{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates properties for an alias. To update properties, specify the alias ID to be updated and provide the information to be changed. To reassign an alias to another fleet, provide an updated routing strategy. If successful, the updated alias record is returned.
--
--
--     * 'CreateAlias' 
--
--
--     * 'ListAliases' 
--
--
--     * 'DescribeAlias' 
--
--
--     * 'UpdateAlias' 
--
--
--     * 'DeleteAlias' 
--
--
--     * 'ResolveAlias' 
--
--
module Network.AWS.GameLift.UpdateAlias
    (
    -- * Creating a request
      UpdateAlias (..)
    , mkUpdateAlias
    -- ** Request lenses
    , uaAliasId
    , uaDescription
    , uaName
    , uaRoutingStrategy

    -- * Destructuring the response
    , UpdateAliasResponse (..)
    , mkUpdateAliasResponse
    -- ** Response lenses
    , uarrsAlias
    , uarrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { aliasId :: Types.AliasIdOrArn
    -- ^ A unique identifier for the alias that you want to update. You can use either the alias ID or ARN value.
  , description :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A human-readable description of the alias.
  , name :: Core.Maybe Types.NonBlankAndLengthConstraintString
    -- ^ A descriptive label that is associated with an alias. Alias names do not need to be unique.
  , routingStrategy :: Core.Maybe Types.RoutingStrategy
    -- ^ The routing configuration, including routing type and fleet target, for the alias.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAlias' value with any optional fields omitted.
mkUpdateAlias
    :: Types.AliasIdOrArn -- ^ 'aliasId'
    -> UpdateAlias
mkUpdateAlias aliasId
  = UpdateAlias'{aliasId, description = Core.Nothing,
                 name = Core.Nothing, routingStrategy = Core.Nothing}

-- | A unique identifier for the alias that you want to update. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAliasId :: Lens.Lens' UpdateAlias Types.AliasIdOrArn
uaAliasId = Lens.field @"aliasId"
{-# INLINEABLE uaAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

-- | A human-readable description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateAlias (Core.Maybe Types.NonZeroAndMaxString)
uaDescription = Lens.field @"description"
{-# INLINEABLE uaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaName :: Lens.Lens' UpdateAlias (Core.Maybe Types.NonBlankAndLengthConstraintString)
uaName = Lens.field @"name"
{-# INLINEABLE uaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The routing configuration, including routing type and fleet target, for the alias.
--
-- /Note:/ Consider using 'routingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaRoutingStrategy :: Lens.Lens' UpdateAlias (Core.Maybe Types.RoutingStrategy)
uaRoutingStrategy = Lens.field @"routingStrategy"
{-# INLINEABLE uaRoutingStrategy #-}
{-# DEPRECATED routingStrategy "Use generic-lens or generic-optics with 'routingStrategy' instead"  #-}

instance Core.ToQuery UpdateAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAlias where
        toHeaders UpdateAlias{..}
          = Core.pure ("X-Amz-Target", "GameLift.UpdateAlias") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateAlias where
        toJSON UpdateAlias{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AliasId" Core..= aliasId),
                  ("Description" Core..=) Core.<$> description,
                  ("Name" Core..=) Core.<$> name,
                  ("RoutingStrategy" Core..=) Core.<$> routingStrategy])

instance Core.AWSRequest UpdateAlias where
        type Rs UpdateAlias = UpdateAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateAliasResponse' Core.<$>
                   (x Core..:? "Alias") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateAliasResponse' smart constructor.
data UpdateAliasResponse = UpdateAliasResponse'
  { alias :: Core.Maybe Types.Alias
    -- ^ The updated alias resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateAliasResponse' value with any optional fields omitted.
mkUpdateAliasResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateAliasResponse
mkUpdateAliasResponse responseStatus
  = UpdateAliasResponse'{alias = Core.Nothing, responseStatus}

-- | The updated alias resource.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsAlias :: Lens.Lens' UpdateAliasResponse (Core.Maybe Types.Alias)
uarrsAlias = Lens.field @"alias"
{-# INLINEABLE uarrsAlias #-}
{-# DEPRECATED alias "Use generic-lens or generic-optics with 'alias' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsResponseStatus :: Lens.Lens' UpdateAliasResponse Core.Int
uarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
