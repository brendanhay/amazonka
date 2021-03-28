{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ResolveAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the fleet ID that an alias is currently pointing to.
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
module Network.AWS.GameLift.ResolveAlias
    (
    -- * Creating a request
      ResolveAlias (..)
    , mkResolveAlias
    -- ** Request lenses
    , raAliasId

    -- * Destructuring the response
    , ResolveAliasResponse (..)
    , mkResolveAliasResponse
    -- ** Response lenses
    , rarrsFleetArn
    , rarrsFleetId
    , rarrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkResolveAlias' smart constructor.
newtype ResolveAlias = ResolveAlias'
  { aliasId :: Types.AliasId
    -- ^ The unique identifier of the alias that you want to retrieve a fleet ID for. You can use either the alias ID or ARN value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResolveAlias' value with any optional fields omitted.
mkResolveAlias
    :: Types.AliasId -- ^ 'aliasId'
    -> ResolveAlias
mkResolveAlias aliasId = ResolveAlias'{aliasId}

-- | The unique identifier of the alias that you want to retrieve a fleet ID for. You can use either the alias ID or ARN value.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAliasId :: Lens.Lens' ResolveAlias Types.AliasId
raAliasId = Lens.field @"aliasId"
{-# INLINEABLE raAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

instance Core.ToQuery ResolveAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResolveAlias where
        toHeaders ResolveAlias{..}
          = Core.pure ("X-Amz-Target", "GameLift.ResolveAlias") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResolveAlias where
        toJSON ResolveAlias{..}
          = Core.object
              (Core.catMaybes [Core.Just ("AliasId" Core..= aliasId)])

instance Core.AWSRequest ResolveAlias where
        type Rs ResolveAlias = ResolveAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ResolveAliasResponse' Core.<$>
                   (x Core..:? "FleetArn") Core.<*> x Core..:? "FleetId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkResolveAliasResponse' smart constructor.
data ResolveAliasResponse = ResolveAliasResponse'
  { fleetArn :: Core.Maybe Types.FleetArn
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet resource that this alias points to. 
  , fleetId :: Core.Maybe Types.FleetId
    -- ^ The fleet identifier that the alias is pointing to.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResolveAliasResponse' value with any optional fields omitted.
mkResolveAliasResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResolveAliasResponse
mkResolveAliasResponse responseStatus
  = ResolveAliasResponse'{fleetArn = Core.Nothing,
                          fleetId = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet resource that this alias points to. 
--
-- /Note:/ Consider using 'fleetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarrsFleetArn :: Lens.Lens' ResolveAliasResponse (Core.Maybe Types.FleetArn)
rarrsFleetArn = Lens.field @"fleetArn"
{-# INLINEABLE rarrsFleetArn #-}
{-# DEPRECATED fleetArn "Use generic-lens or generic-optics with 'fleetArn' instead"  #-}

-- | The fleet identifier that the alias is pointing to.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarrsFleetId :: Lens.Lens' ResolveAliasResponse (Core.Maybe Types.FleetId)
rarrsFleetId = Lens.field @"fleetId"
{-# INLINEABLE rarrsFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarrsResponseStatus :: Lens.Lens' ResolveAliasResponse Core.Int
rarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
