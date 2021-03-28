{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateRoutingProfileName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and description of a routing profile. The request accepts the following data in JSON format. At least @Name@ or @Description@ must be provided.
module Network.AWS.Connect.UpdateRoutingProfileName
    (
    -- * Creating a request
      UpdateRoutingProfileName (..)
    , mkUpdateRoutingProfileName
    -- ** Request lenses
    , urpnInstanceId
    , urpnRoutingProfileId
    , urpnDescription
    , urpnName

    -- * Destructuring the response
    , UpdateRoutingProfileNameResponse (..)
    , mkUpdateRoutingProfileNameResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRoutingProfileName' smart constructor.
data UpdateRoutingProfileName = UpdateRoutingProfileName'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , routingProfileId :: Types.RoutingProfileId
    -- ^ The identifier of the routing profile.
  , description :: Core.Maybe Types.RoutingProfileDescription
    -- ^ The description of the routing profile. Must not be more than 250 characters.
  , name :: Core.Maybe Types.RoutingProfileName
    -- ^ The name of the routing profile. Must not be more than 127 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoutingProfileName' value with any optional fields omitted.
mkUpdateRoutingProfileName
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.RoutingProfileId -- ^ 'routingProfileId'
    -> UpdateRoutingProfileName
mkUpdateRoutingProfileName instanceId routingProfileId
  = UpdateRoutingProfileName'{instanceId, routingProfileId,
                              description = Core.Nothing, name = Core.Nothing}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpnInstanceId :: Lens.Lens' UpdateRoutingProfileName Types.InstanceId
urpnInstanceId = Lens.field @"instanceId"
{-# INLINEABLE urpnInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpnRoutingProfileId :: Lens.Lens' UpdateRoutingProfileName Types.RoutingProfileId
urpnRoutingProfileId = Lens.field @"routingProfileId"
{-# INLINEABLE urpnRoutingProfileId #-}
{-# DEPRECATED routingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead"  #-}

-- | The description of the routing profile. Must not be more than 250 characters.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpnDescription :: Lens.Lens' UpdateRoutingProfileName (Core.Maybe Types.RoutingProfileDescription)
urpnDescription = Lens.field @"description"
{-# INLINEABLE urpnDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the routing profile. Must not be more than 127 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpnName :: Lens.Lens' UpdateRoutingProfileName (Core.Maybe Types.RoutingProfileName)
urpnName = Lens.field @"name"
{-# INLINEABLE urpnName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateRoutingProfileName where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRoutingProfileName where
        toHeaders UpdateRoutingProfileName{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRoutingProfileName where
        toJSON UpdateRoutingProfileName{..}
          = Core.object
              (Core.catMaybes
                 [("Description" Core..=) Core.<$> description,
                  ("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateRoutingProfileName where
        type Rs UpdateRoutingProfileName = UpdateRoutingProfileNameResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/routing-profiles/" Core.<> Core.toText instanceId Core.<> "/"
                             Core.<> Core.toText routingProfileId
                             Core.<> "/name",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull UpdateRoutingProfileNameResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRoutingProfileNameResponse' smart constructor.
data UpdateRoutingProfileNameResponse = UpdateRoutingProfileNameResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoutingProfileNameResponse' value with any optional fields omitted.
mkUpdateRoutingProfileNameResponse
    :: UpdateRoutingProfileNameResponse
mkUpdateRoutingProfileNameResponse
  = UpdateRoutingProfileNameResponse'
