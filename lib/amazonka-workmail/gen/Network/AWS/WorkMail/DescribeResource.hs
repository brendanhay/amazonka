{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DescribeResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data available for the resource.
module Network.AWS.WorkMail.DescribeResource
    (
    -- * Creating a request
      DescribeResource (..)
    , mkDescribeResource
    -- ** Request lenses
    , drOrganizationId
    , drResourceId

    -- * Destructuring the response
    , DescribeResourceResponse (..)
    , mkDescribeResourceResponse
    -- ** Response lenses
    , drrrsBookingOptions
    , drrrsDisabledDate
    , drrrsEmail
    , drrrsEnabledDate
    , drrrsName
    , drrrsResourceId
    , drrrsState
    , drrrsType
    , drrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDescribeResource' smart constructor.
data DescribeResource = DescribeResource'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier associated with the organization for which the resource is described.
  , resourceId :: Types.ResourceId
    -- ^ The identifier of the resource to be described.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeResource' value with any optional fields omitted.
mkDescribeResource
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.ResourceId -- ^ 'resourceId'
    -> DescribeResource
mkDescribeResource organizationId resourceId
  = DescribeResource'{organizationId, resourceId}

-- | The identifier associated with the organization for which the resource is described.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drOrganizationId :: Lens.Lens' DescribeResource Types.OrganizationId
drOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE drOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The identifier of the resource to be described.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drResourceId :: Lens.Lens' DescribeResource Types.ResourceId
drResourceId = Lens.field @"resourceId"
{-# INLINEABLE drResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

instance Core.ToQuery DescribeResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeResource where
        toHeaders DescribeResource{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.DescribeResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeResource where
        toJSON DescribeResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("ResourceId" Core..= resourceId)])

instance Core.AWSRequest DescribeResource where
        type Rs DescribeResource = DescribeResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeResourceResponse' Core.<$>
                   (x Core..:? "BookingOptions") Core.<*> x Core..:? "DisabledDate"
                     Core.<*> x Core..:? "Email"
                     Core.<*> x Core..:? "EnabledDate"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "ResourceId"
                     Core.<*> x Core..:? "State"
                     Core.<*> x Core..:? "Type"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeResourceResponse' smart constructor.
data DescribeResourceResponse = DescribeResourceResponse'
  { bookingOptions :: Core.Maybe Types.BookingOptions
    -- ^ The booking options for the described resource.
  , disabledDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when a resource was disabled from WorkMail, in UNIX epoch time format.
  , email :: Core.Maybe Types.Email
    -- ^ The email of the described resource.
  , enabledDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when a resource was enabled for WorkMail, in UNIX epoch time format.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the described resource.
  , resourceId :: Core.Maybe Types.ResourceId
    -- ^ The identifier of the described resource.
  , state :: Core.Maybe Types.EntityState
    -- ^ The state of the resource: enabled (registered to Amazon WorkMail), disabled (deregistered or never registered to WorkMail), or deleted.
  , type' :: Core.Maybe Types.ResourceType
    -- ^ The type of the described resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeResourceResponse' value with any optional fields omitted.
mkDescribeResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeResourceResponse
mkDescribeResourceResponse responseStatus
  = DescribeResourceResponse'{bookingOptions = Core.Nothing,
                              disabledDate = Core.Nothing, email = Core.Nothing,
                              enabledDate = Core.Nothing, name = Core.Nothing,
                              resourceId = Core.Nothing, state = Core.Nothing,
                              type' = Core.Nothing, responseStatus}

-- | The booking options for the described resource.
--
-- /Note:/ Consider using 'bookingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsBookingOptions :: Lens.Lens' DescribeResourceResponse (Core.Maybe Types.BookingOptions)
drrrsBookingOptions = Lens.field @"bookingOptions"
{-# INLINEABLE drrrsBookingOptions #-}
{-# DEPRECATED bookingOptions "Use generic-lens or generic-optics with 'bookingOptions' instead"  #-}

-- | The date and time when a resource was disabled from WorkMail, in UNIX epoch time format.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsDisabledDate :: Lens.Lens' DescribeResourceResponse (Core.Maybe Core.NominalDiffTime)
drrrsDisabledDate = Lens.field @"disabledDate"
{-# INLINEABLE drrrsDisabledDate #-}
{-# DEPRECATED disabledDate "Use generic-lens or generic-optics with 'disabledDate' instead"  #-}

-- | The email of the described resource.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsEmail :: Lens.Lens' DescribeResourceResponse (Core.Maybe Types.Email)
drrrsEmail = Lens.field @"email"
{-# INLINEABLE drrrsEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The date and time when a resource was enabled for WorkMail, in UNIX epoch time format.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsEnabledDate :: Lens.Lens' DescribeResourceResponse (Core.Maybe Core.NominalDiffTime)
drrrsEnabledDate = Lens.field @"enabledDate"
{-# INLINEABLE drrrsEnabledDate #-}
{-# DEPRECATED enabledDate "Use generic-lens or generic-optics with 'enabledDate' instead"  #-}

-- | The name of the described resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsName :: Lens.Lens' DescribeResourceResponse (Core.Maybe Types.Name)
drrrsName = Lens.field @"name"
{-# INLINEABLE drrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The identifier of the described resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResourceId :: Lens.Lens' DescribeResourceResponse (Core.Maybe Types.ResourceId)
drrrsResourceId = Lens.field @"resourceId"
{-# INLINEABLE drrrsResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The state of the resource: enabled (registered to Amazon WorkMail), disabled (deregistered or never registered to WorkMail), or deleted.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsState :: Lens.Lens' DescribeResourceResponse (Core.Maybe Types.EntityState)
drrrsState = Lens.field @"state"
{-# INLINEABLE drrrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The type of the described resource.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsType :: Lens.Lens' DescribeResourceResponse (Core.Maybe Types.ResourceType)
drrrsType = Lens.field @"type'"
{-# INLINEABLE drrrsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DescribeResourceResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
