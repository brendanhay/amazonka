{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteCustomAvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom Availability Zone (AZ).
--
-- A custom AZ is an on-premises AZ that is integrated with a VMware vSphere cluster.
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ > 
module Network.AWS.RDS.DeleteCustomAvailabilityZone
    (
    -- * Creating a request
      DeleteCustomAvailabilityZone (..)
    , mkDeleteCustomAvailabilityZone
    -- ** Request lenses
    , dCustomAvailabilityZoneId

    -- * Destructuring the response
    , DeleteCustomAvailabilityZoneResponse (..)
    , mkDeleteCustomAvailabilityZoneResponse
    -- ** Response lenses
    , dcazrfrsCustomAvailabilityZone
    , dcazrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCustomAvailabilityZone' smart constructor.
newtype DeleteCustomAvailabilityZone = DeleteCustomAvailabilityZone'
  { customAvailabilityZoneId :: Core.Text
    -- ^ The custom AZ identifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomAvailabilityZone' value with any optional fields omitted.
mkDeleteCustomAvailabilityZone
    :: Core.Text -- ^ 'customAvailabilityZoneId'
    -> DeleteCustomAvailabilityZone
mkDeleteCustomAvailabilityZone customAvailabilityZoneId
  = DeleteCustomAvailabilityZone'{customAvailabilityZoneId}

-- | The custom AZ identifier.
--
-- /Note:/ Consider using 'customAvailabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCustomAvailabilityZoneId :: Lens.Lens' DeleteCustomAvailabilityZone Core.Text
dCustomAvailabilityZoneId = Lens.field @"customAvailabilityZoneId"
{-# INLINEABLE dCustomAvailabilityZoneId #-}
{-# DEPRECATED customAvailabilityZoneId "Use generic-lens or generic-optics with 'customAvailabilityZoneId' instead"  #-}

instance Core.ToQuery DeleteCustomAvailabilityZone where
        toQuery DeleteCustomAvailabilityZone{..}
          = Core.toQueryPair "Action"
              ("DeleteCustomAvailabilityZone" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "CustomAvailabilityZoneId"
                customAvailabilityZoneId

instance Core.ToHeaders DeleteCustomAvailabilityZone where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteCustomAvailabilityZone where
        type Rs DeleteCustomAvailabilityZone =
             DeleteCustomAvailabilityZoneResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DeleteCustomAvailabilityZoneResult"
              (\ s h x ->
                 DeleteCustomAvailabilityZoneResponse' Core.<$>
                   (x Core..@? "CustomAvailabilityZone") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCustomAvailabilityZoneResponse' smart constructor.
data DeleteCustomAvailabilityZoneResponse = DeleteCustomAvailabilityZoneResponse'
  { customAvailabilityZone :: Core.Maybe Types.CustomAvailabilityZone
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomAvailabilityZoneResponse' value with any optional fields omitted.
mkDeleteCustomAvailabilityZoneResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCustomAvailabilityZoneResponse
mkDeleteCustomAvailabilityZoneResponse responseStatus
  = DeleteCustomAvailabilityZoneResponse'{customAvailabilityZone =
                                            Core.Nothing,
                                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'customAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazrfrsCustomAvailabilityZone :: Lens.Lens' DeleteCustomAvailabilityZoneResponse (Core.Maybe Types.CustomAvailabilityZone)
dcazrfrsCustomAvailabilityZone = Lens.field @"customAvailabilityZone"
{-# INLINEABLE dcazrfrsCustomAvailabilityZone #-}
{-# DEPRECATED customAvailabilityZone "Use generic-lens or generic-optics with 'customAvailabilityZone' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazrfrsResponseStatus :: Lens.Lens' DeleteCustomAvailabilityZoneResponse Core.Int
dcazrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcazrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
