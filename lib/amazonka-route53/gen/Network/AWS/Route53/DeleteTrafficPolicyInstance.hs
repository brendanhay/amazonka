{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteTrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a traffic policy instance and all of the resource record sets that Amazon Route 53 created when you created the instance.
module Network.AWS.Route53.DeleteTrafficPolicyInstance
    (
    -- * Creating a request
      DeleteTrafficPolicyInstance (..)
    , mkDeleteTrafficPolicyInstance
    -- ** Request lenses
    , dtpiId

    -- * Destructuring the response
    , DeleteTrafficPolicyInstanceResponse (..)
    , mkDeleteTrafficPolicyInstanceResponse
    -- ** Response lenses
    , dtpirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to delete a specified traffic policy instance.
--
-- /See:/ 'mkDeleteTrafficPolicyInstance' smart constructor.
newtype DeleteTrafficPolicyInstance = DeleteTrafficPolicyInstance'
  { id :: Types.TrafficPolicyInstanceId
    -- ^ The ID of the traffic policy instance that you want to delete. 
--
-- /Important:/ When you delete a traffic policy instance, Amazon Route 53 also deletes all of the resource record sets that were created when you created the traffic policy instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficPolicyInstance' value with any optional fields omitted.
mkDeleteTrafficPolicyInstance
    :: Types.TrafficPolicyInstanceId -- ^ 'id'
    -> DeleteTrafficPolicyInstance
mkDeleteTrafficPolicyInstance id = DeleteTrafficPolicyInstance'{id}

-- | The ID of the traffic policy instance that you want to delete. 
--
-- /Important:/ When you delete a traffic policy instance, Amazon Route 53 also deletes all of the resource record sets that were created when you created the traffic policy instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpiId :: Lens.Lens' DeleteTrafficPolicyInstance Types.TrafficPolicyInstanceId
dtpiId = Lens.field @"id"
{-# INLINEABLE dtpiId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeleteTrafficPolicyInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTrafficPolicyInstance where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTrafficPolicyInstance where
        type Rs DeleteTrafficPolicyInstance =
             DeleteTrafficPolicyInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2013-04-01/trafficpolicyinstance/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteTrafficPolicyInstanceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element.
--
-- /See:/ 'mkDeleteTrafficPolicyInstanceResponse' smart constructor.
newtype DeleteTrafficPolicyInstanceResponse = DeleteTrafficPolicyInstanceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrafficPolicyInstanceResponse' value with any optional fields omitted.
mkDeleteTrafficPolicyInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTrafficPolicyInstanceResponse
mkDeleteTrafficPolicyInstanceResponse responseStatus
  = DeleteTrafficPolicyInstanceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpirrsResponseStatus :: Lens.Lens' DeleteTrafficPolicyInstanceResponse Core.Int
dtpirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtpirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
