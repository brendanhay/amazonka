{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteUsagePlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage plan of a given plan Id.
module Network.AWS.ApiGateway.DeleteUsagePlan
    (
    -- * Creating a request
      DeleteUsagePlan (..)
    , mkDeleteUsagePlan
    -- ** Request lenses
    , dupUsagePlanId

    -- * Destructuring the response
    , DeleteUsagePlanResponse (..)
    , mkDeleteUsagePlanResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The DELETE request to delete a usage plan of a given plan Id.
--
-- /See:/ 'mkDeleteUsagePlan' smart constructor.
newtype DeleteUsagePlan = DeleteUsagePlan'
  { usagePlanId :: Core.Text
    -- ^ [Required] The Id of the to-be-deleted usage plan.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUsagePlan' value with any optional fields omitted.
mkDeleteUsagePlan
    :: Core.Text -- ^ 'usagePlanId'
    -> DeleteUsagePlan
mkDeleteUsagePlan usagePlanId = DeleteUsagePlan'{usagePlanId}

-- | [Required] The Id of the to-be-deleted usage plan.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupUsagePlanId :: Lens.Lens' DeleteUsagePlan Core.Text
dupUsagePlanId = Lens.field @"usagePlanId"
{-# INLINEABLE dupUsagePlanId #-}
{-# DEPRECATED usagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead"  #-}

instance Core.ToQuery DeleteUsagePlan where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUsagePlan where
        toHeaders DeleteUsagePlan{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteUsagePlan where
        type Rs DeleteUsagePlan = DeleteUsagePlanResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/usageplans/" Core.<> Core.toText usagePlanId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteUsagePlanResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUsagePlanResponse' smart constructor.
data DeleteUsagePlanResponse = DeleteUsagePlanResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUsagePlanResponse' value with any optional fields omitted.
mkDeleteUsagePlanResponse
    :: DeleteUsagePlanResponse
mkDeleteUsagePlanResponse = DeleteUsagePlanResponse'
