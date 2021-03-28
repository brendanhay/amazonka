{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateApprovedOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an approved origin to an Amazon Connect instance.
module Network.AWS.Connect.AssociateApprovedOrigin
    (
    -- * Creating a request
      AssociateApprovedOrigin (..)
    , mkAssociateApprovedOrigin
    -- ** Request lenses
    , aaoInstanceId
    , aaoOrigin

    -- * Destructuring the response
    , AssociateApprovedOriginResponse (..)
    , mkAssociateApprovedOriginResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateApprovedOrigin' smart constructor.
data AssociateApprovedOrigin = AssociateApprovedOrigin'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , origin :: Types.Origin
    -- ^ The domain to add to your allow list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateApprovedOrigin' value with any optional fields omitted.
mkAssociateApprovedOrigin
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.Origin -- ^ 'origin'
    -> AssociateApprovedOrigin
mkAssociateApprovedOrigin instanceId origin
  = AssociateApprovedOrigin'{instanceId, origin}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoInstanceId :: Lens.Lens' AssociateApprovedOrigin Types.InstanceId
aaoInstanceId = Lens.field @"instanceId"
{-# INLINEABLE aaoInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The domain to add to your allow list.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoOrigin :: Lens.Lens' AssociateApprovedOrigin Types.Origin
aaoOrigin = Lens.field @"origin"
{-# INLINEABLE aaoOrigin #-}
{-# DEPRECATED origin "Use generic-lens or generic-optics with 'origin' instead"  #-}

instance Core.ToQuery AssociateApprovedOrigin where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateApprovedOrigin where
        toHeaders AssociateApprovedOrigin{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateApprovedOrigin where
        toJSON AssociateApprovedOrigin{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Origin" Core..= origin)])

instance Core.AWSRequest AssociateApprovedOrigin where
        type Rs AssociateApprovedOrigin = AssociateApprovedOriginResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<>
                             "/approved-origin",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull AssociateApprovedOriginResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateApprovedOriginResponse' smart constructor.
data AssociateApprovedOriginResponse = AssociateApprovedOriginResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateApprovedOriginResponse' value with any optional fields omitted.
mkAssociateApprovedOriginResponse
    :: AssociateApprovedOriginResponse
mkAssociateApprovedOriginResponse
  = AssociateApprovedOriginResponse'
