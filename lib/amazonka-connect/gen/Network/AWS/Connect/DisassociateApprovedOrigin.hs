{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateApprovedOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes access to integrated applications from Amazon Connect.
module Network.AWS.Connect.DisassociateApprovedOrigin
    (
    -- * Creating a request
      DisassociateApprovedOrigin (..)
    , mkDisassociateApprovedOrigin
    -- ** Request lenses
    , daoInstanceId
    , daoOrigin

    -- * Destructuring the response
    , DisassociateApprovedOriginResponse (..)
    , mkDisassociateApprovedOriginResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateApprovedOrigin' smart constructor.
data DisassociateApprovedOrigin = DisassociateApprovedOrigin'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , origin :: Types.Origin
    -- ^ The domain URL of the integrated application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateApprovedOrigin' value with any optional fields omitted.
mkDisassociateApprovedOrigin
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.Origin -- ^ 'origin'
    -> DisassociateApprovedOrigin
mkDisassociateApprovedOrigin instanceId origin
  = DisassociateApprovedOrigin'{instanceId, origin}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoInstanceId :: Lens.Lens' DisassociateApprovedOrigin Types.InstanceId
daoInstanceId = Lens.field @"instanceId"
{-# INLINEABLE daoInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The domain URL of the integrated application.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoOrigin :: Lens.Lens' DisassociateApprovedOrigin Types.Origin
daoOrigin = Lens.field @"origin"
{-# INLINEABLE daoOrigin #-}
{-# DEPRECATED origin "Use generic-lens or generic-optics with 'origin' instead"  #-}

instance Core.ToQuery DisassociateApprovedOrigin where
        toQuery DisassociateApprovedOrigin{..}
          = Core.toQueryPair "origin" origin

instance Core.ToHeaders DisassociateApprovedOrigin where
        toHeaders DisassociateApprovedOrigin{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DisassociateApprovedOrigin where
        type Rs DisassociateApprovedOrigin =
             DisassociateApprovedOriginResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<>
                             "/approved-origin",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DisassociateApprovedOriginResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateApprovedOriginResponse' smart constructor.
data DisassociateApprovedOriginResponse = DisassociateApprovedOriginResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateApprovedOriginResponse' value with any optional fields omitted.
mkDisassociateApprovedOriginResponse
    :: DisassociateApprovedOriginResponse
mkDisassociateApprovedOriginResponse
  = DisassociateApprovedOriginResponse'
