{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DeleteAccessPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified access point. After deletion is complete, new clients can no longer connect to the access points. Clients connected to the access point at the time of deletion will continue to function until they terminate their connection.
--
-- This operation requires permissions for the @elasticfilesystem:DeleteAccessPoint@ action.
module Network.AWS.EFS.DeleteAccessPoint
    (
    -- * Creating a request
      DeleteAccessPoint (..)
    , mkDeleteAccessPoint
    -- ** Request lenses
    , dAccessPointId

    -- * Destructuring the response
    , DeleteAccessPointResponse (..)
    , mkDeleteAccessPointResponse
    ) where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAccessPoint' smart constructor.
newtype DeleteAccessPoint = DeleteAccessPoint'
  { accessPointId :: Types.AccessPointId
    -- ^ The ID of the access point that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccessPoint' value with any optional fields omitted.
mkDeleteAccessPoint
    :: Types.AccessPointId -- ^ 'accessPointId'
    -> DeleteAccessPoint
mkDeleteAccessPoint accessPointId
  = DeleteAccessPoint'{accessPointId}

-- | The ID of the access point that you want to delete.
--
-- /Note:/ Consider using 'accessPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccessPointId :: Lens.Lens' DeleteAccessPoint Types.AccessPointId
dAccessPointId = Lens.field @"accessPointId"
{-# INLINEABLE dAccessPointId #-}
{-# DEPRECATED accessPointId "Use generic-lens or generic-optics with 'accessPointId' instead"  #-}

instance Core.ToQuery DeleteAccessPoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAccessPoint where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteAccessPoint where
        type Rs DeleteAccessPoint = DeleteAccessPointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2015-02-01/access-points/" Core.<> Core.toText accessPointId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteAccessPointResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAccessPointResponse' smart constructor.
data DeleteAccessPointResponse = DeleteAccessPointResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccessPointResponse' value with any optional fields omitted.
mkDeleteAccessPointResponse
    :: DeleteAccessPointResponse
mkDeleteAccessPointResponse = DeleteAccessPointResponse'
