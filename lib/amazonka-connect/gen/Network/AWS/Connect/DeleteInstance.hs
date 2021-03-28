{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DeleteInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Amazon Connect instance.
module Network.AWS.Connect.DeleteInstance
    (
    -- * Creating a request
      DeleteInstance (..)
    , mkDeleteInstance
    -- ** Request lenses
    , dInstanceId

    -- * Destructuring the response
    , DeleteInstanceResponse (..)
    , mkDeleteInstanceResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInstance' smart constructor.
newtype DeleteInstance = DeleteInstance'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstance' value with any optional fields omitted.
mkDeleteInstance
    :: Types.InstanceId -- ^ 'instanceId'
    -> DeleteInstance
mkDeleteInstance instanceId = DeleteInstance'{instanceId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInstanceId :: Lens.Lens' DeleteInstance Types.InstanceId
dInstanceId = Lens.field @"instanceId"
{-# INLINEABLE dInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery DeleteInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteInstance where
        toHeaders DeleteInstance{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteInstance where
        type Rs DeleteInstance = DeleteInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/instance/" Core.<> Core.toText instanceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteInstanceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstanceResponse' value with any optional fields omitted.
mkDeleteInstanceResponse
    :: DeleteInstanceResponse
mkDeleteInstanceResponse = DeleteInstanceResponse'
