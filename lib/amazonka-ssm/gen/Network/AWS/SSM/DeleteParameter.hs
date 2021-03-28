{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a parameter from the system.
module Network.AWS.SSM.DeleteParameter
    (
    -- * Creating a request
      DeleteParameter (..)
    , mkDeleteParameter
    -- ** Request lenses
    , dpfName

    -- * Destructuring the response
    , DeleteParameterResponse (..)
    , mkDeleteParameterResponse
    -- ** Response lenses
    , dprgrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeleteParameter' smart constructor.
newtype DeleteParameter = DeleteParameter'
  { name :: Types.PSParameterName
    -- ^ The name of the parameter to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteParameter' value with any optional fields omitted.
mkDeleteParameter
    :: Types.PSParameterName -- ^ 'name'
    -> DeleteParameter
mkDeleteParameter name = DeleteParameter'{name}

-- | The name of the parameter to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfName :: Lens.Lens' DeleteParameter Types.PSParameterName
dpfName = Lens.field @"name"
{-# INLINEABLE dpfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteParameter where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteParameter where
        toHeaders DeleteParameter{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DeleteParameter") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteParameter where
        toJSON DeleteParameter{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteParameter where
        type Rs DeleteParameter = DeleteParameterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteParameterResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteParameterResponse' smart constructor.
newtype DeleteParameterResponse = DeleteParameterResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteParameterResponse' value with any optional fields omitted.
mkDeleteParameterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteParameterResponse
mkDeleteParameterResponse responseStatus
  = DeleteParameterResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprgrsResponseStatus :: Lens.Lens' DeleteParameterResponse Core.Int
dprgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
