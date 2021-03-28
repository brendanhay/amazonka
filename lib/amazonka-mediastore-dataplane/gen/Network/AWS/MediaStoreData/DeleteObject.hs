{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.DeleteObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an object at the specified path.
module Network.AWS.MediaStoreData.DeleteObject
    (
    -- * Creating a request
      DeleteObject (..)
    , mkDeleteObject
    -- ** Request lenses
    , doPath

    -- * Destructuring the response
    , DeleteObjectResponse (..)
    , mkDeleteObjectResponse
    -- ** Response lenses
    , dorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStoreData.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteObject' smart constructor.
newtype DeleteObject = DeleteObject'
  { path :: Types.PathNaming
    -- ^ The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteObject' value with any optional fields omitted.
mkDeleteObject
    :: Types.PathNaming -- ^ 'path'
    -> DeleteObject
mkDeleteObject path = DeleteObject'{path}

-- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doPath :: Lens.Lens' DeleteObject Types.PathNaming
doPath = Lens.field @"path"
{-# INLINEABLE doPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.ToQuery DeleteObject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteObject where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteObject where
        type Rs DeleteObject = DeleteObjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/" Core.<> Core.toText path,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteObjectResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteObjectResponse' smart constructor.
newtype DeleteObjectResponse = DeleteObjectResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteObjectResponse' value with any optional fields omitted.
mkDeleteObjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteObjectResponse
mkDeleteObjectResponse responseStatus
  = DeleteObjectResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsResponseStatus :: Lens.Lens' DeleteObjectResponse Core.Int
dorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
