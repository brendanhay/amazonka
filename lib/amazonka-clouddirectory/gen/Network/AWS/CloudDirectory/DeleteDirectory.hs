{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DeleteDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a directory. Only disabled directories can be deleted. A deleted directory cannot be undone. Exercise extreme caution when deleting directories.
module Network.AWS.CloudDirectory.DeleteDirectory
    (
    -- * Creating a request
      DeleteDirectory (..)
    , mkDeleteDirectory
    -- ** Request lenses
    , ddfDirectoryArn

    -- * Destructuring the response
    , DeleteDirectoryResponse (..)
    , mkDeleteDirectoryResponse
    -- ** Response lenses
    , ddrrsDirectoryArn
    , ddrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDirectory' smart constructor.
newtype DeleteDirectory = DeleteDirectory'
  { directoryArn :: Types.Arn
    -- ^ The ARN of the directory to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectory' value with any optional fields omitted.
mkDeleteDirectory
    :: Types.Arn -- ^ 'directoryArn'
    -> DeleteDirectory
mkDeleteDirectory directoryArn = DeleteDirectory'{directoryArn}

-- | The ARN of the directory to delete.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDirectoryArn :: Lens.Lens' DeleteDirectory Types.Arn
ddfDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE ddfDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

instance Core.ToQuery DeleteDirectory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDirectory where
        toHeaders DeleteDirectory{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON DeleteDirectory where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DeleteDirectory where
        type Rs DeleteDirectory = DeleteDirectoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/directory",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteDirectoryResponse' Core.<$>
                   (x Core..: "DirectoryArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDirectoryResponse' smart constructor.
data DeleteDirectoryResponse = DeleteDirectoryResponse'
  { directoryArn :: Types.Arn
    -- ^ The ARN of the deleted directory.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectoryResponse' value with any optional fields omitted.
mkDeleteDirectoryResponse
    :: Types.Arn -- ^ 'directoryArn'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteDirectoryResponse
mkDeleteDirectoryResponse directoryArn responseStatus
  = DeleteDirectoryResponse'{directoryArn, responseStatus}

-- | The ARN of the deleted directory.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDirectoryArn :: Lens.Lens' DeleteDirectoryResponse Types.Arn
ddrrsDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE ddrrsDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DeleteDirectoryResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
