{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Directory Service directory.
--
-- Before you call @DeleteDirectory@ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the @DeleteDirectory@ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
module Network.AWS.DirectoryService.DeleteDirectory
    (
    -- * Creating a request
      DeleteDirectory (..)
    , mkDeleteDirectory
    -- ** Request lenses
    , ddfDirectoryId

    -- * Destructuring the response
    , DeleteDirectoryResponse (..)
    , mkDeleteDirectoryResponse
    -- ** Response lenses
    , ddrfrsDirectoryId
    , ddrfrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DeleteDirectory' operation.
--
-- /See:/ 'mkDeleteDirectory' smart constructor.
newtype DeleteDirectory = DeleteDirectory'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectory' value with any optional fields omitted.
mkDeleteDirectory
    :: Types.DirectoryId -- ^ 'directoryId'
    -> DeleteDirectory
mkDeleteDirectory directoryId = DeleteDirectory'{directoryId}

-- | The identifier of the directory to delete.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDirectoryId :: Lens.Lens' DeleteDirectory Types.DirectoryId
ddfDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE ddfDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

instance Core.ToQuery DeleteDirectory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDirectory where
        toHeaders DeleteDirectory{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DeleteDirectory")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDirectory where
        toJSON DeleteDirectory{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DirectoryId" Core..= directoryId)])

instance Core.AWSRequest DeleteDirectory where
        type Rs DeleteDirectory = DeleteDirectoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteDirectoryResponse' Core.<$>
                   (x Core..:? "DirectoryId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the results of the 'DeleteDirectory' operation.
--
-- /See:/ 'mkDeleteDirectoryResponse' smart constructor.
data DeleteDirectoryResponse = DeleteDirectoryResponse'
  { directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The directory identifier.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectoryResponse' value with any optional fields omitted.
mkDeleteDirectoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDirectoryResponse
mkDeleteDirectoryResponse responseStatus
  = DeleteDirectoryResponse'{directoryId = Core.Nothing,
                             responseStatus}

-- | The directory identifier.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsDirectoryId :: Lens.Lens' DeleteDirectoryResponse (Core.Maybe Types.DirectoryId)
ddrfrsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE ddrfrsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsResponseStatus :: Lens.Lens' DeleteDirectoryResponse Core.Int
ddrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
