{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CancelSchemaExtension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an in-progress schema extension to a Microsoft AD directory. Once a schema extension has started replicating to all domain controllers, the task can no longer be canceled. A schema extension can be canceled during any of the following states; @Initializing@ , @CreatingSnapshot@ , and @UpdatingSchema@ .
module Network.AWS.DirectoryService.CancelSchemaExtension
    (
    -- * Creating a request
      CancelSchemaExtension (..)
    , mkCancelSchemaExtension
    -- ** Request lenses
    , cseDirectoryId
    , cseSchemaExtensionId

    -- * Destructuring the response
    , CancelSchemaExtensionResponse (..)
    , mkCancelSchemaExtensionResponse
    -- ** Response lenses
    , cserrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelSchemaExtension' smart constructor.
data CancelSchemaExtension = CancelSchemaExtension'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory whose schema extension will be canceled.
  , schemaExtensionId :: Types.SchemaExtensionId
    -- ^ The identifier of the schema extension that will be canceled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSchemaExtension' value with any optional fields omitted.
mkCancelSchemaExtension
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.SchemaExtensionId -- ^ 'schemaExtensionId'
    -> CancelSchemaExtension
mkCancelSchemaExtension directoryId schemaExtensionId
  = CancelSchemaExtension'{directoryId, schemaExtensionId}

-- | The identifier of the directory whose schema extension will be canceled.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseDirectoryId :: Lens.Lens' CancelSchemaExtension Types.DirectoryId
cseDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE cseDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The identifier of the schema extension that will be canceled.
--
-- /Note:/ Consider using 'schemaExtensionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseSchemaExtensionId :: Lens.Lens' CancelSchemaExtension Types.SchemaExtensionId
cseSchemaExtensionId = Lens.field @"schemaExtensionId"
{-# INLINEABLE cseSchemaExtensionId #-}
{-# DEPRECATED schemaExtensionId "Use generic-lens or generic-optics with 'schemaExtensionId' instead"  #-}

instance Core.ToQuery CancelSchemaExtension where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelSchemaExtension where
        toHeaders CancelSchemaExtension{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.CancelSchemaExtension")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelSchemaExtension where
        toJSON CancelSchemaExtension{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("SchemaExtensionId" Core..= schemaExtensionId)])

instance Core.AWSRequest CancelSchemaExtension where
        type Rs CancelSchemaExtension = CancelSchemaExtensionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CancelSchemaExtensionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelSchemaExtensionResponse' smart constructor.
newtype CancelSchemaExtensionResponse = CancelSchemaExtensionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSchemaExtensionResponse' value with any optional fields omitted.
mkCancelSchemaExtensionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelSchemaExtensionResponse
mkCancelSchemaExtensionResponse responseStatus
  = CancelSchemaExtensionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cserrsResponseStatus :: Lens.Lens' CancelSchemaExtensionResponse Core.Int
cserrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cserrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
