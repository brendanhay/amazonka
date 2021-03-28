{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DisableDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified directory. Disabled directories cannot be read or written to. Only enabled directories can be disabled. Disabled directories may be reenabled.
module Network.AWS.CloudDirectory.DisableDirectory
    (
    -- * Creating a request
      DisableDirectory (..)
    , mkDisableDirectory
    -- ** Request lenses
    , ddDirectoryArn

    -- * Destructuring the response
    , DisableDirectoryResponse (..)
    , mkDisableDirectoryResponse
    -- ** Response lenses
    , drsDirectoryArn
    , drsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableDirectory' smart constructor.
newtype DisableDirectory = DisableDirectory'
  { directoryArn :: Types.DirectoryArn
    -- ^ The ARN of the directory to disable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableDirectory' value with any optional fields omitted.
mkDisableDirectory
    :: Types.DirectoryArn -- ^ 'directoryArn'
    -> DisableDirectory
mkDisableDirectory directoryArn = DisableDirectory'{directoryArn}

-- | The ARN of the directory to disable.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDirectoryArn :: Lens.Lens' DisableDirectory Types.DirectoryArn
ddDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE ddDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

instance Core.ToQuery DisableDirectory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableDirectory where
        toHeaders DisableDirectory{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON DisableDirectory where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DisableDirectory where
        type Rs DisableDirectory = DisableDirectoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/amazonclouddirectory/2017-01-11/directory/disable",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DisableDirectoryResponse' Core.<$>
                   (x Core..: "DirectoryArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableDirectoryResponse' smart constructor.
data DisableDirectoryResponse = DisableDirectoryResponse'
  { directoryArn :: Types.Arn
    -- ^ The ARN of the directory that has been disabled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableDirectoryResponse' value with any optional fields omitted.
mkDisableDirectoryResponse
    :: Types.Arn -- ^ 'directoryArn'
    -> Core.Int -- ^ 'responseStatus'
    -> DisableDirectoryResponse
mkDisableDirectoryResponse directoryArn responseStatus
  = DisableDirectoryResponse'{directoryArn, responseStatus}

-- | The ARN of the directory that has been disabled.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDirectoryArn :: Lens.Lens' DisableDirectoryResponse Types.Arn
drsDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE drsDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DisableDirectoryResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
