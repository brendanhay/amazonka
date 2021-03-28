{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about a directory.
module Network.AWS.CloudDirectory.GetDirectory
    (
    -- * Creating a request
      GetDirectory (..)
    , mkGetDirectory
    -- ** Request lenses
    , gdDirectoryArn

    -- * Destructuring the response
    , GetDirectoryResponse (..)
    , mkGetDirectoryResponse
    -- ** Response lenses
    , gdrrsDirectory
    , gdrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDirectory' smart constructor.
newtype GetDirectory = GetDirectory'
  { directoryArn :: Types.DirectoryArn
    -- ^ The ARN of the directory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDirectory' value with any optional fields omitted.
mkGetDirectory
    :: Types.DirectoryArn -- ^ 'directoryArn'
    -> GetDirectory
mkGetDirectory directoryArn = GetDirectory'{directoryArn}

-- | The ARN of the directory.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDirectoryArn :: Lens.Lens' GetDirectory Types.DirectoryArn
gdDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE gdDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

instance Core.ToQuery GetDirectory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDirectory where
        toHeaders GetDirectory{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON GetDirectory where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetDirectory where
        type Rs GetDirectory = GetDirectoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/directory/get",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDirectoryResponse' Core.<$>
                   (x Core..: "Directory") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDirectoryResponse' smart constructor.
data GetDirectoryResponse = GetDirectoryResponse'
  { directory :: Types.Directory
    -- ^ Metadata about the directory.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDirectoryResponse' value with any optional fields omitted.
mkGetDirectoryResponse
    :: Types.Directory -- ^ 'directory'
    -> Core.Int -- ^ 'responseStatus'
    -> GetDirectoryResponse
mkGetDirectoryResponse directory responseStatus
  = GetDirectoryResponse'{directory, responseStatus}

-- | Metadata about the directory.
--
-- /Note:/ Consider using 'directory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDirectory :: Lens.Lens' GetDirectoryResponse Types.Directory
gdrrsDirectory = Lens.field @"directory"
{-# INLINEABLE gdrrsDirectory #-}
{-# DEPRECATED directory "Use generic-lens or generic-optics with 'directory' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDirectoryResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
