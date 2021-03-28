{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerAPIMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Amazon Lightsail containers, such as the current version of the Lightsail Control (lightsailctl) plugin.
module Network.AWS.Lightsail.GetContainerAPIMetadata
    (
    -- * Creating a request
      GetContainerAPIMetadata (..)
    , mkGetContainerAPIMetadata

    -- * Destructuring the response
    , GetContainerAPIMetadataResponse (..)
    , mkGetContainerAPIMetadataResponse
    -- ** Response lenses
    , gcapimrrsMetadata
    , gcapimrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContainerAPIMetadata' smart constructor.
data GetContainerAPIMetadata = GetContainerAPIMetadata'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerAPIMetadata' value with any optional fields omitted.
mkGetContainerAPIMetadata
    :: GetContainerAPIMetadata
mkGetContainerAPIMetadata = GetContainerAPIMetadata'

instance Core.ToQuery GetContainerAPIMetadata where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetContainerAPIMetadata where
        toHeaders GetContainerAPIMetadata{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.GetContainerAPIMetadata")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetContainerAPIMetadata where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetContainerAPIMetadata where
        type Rs GetContainerAPIMetadata = GetContainerAPIMetadataResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetContainerAPIMetadataResponse' Core.<$>
                   (x Core..:? "metadata") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetContainerAPIMetadataResponse' smart constructor.
data GetContainerAPIMetadataResponse = GetContainerAPIMetadataResponse'
  { metadata :: Core.Maybe [Core.HashMap Core.Text Core.Text]
    -- ^ Metadata about Lightsail containers, such as the current version of the Lightsail Control (lightsailctl) plugin.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerAPIMetadataResponse' value with any optional fields omitted.
mkGetContainerAPIMetadataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetContainerAPIMetadataResponse
mkGetContainerAPIMetadataResponse responseStatus
  = GetContainerAPIMetadataResponse'{metadata = Core.Nothing,
                                     responseStatus}

-- | Metadata about Lightsail containers, such as the current version of the Lightsail Control (lightsailctl) plugin.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcapimrrsMetadata :: Lens.Lens' GetContainerAPIMetadataResponse (Core.Maybe [Core.HashMap Core.Text Core.Text])
gcapimrrsMetadata = Lens.field @"metadata"
{-# INLINEABLE gcapimrrsMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcapimrrsResponseStatus :: Lens.Lens' GetContainerAPIMetadataResponse Core.Int
gcapimrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcapimrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
