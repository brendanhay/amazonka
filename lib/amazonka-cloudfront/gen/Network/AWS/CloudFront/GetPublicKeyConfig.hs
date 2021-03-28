{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetPublicKeyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a public key configuration.
module Network.AWS.CloudFront.GetPublicKeyConfig
    (
    -- * Creating a request
      GetPublicKeyConfig (..)
    , mkGetPublicKeyConfig
    -- ** Request lenses
    , gpkcId

    -- * Destructuring the response
    , GetPublicKeyConfigResponse (..)
    , mkGetPublicKeyConfigResponse
    -- ** Response lenses
    , gpkcrrsETag
    , gpkcrrsPublicKeyConfig
    , gpkcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPublicKeyConfig' smart constructor.
newtype GetPublicKeyConfig = GetPublicKeyConfig'
  { id :: Core.Text
    -- ^ The identifier of the public key whose configuration you are getting.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPublicKeyConfig' value with any optional fields omitted.
mkGetPublicKeyConfig
    :: Core.Text -- ^ 'id'
    -> GetPublicKeyConfig
mkGetPublicKeyConfig id = GetPublicKeyConfig'{id}

-- | The identifier of the public key whose configuration you are getting.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcId :: Lens.Lens' GetPublicKeyConfig Core.Text
gpkcId = Lens.field @"id"
{-# INLINEABLE gpkcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetPublicKeyConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPublicKeyConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetPublicKeyConfig where
        type Rs GetPublicKeyConfig = GetPublicKeyConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/public-key/" Core.<> Core.toText id Core.<> "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetPublicKeyConfigResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPublicKeyConfigResponse' smart constructor.
data GetPublicKeyConfigResponse = GetPublicKeyConfigResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The identifier for this version of the public key configuration.
  , publicKeyConfig :: Core.Maybe Types.PublicKeyConfig
    -- ^ A public key configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPublicKeyConfigResponse' value with any optional fields omitted.
mkGetPublicKeyConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPublicKeyConfigResponse
mkGetPublicKeyConfigResponse responseStatus
  = GetPublicKeyConfigResponse'{eTag = Core.Nothing,
                                publicKeyConfig = Core.Nothing, responseStatus}

-- | The identifier for this version of the public key configuration.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcrrsETag :: Lens.Lens' GetPublicKeyConfigResponse (Core.Maybe Core.Text)
gpkcrrsETag = Lens.field @"eTag"
{-# INLINEABLE gpkcrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | A public key configuration.
--
-- /Note:/ Consider using 'publicKeyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcrrsPublicKeyConfig :: Lens.Lens' GetPublicKeyConfigResponse (Core.Maybe Types.PublicKeyConfig)
gpkcrrsPublicKeyConfig = Lens.field @"publicKeyConfig"
{-# INLINEABLE gpkcrrsPublicKeyConfig #-}
{-# DEPRECATED publicKeyConfig "Use generic-lens or generic-optics with 'publicKeyConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcrrsResponseStatus :: Lens.Lens' GetPublicKeyConfigResponse Core.Int
gpkcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpkcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
