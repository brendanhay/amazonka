{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a public key.
module Network.AWS.CloudFront.GetPublicKey
    (
    -- * Creating a request
      GetPublicKey (..)
    , mkGetPublicKey
    -- ** Request lenses
    , gpkId

    -- * Destructuring the response
    , GetPublicKeyResponse (..)
    , mkGetPublicKeyResponse
    -- ** Response lenses
    , gpkrrsETag
    , gpkrrsPublicKey
    , gpkrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPublicKey' smart constructor.
newtype GetPublicKey = GetPublicKey'
  { id :: Core.Text
    -- ^ The identifier of the public key you are getting.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPublicKey' value with any optional fields omitted.
mkGetPublicKey
    :: Core.Text -- ^ 'id'
    -> GetPublicKey
mkGetPublicKey id = GetPublicKey'{id}

-- | The identifier of the public key you are getting.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkId :: Lens.Lens' GetPublicKey Core.Text
gpkId = Lens.field @"id"
{-# INLINEABLE gpkId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetPublicKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPublicKey where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetPublicKey where
        type Rs GetPublicKey = GetPublicKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2020-05-31/public-key/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetPublicKeyResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPublicKeyResponse' smart constructor.
data GetPublicKeyResponse = GetPublicKeyResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The identifier for this version of the public key.
  , publicKey :: Core.Maybe Types.PublicKey
    -- ^ The public key.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetPublicKeyResponse' value with any optional fields omitted.
mkGetPublicKeyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPublicKeyResponse
mkGetPublicKeyResponse responseStatus
  = GetPublicKeyResponse'{eTag = Core.Nothing,
                          publicKey = Core.Nothing, responseStatus}

-- | The identifier for this version of the public key.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrrsETag :: Lens.Lens' GetPublicKeyResponse (Core.Maybe Core.Text)
gpkrrsETag = Lens.field @"eTag"
{-# INLINEABLE gpkrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The public key.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrrsPublicKey :: Lens.Lens' GetPublicKeyResponse (Core.Maybe Types.PublicKey)
gpkrrsPublicKey = Lens.field @"publicKey"
{-# INLINEABLE gpkrrsPublicKey #-}
{-# DEPRECATED publicKey "Use generic-lens or generic-optics with 'publicKey' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrrsResponseStatus :: Lens.Lens' GetPublicKeyResponse Core.Int
gpkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
