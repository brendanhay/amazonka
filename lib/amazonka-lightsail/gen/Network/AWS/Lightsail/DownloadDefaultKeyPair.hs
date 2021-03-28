{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DownloadDefaultKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the default SSH key pair from the user's account.
module Network.AWS.Lightsail.DownloadDefaultKeyPair
    (
    -- * Creating a request
      DownloadDefaultKeyPair (..)
    , mkDownloadDefaultKeyPair

    -- * Destructuring the response
    , DownloadDefaultKeyPairResponse (..)
    , mkDownloadDefaultKeyPairResponse
    -- ** Response lenses
    , ddkprrsPrivateKeyBase64
    , ddkprrsPublicKeyBase64
    , ddkprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDownloadDefaultKeyPair' smart constructor.
data DownloadDefaultKeyPair = DownloadDefaultKeyPair'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DownloadDefaultKeyPair' value with any optional fields omitted.
mkDownloadDefaultKeyPair
    :: DownloadDefaultKeyPair
mkDownloadDefaultKeyPair = DownloadDefaultKeyPair'

instance Core.ToQuery DownloadDefaultKeyPair where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DownloadDefaultKeyPair where
        toHeaders DownloadDefaultKeyPair{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.DownloadDefaultKeyPair")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DownloadDefaultKeyPair where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DownloadDefaultKeyPair where
        type Rs DownloadDefaultKeyPair = DownloadDefaultKeyPairResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DownloadDefaultKeyPairResponse' Core.<$>
                   (x Core..:? "privateKeyBase64") Core.<*>
                     x Core..:? "publicKeyBase64"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDownloadDefaultKeyPairResponse' smart constructor.
data DownloadDefaultKeyPairResponse = DownloadDefaultKeyPairResponse'
  { privateKeyBase64 :: Core.Maybe Types.PrivateKeyBase64
    -- ^ A base64-encoded RSA private key.
  , publicKeyBase64 :: Core.Maybe Types.PublicKeyBase64
    -- ^ A base64-encoded public key of the @ssh-rsa@ type.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DownloadDefaultKeyPairResponse' value with any optional fields omitted.
mkDownloadDefaultKeyPairResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DownloadDefaultKeyPairResponse
mkDownloadDefaultKeyPairResponse responseStatus
  = DownloadDefaultKeyPairResponse'{privateKeyBase64 = Core.Nothing,
                                    publicKeyBase64 = Core.Nothing, responseStatus}

-- | A base64-encoded RSA private key.
--
-- /Note:/ Consider using 'privateKeyBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddkprrsPrivateKeyBase64 :: Lens.Lens' DownloadDefaultKeyPairResponse (Core.Maybe Types.PrivateKeyBase64)
ddkprrsPrivateKeyBase64 = Lens.field @"privateKeyBase64"
{-# INLINEABLE ddkprrsPrivateKeyBase64 #-}
{-# DEPRECATED privateKeyBase64 "Use generic-lens or generic-optics with 'privateKeyBase64' instead"  #-}

-- | A base64-encoded public key of the @ssh-rsa@ type.
--
-- /Note:/ Consider using 'publicKeyBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddkprrsPublicKeyBase64 :: Lens.Lens' DownloadDefaultKeyPairResponse (Core.Maybe Types.PublicKeyBase64)
ddkprrsPublicKeyBase64 = Lens.field @"publicKeyBase64"
{-# INLINEABLE ddkprrsPublicKeyBase64 #-}
{-# DEPRECATED publicKeyBase64 "Use generic-lens or generic-optics with 'publicKeyBase64' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddkprrsResponseStatus :: Lens.Lens' DownloadDefaultKeyPairResponse Core.Int
ddkprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddkprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
