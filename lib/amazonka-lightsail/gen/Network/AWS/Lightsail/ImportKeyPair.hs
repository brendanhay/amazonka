{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.ImportKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a public SSH key from a specific key pair.
module Network.AWS.Lightsail.ImportKeyPair
    (
    -- * Creating a request
      ImportKeyPair (..)
    , mkImportKeyPair
    -- ** Request lenses
    , ikpKeyPairName
    , ikpPublicKeyBase64

    -- * Destructuring the response
    , ImportKeyPairResponse (..)
    , mkImportKeyPairResponse
    -- ** Response lenses
    , ikprrsOperation
    , ikprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportKeyPair' smart constructor.
data ImportKeyPair = ImportKeyPair'
  { keyPairName :: Types.ResourceName
    -- ^ The name of the key pair for which you want to import the public key.
  , publicKeyBase64 :: Types.Base64
    -- ^ A base64-encoded public key of the @ssh-rsa@ type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportKeyPair' value with any optional fields omitted.
mkImportKeyPair
    :: Types.ResourceName -- ^ 'keyPairName'
    -> Types.Base64 -- ^ 'publicKeyBase64'
    -> ImportKeyPair
mkImportKeyPair keyPairName publicKeyBase64
  = ImportKeyPair'{keyPairName, publicKeyBase64}

-- | The name of the key pair for which you want to import the public key.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpKeyPairName :: Lens.Lens' ImportKeyPair Types.ResourceName
ikpKeyPairName = Lens.field @"keyPairName"
{-# INLINEABLE ikpKeyPairName #-}
{-# DEPRECATED keyPairName "Use generic-lens or generic-optics with 'keyPairName' instead"  #-}

-- | A base64-encoded public key of the @ssh-rsa@ type.
--
-- /Note:/ Consider using 'publicKeyBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpPublicKeyBase64 :: Lens.Lens' ImportKeyPair Types.Base64
ikpPublicKeyBase64 = Lens.field @"publicKeyBase64"
{-# INLINEABLE ikpPublicKeyBase64 #-}
{-# DEPRECATED publicKeyBase64 "Use generic-lens or generic-optics with 'publicKeyBase64' instead"  #-}

instance Core.ToQuery ImportKeyPair where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ImportKeyPair where
        toHeaders ImportKeyPair{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.ImportKeyPair")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ImportKeyPair where
        toJSON ImportKeyPair{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("keyPairName" Core..= keyPairName),
                  Core.Just ("publicKeyBase64" Core..= publicKeyBase64)])

instance Core.AWSRequest ImportKeyPair where
        type Rs ImportKeyPair = ImportKeyPairResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ImportKeyPairResponse' Core.<$>
                   (x Core..:? "operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportKeyPairResponse' smart constructor.
data ImportKeyPairResponse = ImportKeyPairResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ImportKeyPairResponse' value with any optional fields omitted.
mkImportKeyPairResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportKeyPairResponse
mkImportKeyPairResponse responseStatus
  = ImportKeyPairResponse'{operation = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsOperation :: Lens.Lens' ImportKeyPairResponse (Core.Maybe Types.Operation)
ikprrsOperation = Lens.field @"operation"
{-# INLINEABLE ikprrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprrsResponseStatus :: Lens.Lens' ImportKeyPairResponse Core.Int
ikprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ikprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
