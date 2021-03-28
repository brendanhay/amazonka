{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an SSH key pair.
--
-- The @create key pair@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateKeyPair
    (
    -- * Creating a request
      CreateKeyPair (..)
    , mkCreateKeyPair
    -- ** Request lenses
    , ckpKeyPairName
    , ckpTags

    -- * Destructuring the response
    , CreateKeyPairResponse (..)
    , mkCreateKeyPairResponse
    -- ** Response lenses
    , ckprrsKeyPair
    , ckprrsOperation
    , ckprrsPrivateKeyBase64
    , ckprrsPublicKeyBase64
    , ckprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateKeyPair' smart constructor.
data CreateKeyPair = CreateKeyPair'
  { keyPairName :: Types.ResourceName
    -- ^ The name for your new key pair.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateKeyPair' value with any optional fields omitted.
mkCreateKeyPair
    :: Types.ResourceName -- ^ 'keyPairName'
    -> CreateKeyPair
mkCreateKeyPair keyPairName
  = CreateKeyPair'{keyPairName, tags = Core.Nothing}

-- | The name for your new key pair.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckpKeyPairName :: Lens.Lens' CreateKeyPair Types.ResourceName
ckpKeyPairName = Lens.field @"keyPairName"
{-# INLINEABLE ckpKeyPairName #-}
{-# DEPRECATED keyPairName "Use generic-lens or generic-optics with 'keyPairName' instead"  #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckpTags :: Lens.Lens' CreateKeyPair (Core.Maybe [Types.Tag])
ckpTags = Lens.field @"tags"
{-# INLINEABLE ckpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateKeyPair where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateKeyPair where
        toHeaders CreateKeyPair{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.CreateKeyPair")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateKeyPair where
        toJSON CreateKeyPair{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("keyPairName" Core..= keyPairName),
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateKeyPair where
        type Rs CreateKeyPair = CreateKeyPairResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateKeyPairResponse' Core.<$>
                   (x Core..:? "keyPair") Core.<*> x Core..:? "operation" Core.<*>
                     x Core..:? "privateKeyBase64"
                     Core.<*> x Core..:? "publicKeyBase64"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateKeyPairResponse' smart constructor.
data CreateKeyPairResponse = CreateKeyPairResponse'
  { keyPair :: Core.Maybe Types.KeyPair
    -- ^ An array of key-value pairs containing information about the new key pair you just created.
  , operation :: Core.Maybe Types.Operation
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , privateKeyBase64 :: Core.Maybe Types.Base64
    -- ^ A base64-encoded RSA private key.
  , publicKeyBase64 :: Core.Maybe Types.Base64
    -- ^ A base64-encoded public key of the @ssh-rsa@ type.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateKeyPairResponse' value with any optional fields omitted.
mkCreateKeyPairResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateKeyPairResponse
mkCreateKeyPairResponse responseStatus
  = CreateKeyPairResponse'{keyPair = Core.Nothing,
                           operation = Core.Nothing, privateKeyBase64 = Core.Nothing,
                           publicKeyBase64 = Core.Nothing, responseStatus}

-- | An array of key-value pairs containing information about the new key pair you just created.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsKeyPair :: Lens.Lens' CreateKeyPairResponse (Core.Maybe Types.KeyPair)
ckprrsKeyPair = Lens.field @"keyPair"
{-# INLINEABLE ckprrsKeyPair #-}
{-# DEPRECATED keyPair "Use generic-lens or generic-optics with 'keyPair' instead"  #-}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsOperation :: Lens.Lens' CreateKeyPairResponse (Core.Maybe Types.Operation)
ckprrsOperation = Lens.field @"operation"
{-# INLINEABLE ckprrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | A base64-encoded RSA private key.
--
-- /Note:/ Consider using 'privateKeyBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsPrivateKeyBase64 :: Lens.Lens' CreateKeyPairResponse (Core.Maybe Types.Base64)
ckprrsPrivateKeyBase64 = Lens.field @"privateKeyBase64"
{-# INLINEABLE ckprrsPrivateKeyBase64 #-}
{-# DEPRECATED privateKeyBase64 "Use generic-lens or generic-optics with 'privateKeyBase64' instead"  #-}

-- | A base64-encoded public key of the @ssh-rsa@ type.
--
-- /Note:/ Consider using 'publicKeyBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsPublicKeyBase64 :: Lens.Lens' CreateKeyPairResponse (Core.Maybe Types.Base64)
ckprrsPublicKeyBase64 = Lens.field @"publicKeyBase64"
{-# INLINEABLE ckprrsPublicKeyBase64 #-}
{-# DEPRECATED publicKeyBase64 "Use generic-lens or generic-optics with 'publicKeyBase64' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprrsResponseStatus :: Lens.Lens' CreateKeyPairResponse Core.Int
ckprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ckprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
