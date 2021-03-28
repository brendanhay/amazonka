{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreatePublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a public key to CloudFront that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
module Network.AWS.CloudFront.CreatePublicKey
    (
    -- * Creating a request
      CreatePublicKey (..)
    , mkCreatePublicKey
    -- ** Request lenses
    , cpkPublicKeyConfig

    -- * Destructuring the response
    , CreatePublicKeyResponse (..)
    , mkCreatePublicKeyResponse
    -- ** Response lenses
    , cpkrrsETag
    , cpkrrsLocation
    , cpkrrsPublicKey
    , cpkrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePublicKey' smart constructor.
newtype CreatePublicKey = CreatePublicKey'
  { publicKeyConfig :: Types.PublicKeyConfig
    -- ^ A CloudFront public key configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePublicKey' value with any optional fields omitted.
mkCreatePublicKey
    :: Types.PublicKeyConfig -- ^ 'publicKeyConfig'
    -> CreatePublicKey
mkCreatePublicKey publicKeyConfig
  = CreatePublicKey'{publicKeyConfig}

-- | A CloudFront public key configuration.
--
-- /Note:/ Consider using 'publicKeyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkPublicKeyConfig :: Lens.Lens' CreatePublicKey Types.PublicKeyConfig
cpkPublicKeyConfig = Lens.field @"publicKeyConfig"
{-# INLINEABLE cpkPublicKeyConfig #-}
{-# DEPRECATED publicKeyConfig "Use generic-lens or generic-optics with 'publicKeyConfig' instead"  #-}

instance Core.ToQuery CreatePublicKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePublicKey where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreatePublicKey where
        type Rs CreatePublicKey = CreatePublicKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2020-05-31/public-key",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreatePublicKeyResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*>
                     Core.parseHeaderMaybe "Location" h
                     Core.<*> Core.parseXML x
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePublicKeyResponse' smart constructor.
data CreatePublicKeyResponse = CreatePublicKeyResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The identifier for this version of the public key.
  , location :: Core.Maybe Core.Text
    -- ^ The URL of the public key.
  , publicKey :: Core.Maybe Types.PublicKey
    -- ^ The public key.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreatePublicKeyResponse' value with any optional fields omitted.
mkCreatePublicKeyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePublicKeyResponse
mkCreatePublicKeyResponse responseStatus
  = CreatePublicKeyResponse'{eTag = Core.Nothing,
                             location = Core.Nothing, publicKey = Core.Nothing, responseStatus}

-- | The identifier for this version of the public key.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrrsETag :: Lens.Lens' CreatePublicKeyResponse (Core.Maybe Core.Text)
cpkrrsETag = Lens.field @"eTag"
{-# INLINEABLE cpkrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The URL of the public key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrrsLocation :: Lens.Lens' CreatePublicKeyResponse (Core.Maybe Core.Text)
cpkrrsLocation = Lens.field @"location"
{-# INLINEABLE cpkrrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The public key.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrrsPublicKey :: Lens.Lens' CreatePublicKeyResponse (Core.Maybe Types.PublicKey)
cpkrrsPublicKey = Lens.field @"publicKey"
{-# INLINEABLE cpkrrsPublicKey #-}
{-# DEPRECATED publicKey "Use generic-lens or generic-optics with 'publicKey' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrrsResponseStatus :: Lens.Lens' CreatePublicKeyResponse Core.Int
cpkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
