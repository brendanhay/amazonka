{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreatePublicKey (..),
    mkCreatePublicKey,

    -- ** Request lenses
    cpkPublicKeyConfig,

    -- * Destructuring the response
    CreatePublicKeyResponse (..),
    mkCreatePublicKeyResponse,

    -- ** Response lenses
    cpkrrsETag,
    cpkrrsLocation,
    cpkrrsPublicKey,
    cpkrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePublicKey' smart constructor.
newtype CreatePublicKey = CreatePublicKey'
  { -- | A CloudFront public key configuration.
    publicKeyConfig :: Types.PublicKeyConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePublicKey' value with any optional fields omitted.
mkCreatePublicKey ::
  -- | 'publicKeyConfig'
  Types.PublicKeyConfig ->
  CreatePublicKey
mkCreatePublicKey publicKeyConfig =
  CreatePublicKey' {publicKeyConfig}

-- | A CloudFront public key configuration.
--
-- /Note:/ Consider using 'publicKeyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkPublicKeyConfig :: Lens.Lens' CreatePublicKey Types.PublicKeyConfig
cpkPublicKeyConfig = Lens.field @"publicKeyConfig"
{-# DEPRECATED cpkPublicKeyConfig "Use generic-lens or generic-optics with 'publicKeyConfig' instead." #-}

instance Core.AWSRequest CreatePublicKey where
  type Rs CreatePublicKey = CreatePublicKeyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2020-05-31/public-key",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreatePublicKeyResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseHeaderMaybe "Location" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePublicKeyResponse' smart constructor.
data CreatePublicKeyResponse = CreatePublicKeyResponse'
  { -- | The identifier for this version of the public key.
    eTag :: Core.Maybe Types.String,
    -- | The URL of the public key.
    location :: Core.Maybe Types.String,
    -- | The public key.
    publicKey :: Core.Maybe Types.PublicKey,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreatePublicKeyResponse' value with any optional fields omitted.
mkCreatePublicKeyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePublicKeyResponse
mkCreatePublicKeyResponse responseStatus =
  CreatePublicKeyResponse'
    { eTag = Core.Nothing,
      location = Core.Nothing,
      publicKey = Core.Nothing,
      responseStatus
    }

-- | The identifier for this version of the public key.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrrsETag :: Lens.Lens' CreatePublicKeyResponse (Core.Maybe Types.String)
cpkrrsETag = Lens.field @"eTag"
{-# DEPRECATED cpkrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The URL of the public key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrrsLocation :: Lens.Lens' CreatePublicKeyResponse (Core.Maybe Types.String)
cpkrrsLocation = Lens.field @"location"
{-# DEPRECATED cpkrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The public key.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrrsPublicKey :: Lens.Lens' CreatePublicKeyResponse (Core.Maybe Types.PublicKey)
cpkrrsPublicKey = Lens.field @"publicKey"
{-# DEPRECATED cpkrrsPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrrsResponseStatus :: Lens.Lens' CreatePublicKeyResponse Core.Int
cpkrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpkrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
