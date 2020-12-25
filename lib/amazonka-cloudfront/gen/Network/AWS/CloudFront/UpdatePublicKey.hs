{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdatePublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update public key information. Note that the only value you can change is the comment.
module Network.AWS.CloudFront.UpdatePublicKey
  ( -- * Creating a request
    UpdatePublicKey (..),
    mkUpdatePublicKey,

    -- ** Request lenses
    upkPublicKeyConfig,
    upkId,
    upkIfMatch,

    -- * Destructuring the response
    UpdatePublicKeyResponse (..),
    mkUpdatePublicKeyResponse,

    -- ** Response lenses
    upkrrsETag,
    upkrrsPublicKey,
    upkrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePublicKey' smart constructor.
data UpdatePublicKey = UpdatePublicKey'
  { -- | A public key configuration.
    publicKeyConfig :: Types.PublicKeyConfig,
    -- | The identifier of the public key that you are updating.
    id :: Types.String,
    -- | The value of the @ETag@ header that you received when retrieving the public key to update. For example: @E2QWRUHAPOMQZL@ .
    ifMatch :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePublicKey' value with any optional fields omitted.
mkUpdatePublicKey ::
  -- | 'publicKeyConfig'
  Types.PublicKeyConfig ->
  -- | 'id'
  Types.String ->
  UpdatePublicKey
mkUpdatePublicKey publicKeyConfig id =
  UpdatePublicKey' {publicKeyConfig, id, ifMatch = Core.Nothing}

-- | A public key configuration.
--
-- /Note:/ Consider using 'publicKeyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkPublicKeyConfig :: Lens.Lens' UpdatePublicKey Types.PublicKeyConfig
upkPublicKeyConfig = Lens.field @"publicKeyConfig"
{-# DEPRECATED upkPublicKeyConfig "Use generic-lens or generic-optics with 'publicKeyConfig' instead." #-}

-- | The identifier of the public key that you are updating.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkId :: Lens.Lens' UpdatePublicKey Types.String
upkId = Lens.field @"id"
{-# DEPRECATED upkId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The value of the @ETag@ header that you received when retrieving the public key to update. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkIfMatch :: Lens.Lens' UpdatePublicKey (Core.Maybe Types.String)
upkIfMatch = Lens.field @"ifMatch"
{-# DEPRECATED upkIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

instance Core.AWSRequest UpdatePublicKey where
  type Rs UpdatePublicKey = UpdatePublicKeyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/public-key/" Core.<> (Core.toText id)
                Core.<> ("/config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "If-Match" ifMatch,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          UpdatePublicKeyResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdatePublicKeyResponse' smart constructor.
data UpdatePublicKeyResponse = UpdatePublicKeyResponse'
  { -- | The identifier of the current version of the public key.
    eTag :: Core.Maybe Types.String,
    -- | The public key.
    publicKey :: Core.Maybe Types.PublicKey,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdatePublicKeyResponse' value with any optional fields omitted.
mkUpdatePublicKeyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdatePublicKeyResponse
mkUpdatePublicKeyResponse responseStatus =
  UpdatePublicKeyResponse'
    { eTag = Core.Nothing,
      publicKey = Core.Nothing,
      responseStatus
    }

-- | The identifier of the current version of the public key.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkrrsETag :: Lens.Lens' UpdatePublicKeyResponse (Core.Maybe Types.String)
upkrrsETag = Lens.field @"eTag"
{-# DEPRECATED upkrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The public key.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkrrsPublicKey :: Lens.Lens' UpdatePublicKeyResponse (Core.Maybe Types.PublicKey)
upkrrsPublicKey = Lens.field @"publicKey"
{-# DEPRECATED upkrrsPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkrrsResponseStatus :: Lens.Lens' UpdatePublicKeyResponse Core.Int
upkrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED upkrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
