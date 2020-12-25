{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListPublicKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all public keys that have been added to CloudFront for this account.
module Network.AWS.CloudFront.ListPublicKeys
  ( -- * Creating a request
    ListPublicKeys (..),
    mkListPublicKeys,

    -- ** Request lenses
    lpkMarker,
    lpkMaxItems,

    -- * Destructuring the response
    ListPublicKeysResponse (..),
    mkListPublicKeysResponse,

    -- ** Response lenses
    lpkrrsPublicKeyList,
    lpkrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPublicKeys' smart constructor.
data ListPublicKeys = ListPublicKeys'
  { -- | Use this when paginating results to indicate where to begin in your list of public keys. The results include public keys in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last public key on that page).
    marker :: Core.Maybe Types.String,
    -- | The maximum number of public keys you want in the response body.
    maxItems :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPublicKeys' value with any optional fields omitted.
mkListPublicKeys ::
  ListPublicKeys
mkListPublicKeys =
  ListPublicKeys' {marker = Core.Nothing, maxItems = Core.Nothing}

-- | Use this when paginating results to indicate where to begin in your list of public keys. The results include public keys in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last public key on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkMarker :: Lens.Lens' ListPublicKeys (Core.Maybe Types.String)
lpkMarker = Lens.field @"marker"
{-# DEPRECATED lpkMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of public keys you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkMaxItems :: Lens.Lens' ListPublicKeys (Core.Maybe Types.String)
lpkMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lpkMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListPublicKeys where
  type Rs ListPublicKeys = ListPublicKeysResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2020-05-31/public-key",
        Core._rqQuery =
          Core.toQueryValue "Marker" Core.<$> marker
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListPublicKeysResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListPublicKeysResponse' smart constructor.
data ListPublicKeysResponse = ListPublicKeysResponse'
  { -- | Returns a list of all public keys that have been added to CloudFront for this account.
    publicKeyList :: Core.Maybe Types.PublicKeyList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListPublicKeysResponse' value with any optional fields omitted.
mkListPublicKeysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPublicKeysResponse
mkListPublicKeysResponse responseStatus =
  ListPublicKeysResponse'
    { publicKeyList = Core.Nothing,
      responseStatus
    }

-- | Returns a list of all public keys that have been added to CloudFront for this account.
--
-- /Note:/ Consider using 'publicKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrrsPublicKeyList :: Lens.Lens' ListPublicKeysResponse (Core.Maybe Types.PublicKeyList)
lpkrrsPublicKeyList = Lens.field @"publicKeyList"
{-# DEPRECATED lpkrrsPublicKeyList "Use generic-lens or generic-optics with 'publicKeyList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrrsResponseStatus :: Lens.Lens' ListPublicKeysResponse Core.Int
lpkrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpkrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
