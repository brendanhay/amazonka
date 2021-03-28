{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListPublicKeys (..)
    , mkListPublicKeys
    -- ** Request lenses
    , lpkMarker
    , lpkMaxItems

    -- * Destructuring the response
    , ListPublicKeysResponse (..)
    , mkListPublicKeysResponse
    -- ** Response lenses
    , lpkrrsPublicKeyList
    , lpkrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPublicKeys' smart constructor.
data ListPublicKeys = ListPublicKeys'
  { marker :: Core.Maybe Core.Text
    -- ^ Use this when paginating results to indicate where to begin in your list of public keys. The results include public keys in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last public key on that page). 
  , maxItems :: Core.Maybe Core.Text
    -- ^ The maximum number of public keys you want in the response body. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPublicKeys' value with any optional fields omitted.
mkListPublicKeys
    :: ListPublicKeys
mkListPublicKeys
  = ListPublicKeys'{marker = Core.Nothing, maxItems = Core.Nothing}

-- | Use this when paginating results to indicate where to begin in your list of public keys. The results include public keys in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last public key on that page). 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkMarker :: Lens.Lens' ListPublicKeys (Core.Maybe Core.Text)
lpkMarker = Lens.field @"marker"
{-# INLINEABLE lpkMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of public keys you want in the response body. 
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkMaxItems :: Lens.Lens' ListPublicKeys (Core.Maybe Core.Text)
lpkMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lpkMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListPublicKeys where
        toQuery ListPublicKeys{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListPublicKeys where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListPublicKeys where
        type Rs ListPublicKeys = ListPublicKeysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2020-05-31/public-key",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListPublicKeysResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListPublicKeysResponse' smart constructor.
data ListPublicKeysResponse = ListPublicKeysResponse'
  { publicKeyList :: Core.Maybe Types.PublicKeyList
    -- ^ Returns a list of all public keys that have been added to CloudFront for this account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPublicKeysResponse' value with any optional fields omitted.
mkListPublicKeysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPublicKeysResponse
mkListPublicKeysResponse responseStatus
  = ListPublicKeysResponse'{publicKeyList = Core.Nothing,
                            responseStatus}

-- | Returns a list of all public keys that have been added to CloudFront for this account.
--
-- /Note:/ Consider using 'publicKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrrsPublicKeyList :: Lens.Lens' ListPublicKeysResponse (Core.Maybe Types.PublicKeyList)
lpkrrsPublicKeyList = Lens.field @"publicKeyList"
{-# INLINEABLE lpkrrsPublicKeyList #-}
{-# DEPRECATED publicKeyList "Use generic-lens or generic-optics with 'publicKeyList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrrsResponseStatus :: Lens.Lens' ListPublicKeysResponse Core.Int
lpkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
