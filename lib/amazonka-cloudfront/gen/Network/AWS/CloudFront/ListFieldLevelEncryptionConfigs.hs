{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all field-level encryption configurations that have been created in CloudFront for this account.
module Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs
    (
    -- * Creating a request
      ListFieldLevelEncryptionConfigs (..)
    , mkListFieldLevelEncryptionConfigs
    -- ** Request lenses
    , lflecMarker
    , lflecMaxItems

    -- * Destructuring the response
    , ListFieldLevelEncryptionConfigsResponse (..)
    , mkListFieldLevelEncryptionConfigsResponse
    -- ** Response lenses
    , lflecrrsFieldLevelEncryptionList
    , lflecrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFieldLevelEncryptionConfigs' smart constructor.
data ListFieldLevelEncryptionConfigs = ListFieldLevelEncryptionConfigs'
  { marker :: Core.Maybe Core.Text
    -- ^ Use this when paginating results to indicate where to begin in your list of configurations. The results include configurations in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last configuration on that page). 
  , maxItems :: Core.Maybe Core.Text
    -- ^ The maximum number of field-level encryption configurations you want in the response body. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFieldLevelEncryptionConfigs' value with any optional fields omitted.
mkListFieldLevelEncryptionConfigs
    :: ListFieldLevelEncryptionConfigs
mkListFieldLevelEncryptionConfigs
  = ListFieldLevelEncryptionConfigs'{marker = Core.Nothing,
                                     maxItems = Core.Nothing}

-- | Use this when paginating results to indicate where to begin in your list of configurations. The results include configurations in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last configuration on that page). 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecMarker :: Lens.Lens' ListFieldLevelEncryptionConfigs (Core.Maybe Core.Text)
lflecMarker = Lens.field @"marker"
{-# INLINEABLE lflecMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of field-level encryption configurations you want in the response body. 
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecMaxItems :: Lens.Lens' ListFieldLevelEncryptionConfigs (Core.Maybe Core.Text)
lflecMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lflecMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListFieldLevelEncryptionConfigs where
        toQuery ListFieldLevelEncryptionConfigs{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListFieldLevelEncryptionConfigs where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListFieldLevelEncryptionConfigs where
        type Rs ListFieldLevelEncryptionConfigs =
             ListFieldLevelEncryptionConfigsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2020-05-31/field-level-encryption",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListFieldLevelEncryptionConfigsResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListFieldLevelEncryptionConfigsResponse' smart constructor.
data ListFieldLevelEncryptionConfigsResponse = ListFieldLevelEncryptionConfigsResponse'
  { fieldLevelEncryptionList :: Core.Maybe Types.FieldLevelEncryptionList
    -- ^ Returns a list of all field-level encryption configurations that have been created in CloudFront for this account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListFieldLevelEncryptionConfigsResponse' value with any optional fields omitted.
mkListFieldLevelEncryptionConfigsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListFieldLevelEncryptionConfigsResponse
mkListFieldLevelEncryptionConfigsResponse responseStatus
  = ListFieldLevelEncryptionConfigsResponse'{fieldLevelEncryptionList
                                               = Core.Nothing,
                                             responseStatus}

-- | Returns a list of all field-level encryption configurations that have been created in CloudFront for this account.
--
-- /Note:/ Consider using 'fieldLevelEncryptionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecrrsFieldLevelEncryptionList :: Lens.Lens' ListFieldLevelEncryptionConfigsResponse (Core.Maybe Types.FieldLevelEncryptionList)
lflecrrsFieldLevelEncryptionList = Lens.field @"fieldLevelEncryptionList"
{-# INLINEABLE lflecrrsFieldLevelEncryptionList #-}
{-# DEPRECATED fieldLevelEncryptionList "Use generic-lens or generic-optics with 'fieldLevelEncryptionList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecrrsResponseStatus :: Lens.Lens' ListFieldLevelEncryptionConfigsResponse Core.Int
lflecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lflecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
