{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListFieldLevelEncryptionConfigs (..),
    mkListFieldLevelEncryptionConfigs,

    -- ** Request lenses
    lflecMarker,
    lflecMaxItems,

    -- * Destructuring the response
    ListFieldLevelEncryptionConfigsResponse (..),
    mkListFieldLevelEncryptionConfigsResponse,

    -- ** Response lenses
    lflecrrsFieldLevelEncryptionList,
    lflecrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFieldLevelEncryptionConfigs' smart constructor.
data ListFieldLevelEncryptionConfigs = ListFieldLevelEncryptionConfigs'
  { -- | Use this when paginating results to indicate where to begin in your list of configurations. The results include configurations in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last configuration on that page).
    marker :: Core.Maybe Types.String,
    -- | The maximum number of field-level encryption configurations you want in the response body.
    maxItems :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFieldLevelEncryptionConfigs' value with any optional fields omitted.
mkListFieldLevelEncryptionConfigs ::
  ListFieldLevelEncryptionConfigs
mkListFieldLevelEncryptionConfigs =
  ListFieldLevelEncryptionConfigs'
    { marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list of configurations. The results include configurations in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last configuration on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecMarker :: Lens.Lens' ListFieldLevelEncryptionConfigs (Core.Maybe Types.String)
lflecMarker = Lens.field @"marker"
{-# DEPRECATED lflecMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of field-level encryption configurations you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecMaxItems :: Lens.Lens' ListFieldLevelEncryptionConfigs (Core.Maybe Types.String)
lflecMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lflecMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListFieldLevelEncryptionConfigs where
  type
    Rs ListFieldLevelEncryptionConfigs =
      ListFieldLevelEncryptionConfigsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2020-05-31/field-level-encryption",
        Core._rqQuery =
          Core.toQueryValue "Marker" Core.<$> marker
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListFieldLevelEncryptionConfigsResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListFieldLevelEncryptionConfigsResponse' smart constructor.
data ListFieldLevelEncryptionConfigsResponse = ListFieldLevelEncryptionConfigsResponse'
  { -- | Returns a list of all field-level encryption configurations that have been created in CloudFront for this account.
    fieldLevelEncryptionList :: Core.Maybe Types.FieldLevelEncryptionList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListFieldLevelEncryptionConfigsResponse' value with any optional fields omitted.
mkListFieldLevelEncryptionConfigsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListFieldLevelEncryptionConfigsResponse
mkListFieldLevelEncryptionConfigsResponse responseStatus =
  ListFieldLevelEncryptionConfigsResponse'
    { fieldLevelEncryptionList =
        Core.Nothing,
      responseStatus
    }

-- | Returns a list of all field-level encryption configurations that have been created in CloudFront for this account.
--
-- /Note:/ Consider using 'fieldLevelEncryptionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecrrsFieldLevelEncryptionList :: Lens.Lens' ListFieldLevelEncryptionConfigsResponse (Core.Maybe Types.FieldLevelEncryptionList)
lflecrrsFieldLevelEncryptionList = Lens.field @"fieldLevelEncryptionList"
{-# DEPRECATED lflecrrsFieldLevelEncryptionList "Use generic-lens or generic-optics with 'fieldLevelEncryptionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflecrrsResponseStatus :: Lens.Lens' ListFieldLevelEncryptionConfigsResponse Core.Int
lflecrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lflecrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
