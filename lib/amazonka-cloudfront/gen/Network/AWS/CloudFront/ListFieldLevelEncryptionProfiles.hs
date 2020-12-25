{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request a list of field-level encryption profiles that have been created in CloudFront for this account.
module Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles
  ( -- * Creating a request
    ListFieldLevelEncryptionProfiles (..),
    mkListFieldLevelEncryptionProfiles,

    -- ** Request lenses
    lflepMarker,
    lflepMaxItems,

    -- * Destructuring the response
    ListFieldLevelEncryptionProfilesResponse (..),
    mkListFieldLevelEncryptionProfilesResponse,

    -- ** Response lenses
    lfleprrsFieldLevelEncryptionProfileList,
    lfleprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFieldLevelEncryptionProfiles' smart constructor.
data ListFieldLevelEncryptionProfiles = ListFieldLevelEncryptionProfiles'
  { -- | Use this when paginating results to indicate where to begin in your list of profiles. The results include profiles in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last profile on that page).
    marker :: Core.Maybe Types.String,
    -- | The maximum number of field-level encryption profiles you want in the response body.
    maxItems :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFieldLevelEncryptionProfiles' value with any optional fields omitted.
mkListFieldLevelEncryptionProfiles ::
  ListFieldLevelEncryptionProfiles
mkListFieldLevelEncryptionProfiles =
  ListFieldLevelEncryptionProfiles'
    { marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list of profiles. The results include profiles in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last profile on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflepMarker :: Lens.Lens' ListFieldLevelEncryptionProfiles (Core.Maybe Types.String)
lflepMarker = Lens.field @"marker"
{-# DEPRECATED lflepMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of field-level encryption profiles you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lflepMaxItems :: Lens.Lens' ListFieldLevelEncryptionProfiles (Core.Maybe Types.String)
lflepMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lflepMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListFieldLevelEncryptionProfiles where
  type
    Rs ListFieldLevelEncryptionProfiles =
      ListFieldLevelEncryptionProfilesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath "/2020-05-31/field-level-encryption-profile",
        Core._rqQuery =
          Core.toQueryValue "Marker" Core.<$> marker
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListFieldLevelEncryptionProfilesResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListFieldLevelEncryptionProfilesResponse' smart constructor.
data ListFieldLevelEncryptionProfilesResponse = ListFieldLevelEncryptionProfilesResponse'
  { -- | Returns a list of the field-level encryption profiles that have been created in CloudFront for this account.
    fieldLevelEncryptionProfileList :: Core.Maybe Types.FieldLevelEncryptionProfileList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListFieldLevelEncryptionProfilesResponse' value with any optional fields omitted.
mkListFieldLevelEncryptionProfilesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListFieldLevelEncryptionProfilesResponse
mkListFieldLevelEncryptionProfilesResponse responseStatus =
  ListFieldLevelEncryptionProfilesResponse'
    { fieldLevelEncryptionProfileList =
        Core.Nothing,
      responseStatus
    }

-- | Returns a list of the field-level encryption profiles that have been created in CloudFront for this account.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfleprrsFieldLevelEncryptionProfileList :: Lens.Lens' ListFieldLevelEncryptionProfilesResponse (Core.Maybe Types.FieldLevelEncryptionProfileList)
lfleprrsFieldLevelEncryptionProfileList = Lens.field @"fieldLevelEncryptionProfileList"
{-# DEPRECATED lfleprrsFieldLevelEncryptionProfileList "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfleprrsResponseStatus :: Lens.Lens' ListFieldLevelEncryptionProfilesResponse Core.Int
lfleprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lfleprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
