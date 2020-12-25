{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ContentTypeProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ContentTypeProfile
  ( ContentTypeProfile (..),

    -- * Smart constructor
    mkContentTypeProfile,

    -- * Lenses
    ctpFormat,
    ctpContentType,
    ctpProfileId,
  )
where

import qualified Network.AWS.CloudFront.Types.ContentType as Types
import qualified Network.AWS.CloudFront.Types.Format as Types
import qualified Network.AWS.CloudFront.Types.ProfileId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A field-level encryption content type profile.
--
-- /See:/ 'mkContentTypeProfile' smart constructor.
data ContentTypeProfile = ContentTypeProfile'
  { -- | The format for a field-level encryption content type-profile mapping.
    format :: Types.Format,
    -- | The content type for a field-level encryption content type-profile mapping.
    contentType :: Types.ContentType,
    -- | The profile ID for a field-level encryption content type-profile mapping.
    profileId :: Core.Maybe Types.ProfileId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContentTypeProfile' value with any optional fields omitted.
mkContentTypeProfile ::
  -- | 'format'
  Types.Format ->
  -- | 'contentType'
  Types.ContentType ->
  ContentTypeProfile
mkContentTypeProfile format contentType =
  ContentTypeProfile'
    { format,
      contentType,
      profileId = Core.Nothing
    }

-- | The format for a field-level encryption content type-profile mapping.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpFormat :: Lens.Lens' ContentTypeProfile Types.Format
ctpFormat = Lens.field @"format"
{-# DEPRECATED ctpFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The content type for a field-level encryption content type-profile mapping.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpContentType :: Lens.Lens' ContentTypeProfile Types.ContentType
ctpContentType = Lens.field @"contentType"
{-# DEPRECATED ctpContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The profile ID for a field-level encryption content type-profile mapping.
--
-- /Note:/ Consider using 'profileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpProfileId :: Lens.Lens' ContentTypeProfile (Core.Maybe Types.ProfileId)
ctpProfileId = Lens.field @"profileId"
{-# DEPRECATED ctpProfileId "Use generic-lens or generic-optics with 'profileId' instead." #-}

instance Core.ToXML ContentTypeProfile where
  toXML ContentTypeProfile {..} =
    Core.toXMLNode "Format" format
      Core.<> Core.toXMLNode "ContentType" contentType
      Core.<> Core.toXMLNode "ProfileId" Core.<$> profileId

instance Core.FromXML ContentTypeProfile where
  parseXML x =
    ContentTypeProfile'
      Core.<$> (x Core..@ "Format")
      Core.<*> (x Core..@ "ContentType")
      Core.<*> (x Core..@? "ProfileId")
