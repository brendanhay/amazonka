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
    ctpProfileId,
    ctpFormat,
    ctpContentType,
  )
where

import Network.AWS.CloudFront.Types.Format
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A field-level encryption content type profile.
--
-- /See:/ 'mkContentTypeProfile' smart constructor.
data ContentTypeProfile = ContentTypeProfile'
  { profileId ::
      Lude.Maybe Lude.Text,
    format :: Format,
    contentType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContentTypeProfile' with the minimum fields required to make a request.
--
-- * 'contentType' - The content type for a field-level encryption content type-profile mapping.
-- * 'format' - The format for a field-level encryption content type-profile mapping.
-- * 'profileId' - The profile ID for a field-level encryption content type-profile mapping.
mkContentTypeProfile ::
  -- | 'format'
  Format ->
  -- | 'contentType'
  Lude.Text ->
  ContentTypeProfile
mkContentTypeProfile pFormat_ pContentType_ =
  ContentTypeProfile'
    { profileId = Lude.Nothing,
      format = pFormat_,
      contentType = pContentType_
    }

-- | The profile ID for a field-level encryption content type-profile mapping.
--
-- /Note:/ Consider using 'profileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpProfileId :: Lens.Lens' ContentTypeProfile (Lude.Maybe Lude.Text)
ctpProfileId = Lens.lens (profileId :: ContentTypeProfile -> Lude.Maybe Lude.Text) (\s a -> s {profileId = a} :: ContentTypeProfile)
{-# DEPRECATED ctpProfileId "Use generic-lens or generic-optics with 'profileId' instead." #-}

-- | The format for a field-level encryption content type-profile mapping.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpFormat :: Lens.Lens' ContentTypeProfile Format
ctpFormat = Lens.lens (format :: ContentTypeProfile -> Format) (\s a -> s {format = a} :: ContentTypeProfile)
{-# DEPRECATED ctpFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The content type for a field-level encryption content type-profile mapping.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpContentType :: Lens.Lens' ContentTypeProfile Lude.Text
ctpContentType = Lens.lens (contentType :: ContentTypeProfile -> Lude.Text) (\s a -> s {contentType = a} :: ContentTypeProfile)
{-# DEPRECATED ctpContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Lude.FromXML ContentTypeProfile where
  parseXML x =
    ContentTypeProfile'
      Lude.<$> (x Lude..@? "ProfileId")
      Lude.<*> (x Lude..@ "Format")
      Lude.<*> (x Lude..@ "ContentType")

instance Lude.ToXML ContentTypeProfile where
  toXML ContentTypeProfile' {..} =
    Lude.mconcat
      [ "ProfileId" Lude.@= profileId,
        "Format" Lude.@= format,
        "ContentType" Lude.@= contentType
      ]
