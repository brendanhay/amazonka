{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ContentTypeProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ContentTypeProfileConfig
  ( ContentTypeProfileConfig (..),

    -- * Smart constructor
    mkContentTypeProfileConfig,

    -- * Lenses
    ctpcForwardWhenContentTypeIsUnknown,
    ctpcContentTypeProfiles,
  )
where

import Network.AWS.CloudFront.Types.ContentTypeProfiles
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration for a field-level encryption content type-profile mapping.
--
-- /See:/ 'mkContentTypeProfileConfig' smart constructor.
data ContentTypeProfileConfig = ContentTypeProfileConfig'
  { -- | The setting in a field-level encryption content type-profile mapping that specifies what to do when an unknown content type is provided for the profile. If true, content is forwarded without being encrypted when the content type is unknown. If false (the default), an error is returned when the content type is unknown.
    forwardWhenContentTypeIsUnknown :: Lude.Bool,
    -- | The configuration for a field-level encryption content type-profile.
    contentTypeProfiles :: Lude.Maybe ContentTypeProfiles
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContentTypeProfileConfig' with the minimum fields required to make a request.
--
-- * 'forwardWhenContentTypeIsUnknown' - The setting in a field-level encryption content type-profile mapping that specifies what to do when an unknown content type is provided for the profile. If true, content is forwarded without being encrypted when the content type is unknown. If false (the default), an error is returned when the content type is unknown.
-- * 'contentTypeProfiles' - The configuration for a field-level encryption content type-profile.
mkContentTypeProfileConfig ::
  -- | 'forwardWhenContentTypeIsUnknown'
  Lude.Bool ->
  ContentTypeProfileConfig
mkContentTypeProfileConfig pForwardWhenContentTypeIsUnknown_ =
  ContentTypeProfileConfig'
    { forwardWhenContentTypeIsUnknown =
        pForwardWhenContentTypeIsUnknown_,
      contentTypeProfiles = Lude.Nothing
    }

-- | The setting in a field-level encryption content type-profile mapping that specifies what to do when an unknown content type is provided for the profile. If true, content is forwarded without being encrypted when the content type is unknown. If false (the default), an error is returned when the content type is unknown.
--
-- /Note:/ Consider using 'forwardWhenContentTypeIsUnknown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpcForwardWhenContentTypeIsUnknown :: Lens.Lens' ContentTypeProfileConfig Lude.Bool
ctpcForwardWhenContentTypeIsUnknown = Lens.lens (forwardWhenContentTypeIsUnknown :: ContentTypeProfileConfig -> Lude.Bool) (\s a -> s {forwardWhenContentTypeIsUnknown = a} :: ContentTypeProfileConfig)
{-# DEPRECATED ctpcForwardWhenContentTypeIsUnknown "Use generic-lens or generic-optics with 'forwardWhenContentTypeIsUnknown' instead." #-}

-- | The configuration for a field-level encryption content type-profile.
--
-- /Note:/ Consider using 'contentTypeProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpcContentTypeProfiles :: Lens.Lens' ContentTypeProfileConfig (Lude.Maybe ContentTypeProfiles)
ctpcContentTypeProfiles = Lens.lens (contentTypeProfiles :: ContentTypeProfileConfig -> Lude.Maybe ContentTypeProfiles) (\s a -> s {contentTypeProfiles = a} :: ContentTypeProfileConfig)
{-# DEPRECATED ctpcContentTypeProfiles "Use generic-lens or generic-optics with 'contentTypeProfiles' instead." #-}

instance Lude.FromXML ContentTypeProfileConfig where
  parseXML x =
    ContentTypeProfileConfig'
      Lude.<$> (x Lude..@ "ForwardWhenContentTypeIsUnknown")
      Lude.<*> (x Lude..@? "ContentTypeProfiles")

instance Lude.ToXML ContentTypeProfileConfig where
  toXML ContentTypeProfileConfig' {..} =
    Lude.mconcat
      [ "ForwardWhenContentTypeIsUnknown"
          Lude.@= forwardWhenContentTypeIsUnknown,
        "ContentTypeProfiles" Lude.@= contentTypeProfiles
      ]
