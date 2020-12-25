{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionDescription
  ( CaptionDescription (..),

    -- * Smart constructor
    mkCaptionDescription,

    -- * Lenses
    cdCaptionSelectorName,
    cdName,
    cdDestinationSettings,
    cdLanguageCode,
    cdLanguageDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.CaptionDestinationSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Caption Description
--
-- /See:/ 'mkCaptionDescription' smart constructor.
data CaptionDescription = CaptionDescription'
  { -- | Specifies which input caption selector to use as a caption source when generating output captions. This field should match a captionSelector name.
    captionSelectorName :: Core.Text,
    -- | Name of the caption description.  Used to associate a caption description with an output.  Names must be unique within an event.
    name :: Core.Text,
    -- | Additional settings for captions destination that depend on the destination type.
    destinationSettings :: Core.Maybe Types.CaptionDestinationSettings,
    -- | ISO 639-2 three-digit code: http://www.loc.gov/standards/iso639-2/
    languageCode :: Core.Maybe Core.Text,
    -- | Human readable information to indicate captions available for players (eg. English, or Spanish).
    languageDescription :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionDescription' value with any optional fields omitted.
mkCaptionDescription ::
  -- | 'captionSelectorName'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  CaptionDescription
mkCaptionDescription captionSelectorName name =
  CaptionDescription'
    { captionSelectorName,
      name,
      destinationSettings = Core.Nothing,
      languageCode = Core.Nothing,
      languageDescription = Core.Nothing
    }

-- | Specifies which input caption selector to use as a caption source when generating output captions. This field should match a captionSelector name.
--
-- /Note:/ Consider using 'captionSelectorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCaptionSelectorName :: Lens.Lens' CaptionDescription Core.Text
cdCaptionSelectorName = Lens.field @"captionSelectorName"
{-# DEPRECATED cdCaptionSelectorName "Use generic-lens or generic-optics with 'captionSelectorName' instead." #-}

-- | Name of the caption description.  Used to associate a caption description with an output.  Names must be unique within an event.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' CaptionDescription Core.Text
cdName = Lens.field @"name"
{-# DEPRECATED cdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Additional settings for captions destination that depend on the destination type.
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDestinationSettings :: Lens.Lens' CaptionDescription (Core.Maybe Types.CaptionDestinationSettings)
cdDestinationSettings = Lens.field @"destinationSettings"
{-# DEPRECATED cdDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

-- | ISO 639-2 three-digit code: http://www.loc.gov/standards/iso639-2/
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLanguageCode :: Lens.Lens' CaptionDescription (Core.Maybe Core.Text)
cdLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED cdLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Human readable information to indicate captions available for players (eg. English, or Spanish).
--
-- /Note:/ Consider using 'languageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLanguageDescription :: Lens.Lens' CaptionDescription (Core.Maybe Core.Text)
cdLanguageDescription = Lens.field @"languageDescription"
{-# DEPRECATED cdLanguageDescription "Use generic-lens or generic-optics with 'languageDescription' instead." #-}

instance Core.FromJSON CaptionDescription where
  toJSON CaptionDescription {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("captionSelectorName" Core..= captionSelectorName),
            Core.Just ("name" Core..= name),
            ("destinationSettings" Core..=) Core.<$> destinationSettings,
            ("languageCode" Core..=) Core.<$> languageCode,
            ("languageDescription" Core..=) Core.<$> languageDescription
          ]
      )

instance Core.FromJSON CaptionDescription where
  parseJSON =
    Core.withObject "CaptionDescription" Core.$
      \x ->
        CaptionDescription'
          Core.<$> (x Core..: "captionSelectorName")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..:? "destinationSettings")
          Core.<*> (x Core..:? "languageCode")
          Core.<*> (x Core..:? "languageDescription")
