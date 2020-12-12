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
    cdLanguageCode,
    cdDestinationSettings,
    cdLanguageDescription,
    cdCaptionSelectorName,
    cdName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CaptionDestinationSettings
import qualified Network.AWS.Prelude as Lude

-- | Caption Description
--
-- /See:/ 'mkCaptionDescription' smart constructor.
data CaptionDescription = CaptionDescription'
  { languageCode ::
      Lude.Maybe Lude.Text,
    destinationSettings ::
      Lude.Maybe CaptionDestinationSettings,
    languageDescription :: Lude.Maybe Lude.Text,
    captionSelectorName :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptionDescription' with the minimum fields required to make a request.
--
-- * 'captionSelectorName' - Specifies which input caption selector to use as a caption source when generating output captions. This field should match a captionSelector name.
-- * 'destinationSettings' - Additional settings for captions destination that depend on the destination type.
-- * 'languageCode' - ISO 639-2 three-digit code: http://www.loc.gov/standards/iso639-2/
-- * 'languageDescription' - Human readable information to indicate captions available for players (eg. English, or Spanish).
-- * 'name' - Name of the caption description.  Used to associate a caption description with an output.  Names must be unique within an event.
mkCaptionDescription ::
  -- | 'captionSelectorName'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CaptionDescription
mkCaptionDescription pCaptionSelectorName_ pName_ =
  CaptionDescription'
    { languageCode = Lude.Nothing,
      destinationSettings = Lude.Nothing,
      languageDescription = Lude.Nothing,
      captionSelectorName = pCaptionSelectorName_,
      name = pName_
    }

-- | ISO 639-2 three-digit code: http://www.loc.gov/standards/iso639-2/
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLanguageCode :: Lens.Lens' CaptionDescription (Lude.Maybe Lude.Text)
cdLanguageCode = Lens.lens (languageCode :: CaptionDescription -> Lude.Maybe Lude.Text) (\s a -> s {languageCode = a} :: CaptionDescription)
{-# DEPRECATED cdLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Additional settings for captions destination that depend on the destination type.
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDestinationSettings :: Lens.Lens' CaptionDescription (Lude.Maybe CaptionDestinationSettings)
cdDestinationSettings = Lens.lens (destinationSettings :: CaptionDescription -> Lude.Maybe CaptionDestinationSettings) (\s a -> s {destinationSettings = a} :: CaptionDescription)
{-# DEPRECATED cdDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

-- | Human readable information to indicate captions available for players (eg. English, or Spanish).
--
-- /Note:/ Consider using 'languageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLanguageDescription :: Lens.Lens' CaptionDescription (Lude.Maybe Lude.Text)
cdLanguageDescription = Lens.lens (languageDescription :: CaptionDescription -> Lude.Maybe Lude.Text) (\s a -> s {languageDescription = a} :: CaptionDescription)
{-# DEPRECATED cdLanguageDescription "Use generic-lens or generic-optics with 'languageDescription' instead." #-}

-- | Specifies which input caption selector to use as a caption source when generating output captions. This field should match a captionSelector name.
--
-- /Note:/ Consider using 'captionSelectorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCaptionSelectorName :: Lens.Lens' CaptionDescription Lude.Text
cdCaptionSelectorName = Lens.lens (captionSelectorName :: CaptionDescription -> Lude.Text) (\s a -> s {captionSelectorName = a} :: CaptionDescription)
{-# DEPRECATED cdCaptionSelectorName "Use generic-lens or generic-optics with 'captionSelectorName' instead." #-}

-- | Name of the caption description.  Used to associate a caption description with an output.  Names must be unique within an event.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' CaptionDescription Lude.Text
cdName = Lens.lens (name :: CaptionDescription -> Lude.Text) (\s a -> s {name = a} :: CaptionDescription)
{-# DEPRECATED cdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON CaptionDescription where
  parseJSON =
    Lude.withObject
      "CaptionDescription"
      ( \x ->
          CaptionDescription'
            Lude.<$> (x Lude..:? "languageCode")
            Lude.<*> (x Lude..:? "destinationSettings")
            Lude.<*> (x Lude..:? "languageDescription")
            Lude.<*> (x Lude..: "captionSelectorName")
            Lude.<*> (x Lude..: "name")
      )

instance Lude.ToJSON CaptionDescription where
  toJSON CaptionDescription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("languageCode" Lude..=) Lude.<$> languageCode,
            ("destinationSettings" Lude..=) Lude.<$> destinationSettings,
            ("languageDescription" Lude..=) Lude.<$> languageDescription,
            Lude.Just ("captionSelectorName" Lude..= captionSelectorName),
            Lude.Just ("name" Lude..= name)
          ]
      )
