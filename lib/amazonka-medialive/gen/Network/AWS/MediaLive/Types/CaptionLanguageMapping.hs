{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionLanguageMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionLanguageMapping
  ( CaptionLanguageMapping (..),

    -- * Smart constructor
    mkCaptionLanguageMapping,

    -- * Lenses
    clmLanguageCode,
    clmLanguageDescription,
    clmCaptionChannel,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Maps a caption channel to an ISO 693-2 language code (http://www.loc.gov/standards/iso639-2), with an optional description.
--
-- /See:/ 'mkCaptionLanguageMapping' smart constructor.
data CaptionLanguageMapping = CaptionLanguageMapping'
  { languageCode ::
      Lude.Text,
    languageDescription :: Lude.Text,
    captionChannel :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptionLanguageMapping' with the minimum fields required to make a request.
--
-- * 'captionChannel' - The closed caption channel being described by this CaptionLanguageMapping.  Each channel mapping must have a unique channel number (maximum of 4)
-- * 'languageCode' - Three character ISO 639-2 language code (see http://www.loc.gov/standards/iso639-2)
-- * 'languageDescription' - Textual description of language
mkCaptionLanguageMapping ::
  -- | 'languageCode'
  Lude.Text ->
  -- | 'languageDescription'
  Lude.Text ->
  -- | 'captionChannel'
  Lude.Natural ->
  CaptionLanguageMapping
mkCaptionLanguageMapping
  pLanguageCode_
  pLanguageDescription_
  pCaptionChannel_ =
    CaptionLanguageMapping'
      { languageCode = pLanguageCode_,
        languageDescription = pLanguageDescription_,
        captionChannel = pCaptionChannel_
      }

-- | Three character ISO 639-2 language code (see http://www.loc.gov/standards/iso639-2)
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmLanguageCode :: Lens.Lens' CaptionLanguageMapping Lude.Text
clmLanguageCode = Lens.lens (languageCode :: CaptionLanguageMapping -> Lude.Text) (\s a -> s {languageCode = a} :: CaptionLanguageMapping)
{-# DEPRECATED clmLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Textual description of language
--
-- /Note:/ Consider using 'languageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmLanguageDescription :: Lens.Lens' CaptionLanguageMapping Lude.Text
clmLanguageDescription = Lens.lens (languageDescription :: CaptionLanguageMapping -> Lude.Text) (\s a -> s {languageDescription = a} :: CaptionLanguageMapping)
{-# DEPRECATED clmLanguageDescription "Use generic-lens or generic-optics with 'languageDescription' instead." #-}

-- | The closed caption channel being described by this CaptionLanguageMapping.  Each channel mapping must have a unique channel number (maximum of 4)
--
-- /Note:/ Consider using 'captionChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmCaptionChannel :: Lens.Lens' CaptionLanguageMapping Lude.Natural
clmCaptionChannel = Lens.lens (captionChannel :: CaptionLanguageMapping -> Lude.Natural) (\s a -> s {captionChannel = a} :: CaptionLanguageMapping)
{-# DEPRECATED clmCaptionChannel "Use generic-lens or generic-optics with 'captionChannel' instead." #-}

instance Lude.FromJSON CaptionLanguageMapping where
  parseJSON =
    Lude.withObject
      "CaptionLanguageMapping"
      ( \x ->
          CaptionLanguageMapping'
            Lude.<$> (x Lude..: "languageCode")
            Lude.<*> (x Lude..: "languageDescription")
            Lude.<*> (x Lude..: "captionChannel")
      )

instance Lude.ToJSON CaptionLanguageMapping where
  toJSON CaptionLanguageMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("languageCode" Lude..= languageCode),
            Lude.Just ("languageDescription" Lude..= languageDescription),
            Lude.Just ("captionChannel" Lude..= captionChannel)
          ]
      )
