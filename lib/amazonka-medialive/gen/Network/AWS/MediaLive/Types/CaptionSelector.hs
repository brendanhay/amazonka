{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionSelector
  ( CaptionSelector (..),

    -- * Smart constructor
    mkCaptionSelector,

    -- * Lenses
    cLanguageCode,
    cSelectorSettings,
    cName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CaptionSelectorSettings
import qualified Network.AWS.Prelude as Lude

-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
--
-- /See:/ 'mkCaptionSelector' smart constructor.
data CaptionSelector = CaptionSelector'
  { languageCode ::
      Lude.Maybe Lude.Text,
    selectorSettings :: Lude.Maybe CaptionSelectorSettings,
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

-- | Creates a value of 'CaptionSelector' with the minimum fields required to make a request.
--
-- * 'languageCode' - When specified this field indicates the three letter language code of the caption track to extract from the source.
-- * 'name' - Name identifier for a caption selector.  This name is used to associate this caption selector with one or more caption descriptions.  Names must be unique within an event.
-- * 'selectorSettings' - Caption selector settings.
mkCaptionSelector ::
  -- | 'name'
  Lude.Text ->
  CaptionSelector
mkCaptionSelector pName_ =
  CaptionSelector'
    { languageCode = Lude.Nothing,
      selectorSettings = Lude.Nothing,
      name = pName_
    }

-- | When specified this field indicates the three letter language code of the caption track to extract from the source.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLanguageCode :: Lens.Lens' CaptionSelector (Lude.Maybe Lude.Text)
cLanguageCode = Lens.lens (languageCode :: CaptionSelector -> Lude.Maybe Lude.Text) (\s a -> s {languageCode = a} :: CaptionSelector)
{-# DEPRECATED cLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | Caption selector settings.
--
-- /Note:/ Consider using 'selectorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSelectorSettings :: Lens.Lens' CaptionSelector (Lude.Maybe CaptionSelectorSettings)
cSelectorSettings = Lens.lens (selectorSettings :: CaptionSelector -> Lude.Maybe CaptionSelectorSettings) (\s a -> s {selectorSettings = a} :: CaptionSelector)
{-# DEPRECATED cSelectorSettings "Use generic-lens or generic-optics with 'selectorSettings' instead." #-}

-- | Name identifier for a caption selector.  This name is used to associate this caption selector with one or more caption descriptions.  Names must be unique within an event.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CaptionSelector Lude.Text
cName = Lens.lens (name :: CaptionSelector -> Lude.Text) (\s a -> s {name = a} :: CaptionSelector)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON CaptionSelector where
  parseJSON =
    Lude.withObject
      "CaptionSelector"
      ( \x ->
          CaptionSelector'
            Lude.<$> (x Lude..:? "languageCode")
            Lude.<*> (x Lude..:? "selectorSettings")
            Lude.<*> (x Lude..: "name")
      )

instance Lude.ToJSON CaptionSelector where
  toJSON CaptionSelector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("languageCode" Lude..=) Lude.<$> languageCode,
            ("selectorSettings" Lude..=) Lude.<$> selectorSettings,
            Lude.Just ("name" Lude..= name)
          ]
      )
