{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioSelector
  ( AudioSelector (..),

    -- * Smart constructor
    mkAudioSelector,

    -- * Lenses
    asSelectorSettings,
    asName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioSelectorSettings
import qualified Network.AWS.Prelude as Lude

-- | Audio Selector
--
-- /See:/ 'mkAudioSelector' smart constructor.
data AudioSelector = AudioSelector'
  { -- | The audio selector settings.
    selectorSettings :: Lude.Maybe AudioSelectorSettings,
    -- | The name of this AudioSelector. AudioDescriptions will use this name to uniquely identify this Selector.  Selector names should be unique per input.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioSelector' with the minimum fields required to make a request.
--
-- * 'selectorSettings' - The audio selector settings.
-- * 'name' - The name of this AudioSelector. AudioDescriptions will use this name to uniquely identify this Selector.  Selector names should be unique per input.
mkAudioSelector ::
  -- | 'name'
  Lude.Text ->
  AudioSelector
mkAudioSelector pName_ =
  AudioSelector' {selectorSettings = Lude.Nothing, name = pName_}

-- | The audio selector settings.
--
-- /Note:/ Consider using 'selectorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSelectorSettings :: Lens.Lens' AudioSelector (Lude.Maybe AudioSelectorSettings)
asSelectorSettings = Lens.lens (selectorSettings :: AudioSelector -> Lude.Maybe AudioSelectorSettings) (\s a -> s {selectorSettings = a} :: AudioSelector)
{-# DEPRECATED asSelectorSettings "Use generic-lens or generic-optics with 'selectorSettings' instead." #-}

-- | The name of this AudioSelector. AudioDescriptions will use this name to uniquely identify this Selector.  Selector names should be unique per input.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asName :: Lens.Lens' AudioSelector Lude.Text
asName = Lens.lens (name :: AudioSelector -> Lude.Text) (\s a -> s {name = a} :: AudioSelector)
{-# DEPRECATED asName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AudioSelector where
  parseJSON =
    Lude.withObject
      "AudioSelector"
      ( \x ->
          AudioSelector'
            Lude.<$> (x Lude..:? "selectorSettings") Lude.<*> (x Lude..: "name")
      )

instance Lude.ToJSON AudioSelector where
  toJSON AudioSelector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("selectorSettings" Lude..=) Lude.<$> selectorSettings,
            Lude.Just ("name" Lude..= name)
          ]
      )
