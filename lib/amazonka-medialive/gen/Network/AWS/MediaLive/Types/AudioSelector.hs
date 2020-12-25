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
    asName,
    asSelectorSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AudioSelectorSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Audio Selector
--
-- /See:/ 'mkAudioSelector' smart constructor.
data AudioSelector = AudioSelector'
  { -- | The name of this AudioSelector. AudioDescriptions will use this name to uniquely identify this Selector.  Selector names should be unique per input.
    name :: Core.Text,
    -- | The audio selector settings.
    selectorSettings :: Core.Maybe Types.AudioSelectorSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioSelector' value with any optional fields omitted.
mkAudioSelector ::
  -- | 'name'
  Core.Text ->
  AudioSelector
mkAudioSelector name =
  AudioSelector' {name, selectorSettings = Core.Nothing}

-- | The name of this AudioSelector. AudioDescriptions will use this name to uniquely identify this Selector.  Selector names should be unique per input.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asName :: Lens.Lens' AudioSelector Core.Text
asName = Lens.field @"name"
{-# DEPRECATED asName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The audio selector settings.
--
-- /Note:/ Consider using 'selectorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSelectorSettings :: Lens.Lens' AudioSelector (Core.Maybe Types.AudioSelectorSettings)
asSelectorSettings = Lens.field @"selectorSettings"
{-# DEPRECATED asSelectorSettings "Use generic-lens or generic-optics with 'selectorSettings' instead." #-}

instance Core.FromJSON AudioSelector where
  toJSON AudioSelector {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            ("selectorSettings" Core..=) Core.<$> selectorSettings
          ]
      )

instance Core.FromJSON AudioSelector where
  parseJSON =
    Core.withObject "AudioSelector" Core.$
      \x ->
        AudioSelector'
          Core.<$> (x Core..: "name") Core.<*> (x Core..:? "selectorSettings")
