{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputGroup
  ( OutputGroup (..),

    -- * Smart constructor
    mkOutputGroup,

    -- * Lenses
    ogAutomatedEncodingSettings,
    ogCustomName,
    ogName,
    ogOutputGroupSettings,
    ogOutputs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AutomatedEncodingSettings as Types
import qualified Network.AWS.MediaConvert.Types.Output as Types
import qualified Network.AWS.MediaConvert.Types.OutputGroupSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Group of outputs
--
-- /See:/ 'mkOutputGroup' smart constructor.
data OutputGroup = OutputGroup'
  { -- | Use automated encoding to have MediaConvert choose your encoding settings for you, based on characteristics of your input video.
    automatedEncodingSettings :: Core.Maybe Types.AutomatedEncodingSettings,
    -- | Use Custom Group Name (CustomName) to specify a name for the output group. This value is displayed on the console and can make your job settings JSON more human-readable. It does not affect your outputs. Use up to twelve characters that are either letters, numbers, spaces, or underscores.
    customName :: Core.Maybe Core.Text,
    -- | Name of the output group
    name :: Core.Maybe Core.Text,
    -- | Output Group settings, including type
    outputGroupSettings :: Core.Maybe Types.OutputGroupSettings,
    -- | This object holds groups of encoding settings, one group of settings per output.
    outputs :: Core.Maybe [Types.Output]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputGroup' value with any optional fields omitted.
mkOutputGroup ::
  OutputGroup
mkOutputGroup =
  OutputGroup'
    { automatedEncodingSettings = Core.Nothing,
      customName = Core.Nothing,
      name = Core.Nothing,
      outputGroupSettings = Core.Nothing,
      outputs = Core.Nothing
    }

-- | Use automated encoding to have MediaConvert choose your encoding settings for you, based on characteristics of your input video.
--
-- /Note:/ Consider using 'automatedEncodingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogAutomatedEncodingSettings :: Lens.Lens' OutputGroup (Core.Maybe Types.AutomatedEncodingSettings)
ogAutomatedEncodingSettings = Lens.field @"automatedEncodingSettings"
{-# DEPRECATED ogAutomatedEncodingSettings "Use generic-lens or generic-optics with 'automatedEncodingSettings' instead." #-}

-- | Use Custom Group Name (CustomName) to specify a name for the output group. This value is displayed on the console and can make your job settings JSON more human-readable. It does not affect your outputs. Use up to twelve characters that are either letters, numbers, spaces, or underscores.
--
-- /Note:/ Consider using 'customName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogCustomName :: Lens.Lens' OutputGroup (Core.Maybe Core.Text)
ogCustomName = Lens.field @"customName"
{-# DEPRECATED ogCustomName "Use generic-lens or generic-optics with 'customName' instead." #-}

-- | Name of the output group
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogName :: Lens.Lens' OutputGroup (Core.Maybe Core.Text)
ogName = Lens.field @"name"
{-# DEPRECATED ogName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Output Group settings, including type
--
-- /Note:/ Consider using 'outputGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOutputGroupSettings :: Lens.Lens' OutputGroup (Core.Maybe Types.OutputGroupSettings)
ogOutputGroupSettings = Lens.field @"outputGroupSettings"
{-# DEPRECATED ogOutputGroupSettings "Use generic-lens or generic-optics with 'outputGroupSettings' instead." #-}

-- | This object holds groups of encoding settings, one group of settings per output.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOutputs :: Lens.Lens' OutputGroup (Core.Maybe [Types.Output])
ogOutputs = Lens.field @"outputs"
{-# DEPRECATED ogOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

instance Core.FromJSON OutputGroup where
  toJSON OutputGroup {..} =
    Core.object
      ( Core.catMaybes
          [ ("automatedEncodingSettings" Core..=)
              Core.<$> automatedEncodingSettings,
            ("customName" Core..=) Core.<$> customName,
            ("name" Core..=) Core.<$> name,
            ("outputGroupSettings" Core..=) Core.<$> outputGroupSettings,
            ("outputs" Core..=) Core.<$> outputs
          ]
      )

instance Core.FromJSON OutputGroup where
  parseJSON =
    Core.withObject "OutputGroup" Core.$
      \x ->
        OutputGroup'
          Core.<$> (x Core..:? "automatedEncodingSettings")
          Core.<*> (x Core..:? "customName")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "outputGroupSettings")
          Core.<*> (x Core..:? "outputs")
