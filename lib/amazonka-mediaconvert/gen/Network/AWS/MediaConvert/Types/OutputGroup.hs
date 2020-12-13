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
    ogOutputGroupSettings,
    ogOutputs,
    ogCustomName,
    ogName,
    ogAutomatedEncodingSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AutomatedEncodingSettings
import Network.AWS.MediaConvert.Types.Output
import Network.AWS.MediaConvert.Types.OutputGroupSettings
import qualified Network.AWS.Prelude as Lude

-- | Group of outputs
--
-- /See:/ 'mkOutputGroup' smart constructor.
data OutputGroup = OutputGroup'
  { -- | Output Group settings, including type
    outputGroupSettings :: Lude.Maybe OutputGroupSettings,
    -- | This object holds groups of encoding settings, one group of settings per output.
    outputs :: Lude.Maybe [Output],
    -- | Use Custom Group Name (CustomName) to specify a name for the output group. This value is displayed on the console and can make your job settings JSON more human-readable. It does not affect your outputs. Use up to twelve characters that are either letters, numbers, spaces, or underscores.
    customName :: Lude.Maybe Lude.Text,
    -- | Name of the output group
    name :: Lude.Maybe Lude.Text,
    -- | Use automated encoding to have MediaConvert choose your encoding settings for you, based on characteristics of your input video.
    automatedEncodingSettings :: Lude.Maybe AutomatedEncodingSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputGroup' with the minimum fields required to make a request.
--
-- * 'outputGroupSettings' - Output Group settings, including type
-- * 'outputs' - This object holds groups of encoding settings, one group of settings per output.
-- * 'customName' - Use Custom Group Name (CustomName) to specify a name for the output group. This value is displayed on the console and can make your job settings JSON more human-readable. It does not affect your outputs. Use up to twelve characters that are either letters, numbers, spaces, or underscores.
-- * 'name' - Name of the output group
-- * 'automatedEncodingSettings' - Use automated encoding to have MediaConvert choose your encoding settings for you, based on characteristics of your input video.
mkOutputGroup ::
  OutputGroup
mkOutputGroup =
  OutputGroup'
    { outputGroupSettings = Lude.Nothing,
      outputs = Lude.Nothing,
      customName = Lude.Nothing,
      name = Lude.Nothing,
      automatedEncodingSettings = Lude.Nothing
    }

-- | Output Group settings, including type
--
-- /Note:/ Consider using 'outputGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOutputGroupSettings :: Lens.Lens' OutputGroup (Lude.Maybe OutputGroupSettings)
ogOutputGroupSettings = Lens.lens (outputGroupSettings :: OutputGroup -> Lude.Maybe OutputGroupSettings) (\s a -> s {outputGroupSettings = a} :: OutputGroup)
{-# DEPRECATED ogOutputGroupSettings "Use generic-lens or generic-optics with 'outputGroupSettings' instead." #-}

-- | This object holds groups of encoding settings, one group of settings per output.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOutputs :: Lens.Lens' OutputGroup (Lude.Maybe [Output])
ogOutputs = Lens.lens (outputs :: OutputGroup -> Lude.Maybe [Output]) (\s a -> s {outputs = a} :: OutputGroup)
{-# DEPRECATED ogOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | Use Custom Group Name (CustomName) to specify a name for the output group. This value is displayed on the console and can make your job settings JSON more human-readable. It does not affect your outputs. Use up to twelve characters that are either letters, numbers, spaces, or underscores.
--
-- /Note:/ Consider using 'customName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogCustomName :: Lens.Lens' OutputGroup (Lude.Maybe Lude.Text)
ogCustomName = Lens.lens (customName :: OutputGroup -> Lude.Maybe Lude.Text) (\s a -> s {customName = a} :: OutputGroup)
{-# DEPRECATED ogCustomName "Use generic-lens or generic-optics with 'customName' instead." #-}

-- | Name of the output group
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogName :: Lens.Lens' OutputGroup (Lude.Maybe Lude.Text)
ogName = Lens.lens (name :: OutputGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: OutputGroup)
{-# DEPRECATED ogName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Use automated encoding to have MediaConvert choose your encoding settings for you, based on characteristics of your input video.
--
-- /Note:/ Consider using 'automatedEncodingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogAutomatedEncodingSettings :: Lens.Lens' OutputGroup (Lude.Maybe AutomatedEncodingSettings)
ogAutomatedEncodingSettings = Lens.lens (automatedEncodingSettings :: OutputGroup -> Lude.Maybe AutomatedEncodingSettings) (\s a -> s {automatedEncodingSettings = a} :: OutputGroup)
{-# DEPRECATED ogAutomatedEncodingSettings "Use generic-lens or generic-optics with 'automatedEncodingSettings' instead." #-}

instance Lude.FromJSON OutputGroup where
  parseJSON =
    Lude.withObject
      "OutputGroup"
      ( \x ->
          OutputGroup'
            Lude.<$> (x Lude..:? "outputGroupSettings")
            Lude.<*> (x Lude..:? "outputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "customName")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "automatedEncodingSettings")
      )

instance Lude.ToJSON OutputGroup where
  toJSON OutputGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("outputGroupSettings" Lude..=) Lude.<$> outputGroupSettings,
            ("outputs" Lude..=) Lude.<$> outputs,
            ("customName" Lude..=) Lude.<$> customName,
            ("name" Lude..=) Lude.<$> name,
            ("automatedEncodingSettings" Lude..=)
              Lude.<$> automatedEncodingSettings
          ]
      )
