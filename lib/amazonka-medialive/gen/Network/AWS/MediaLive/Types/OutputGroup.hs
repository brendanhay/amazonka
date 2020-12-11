-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputGroup
  ( OutputGroup (..),

    -- * Smart constructor
    mkOutputGroup,

    -- * Lenses
    ogName,
    ogOutputs,
    ogOutputGroupSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Output
import Network.AWS.MediaLive.Types.OutputGroupSettings
import qualified Network.AWS.Prelude as Lude

-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
--
-- /See:/ 'mkOutputGroup' smart constructor.
data OutputGroup = OutputGroup'
  { name :: Lude.Maybe Lude.Text,
    outputs :: [Output],
    outputGroupSettings :: OutputGroupSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputGroup' with the minimum fields required to make a request.
--
-- * 'name' - Custom output group name optionally defined by the user.  Only letters, numbers, and the underscore character allowed; only 32 characters allowed.
-- * 'outputGroupSettings' - Settings associated with the output group.
-- * 'outputs' - Undocumented field.
mkOutputGroup ::
  -- | 'outputGroupSettings'
  OutputGroupSettings ->
  OutputGroup
mkOutputGroup pOutputGroupSettings_ =
  OutputGroup'
    { name = Lude.Nothing,
      outputs = Lude.mempty,
      outputGroupSettings = pOutputGroupSettings_
    }

-- | Custom output group name optionally defined by the user.  Only letters, numbers, and the underscore character allowed; only 32 characters allowed.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogName :: Lens.Lens' OutputGroup (Lude.Maybe Lude.Text)
ogName = Lens.lens (name :: OutputGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: OutputGroup)
{-# DEPRECATED ogName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOutputs :: Lens.Lens' OutputGroup [Output]
ogOutputs = Lens.lens (outputs :: OutputGroup -> [Output]) (\s a -> s {outputs = a} :: OutputGroup)
{-# DEPRECATED ogOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | Settings associated with the output group.
--
-- /Note:/ Consider using 'outputGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOutputGroupSettings :: Lens.Lens' OutputGroup OutputGroupSettings
ogOutputGroupSettings = Lens.lens (outputGroupSettings :: OutputGroup -> OutputGroupSettings) (\s a -> s {outputGroupSettings = a} :: OutputGroup)
{-# DEPRECATED ogOutputGroupSettings "Use generic-lens or generic-optics with 'outputGroupSettings' instead." #-}

instance Lude.FromJSON OutputGroup where
  parseJSON =
    Lude.withObject
      "OutputGroup"
      ( \x ->
          OutputGroup'
            Lude.<$> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "outputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "outputGroupSettings")
      )

instance Lude.ToJSON OutputGroup where
  toJSON OutputGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("name" Lude..=) Lude.<$> name,
            Lude.Just ("outputs" Lude..= outputs),
            Lude.Just ("outputGroupSettings" Lude..= outputGroupSettings)
          ]
      )
