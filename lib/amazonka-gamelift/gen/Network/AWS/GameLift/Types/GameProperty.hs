{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameProperty
  ( GameProperty (..),

    -- * Smart constructor
    mkGameProperty,

    -- * Lenses
    gpValue,
    gpKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Set of key-value pairs that contain information about a game session. When included in a game session request, these properties communicate details to be used when setting up the new game session. For example, a game property might specify a game mode, level, or map. Game properties are passed to the game server process when initiating a new game session. For more information, see the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-client-api.html#gamelift-sdk-client-api-create Amazon GameLift Developer Guide> .
--
-- /See:/ 'mkGameProperty' smart constructor.
data GameProperty = GameProperty'
  { -- | The game property value.
    value :: Lude.Text,
    -- | The game property identifier.
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GameProperty' with the minimum fields required to make a request.
--
-- * 'value' - The game property value.
-- * 'key' - The game property identifier.
mkGameProperty ::
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  GameProperty
mkGameProperty pValue_ pKey_ =
  GameProperty' {value = pValue_, key = pKey_}

-- | The game property value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpValue :: Lens.Lens' GameProperty Lude.Text
gpValue = Lens.lens (value :: GameProperty -> Lude.Text) (\s a -> s {value = a} :: GameProperty)
{-# DEPRECATED gpValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The game property identifier.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpKey :: Lens.Lens' GameProperty Lude.Text
gpKey = Lens.lens (key :: GameProperty -> Lude.Text) (\s a -> s {key = a} :: GameProperty)
{-# DEPRECATED gpKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON GameProperty where
  parseJSON =
    Lude.withObject
      "GameProperty"
      ( \x ->
          GameProperty'
            Lude.<$> (x Lude..: "Value") Lude.<*> (x Lude..: "Key")
      )

instance Lude.ToJSON GameProperty where
  toJSON GameProperty' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Value" Lude..= value), Lude.Just ("Key" Lude..= key)]
      )
