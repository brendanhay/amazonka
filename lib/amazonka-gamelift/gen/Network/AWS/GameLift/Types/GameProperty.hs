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
    gpKey,
    gpValue,
  )
where

import qualified Network.AWS.GameLift.Types.GamePropertyKey as Types
import qualified Network.AWS.GameLift.Types.GamePropertyValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Set of key-value pairs that contain information about a game session. When included in a game session request, these properties communicate details to be used when setting up the new game session. For example, a game property might specify a game mode, level, or map. Game properties are passed to the game server process when initiating a new game session. For more information, see the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-client-api.html#gamelift-sdk-client-api-create Amazon GameLift Developer Guide> .
--
-- /See:/ 'mkGameProperty' smart constructor.
data GameProperty = GameProperty'
  { -- | The game property identifier.
    key :: Types.GamePropertyKey,
    -- | The game property value.
    value :: Types.GamePropertyValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GameProperty' value with any optional fields omitted.
mkGameProperty ::
  -- | 'key'
  Types.GamePropertyKey ->
  -- | 'value'
  Types.GamePropertyValue ->
  GameProperty
mkGameProperty key value = GameProperty' {key, value}

-- | The game property identifier.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpKey :: Lens.Lens' GameProperty Types.GamePropertyKey
gpKey = Lens.field @"key"
{-# DEPRECATED gpKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The game property value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpValue :: Lens.Lens' GameProperty Types.GamePropertyValue
gpValue = Lens.field @"value"
{-# DEPRECATED gpValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON GameProperty where
  toJSON GameProperty {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Key" Core..= key), Core.Just ("Value" Core..= value)]
      )

instance Core.FromJSON GameProperty where
  parseJSON =
    Core.withObject "GameProperty" Core.$
      \x ->
        GameProperty'
          Core.<$> (x Core..: "Key") Core.<*> (x Core..: "Value")
