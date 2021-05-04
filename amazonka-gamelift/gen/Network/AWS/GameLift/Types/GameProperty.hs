{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameProperty where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Set of key-value pairs that contain information about a game session.
-- When included in a game session request, these properties communicate
-- details to be used when setting up the new game session. For example, a
-- game property might specify a game mode, level, or map. Game properties
-- are passed to the game server process when initiating a new game
-- session. For more information, see the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-client-api.html#gamelift-sdk-client-api-create Amazon GameLift Developer Guide>.
--
-- /See:/ 'newGameProperty' smart constructor.
data GameProperty = GameProperty'
  { -- | The game property identifier.
    key :: Prelude.Text,
    -- | The game property value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GameProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'gameProperty_key' - The game property identifier.
--
-- 'value', 'gameProperty_value' - The game property value.
newGameProperty ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  GameProperty
newGameProperty pKey_ pValue_ =
  GameProperty' {key = pKey_, value = pValue_}

-- | The game property identifier.
gameProperty_key :: Lens.Lens' GameProperty Prelude.Text
gameProperty_key = Lens.lens (\GameProperty' {key} -> key) (\s@GameProperty' {} a -> s {key = a} :: GameProperty)

-- | The game property value.
gameProperty_value :: Lens.Lens' GameProperty Prelude.Text
gameProperty_value = Lens.lens (\GameProperty' {value} -> value) (\s@GameProperty' {} a -> s {value = a} :: GameProperty)

instance Prelude.FromJSON GameProperty where
  parseJSON =
    Prelude.withObject
      "GameProperty"
      ( \x ->
          GameProperty'
            Prelude.<$> (x Prelude..: "Key")
            Prelude.<*> (x Prelude..: "Value")
      )

instance Prelude.Hashable GameProperty

instance Prelude.NFData GameProperty

instance Prelude.ToJSON GameProperty where
  toJSON GameProperty' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Prelude..= key),
            Prelude.Just ("Value" Prelude..= value)
          ]
      )
