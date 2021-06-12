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
-- Module      : Network.AWS.GameLift.Types.DesiredPlayerSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.DesiredPlayerSession where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Player information for use when creating player sessions using a game
-- session placement request with StartGameSessionPlacement.
--
-- /See:/ 'newDesiredPlayerSession' smart constructor.
data DesiredPlayerSession = DesiredPlayerSession'
  { -- | A unique identifier for a player to associate with the player session.
    playerId :: Core.Maybe Core.Text,
    -- | Developer-defined information related to a player. Amazon GameLift does
    -- not use this data, so it can be formatted as needed for use in the game.
    playerData :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DesiredPlayerSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerId', 'desiredPlayerSession_playerId' - A unique identifier for a player to associate with the player session.
--
-- 'playerData', 'desiredPlayerSession_playerData' - Developer-defined information related to a player. Amazon GameLift does
-- not use this data, so it can be formatted as needed for use in the game.
newDesiredPlayerSession ::
  DesiredPlayerSession
newDesiredPlayerSession =
  DesiredPlayerSession'
    { playerId = Core.Nothing,
      playerData = Core.Nothing
    }

-- | A unique identifier for a player to associate with the player session.
desiredPlayerSession_playerId :: Lens.Lens' DesiredPlayerSession (Core.Maybe Core.Text)
desiredPlayerSession_playerId = Lens.lens (\DesiredPlayerSession' {playerId} -> playerId) (\s@DesiredPlayerSession' {} a -> s {playerId = a} :: DesiredPlayerSession)

-- | Developer-defined information related to a player. Amazon GameLift does
-- not use this data, so it can be formatted as needed for use in the game.
desiredPlayerSession_playerData :: Lens.Lens' DesiredPlayerSession (Core.Maybe Core.Text)
desiredPlayerSession_playerData = Lens.lens (\DesiredPlayerSession' {playerData} -> playerData) (\s@DesiredPlayerSession' {} a -> s {playerData = a} :: DesiredPlayerSession)

instance Core.Hashable DesiredPlayerSession

instance Core.NFData DesiredPlayerSession

instance Core.ToJSON DesiredPlayerSession where
  toJSON DesiredPlayerSession' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PlayerId" Core..=) Core.<$> playerId,
            ("PlayerData" Core..=) Core.<$> playerData
          ]
      )
