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
-- Module      : Amazonka.GameLift.Types.DesiredPlayerSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.DesiredPlayerSession where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Player information for use when creating player sessions using a game
-- session placement request.
--
-- /See:/ 'newDesiredPlayerSession' smart constructor.
data DesiredPlayerSession = DesiredPlayerSession'
  { -- | Developer-defined information related to a player. Amazon GameLift does
    -- not use this data, so it can be formatted as needed for use in the game.
    playerData :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player to associate with the player session.
    playerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DesiredPlayerSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerData', 'desiredPlayerSession_playerData' - Developer-defined information related to a player. Amazon GameLift does
-- not use this data, so it can be formatted as needed for use in the game.
--
-- 'playerId', 'desiredPlayerSession_playerId' - A unique identifier for a player to associate with the player session.
newDesiredPlayerSession ::
  DesiredPlayerSession
newDesiredPlayerSession =
  DesiredPlayerSession'
    { playerData = Prelude.Nothing,
      playerId = Prelude.Nothing
    }

-- | Developer-defined information related to a player. Amazon GameLift does
-- not use this data, so it can be formatted as needed for use in the game.
desiredPlayerSession_playerData :: Lens.Lens' DesiredPlayerSession (Prelude.Maybe Prelude.Text)
desiredPlayerSession_playerData = Lens.lens (\DesiredPlayerSession' {playerData} -> playerData) (\s@DesiredPlayerSession' {} a -> s {playerData = a} :: DesiredPlayerSession)

-- | A unique identifier for a player to associate with the player session.
desiredPlayerSession_playerId :: Lens.Lens' DesiredPlayerSession (Prelude.Maybe Prelude.Text)
desiredPlayerSession_playerId = Lens.lens (\DesiredPlayerSession' {playerId} -> playerId) (\s@DesiredPlayerSession' {} a -> s {playerId = a} :: DesiredPlayerSession)

instance Prelude.Hashable DesiredPlayerSession where
  hashWithSalt _salt DesiredPlayerSession' {..} =
    _salt
      `Prelude.hashWithSalt` playerData
      `Prelude.hashWithSalt` playerId

instance Prelude.NFData DesiredPlayerSession where
  rnf DesiredPlayerSession' {..} =
    Prelude.rnf playerData
      `Prelude.seq` Prelude.rnf playerId

instance Data.ToJSON DesiredPlayerSession where
  toJSON DesiredPlayerSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PlayerData" Data..=) Prelude.<$> playerData,
            ("PlayerId" Data..=) Prelude.<$> playerId
          ]
      )
