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
-- Module      : Network.AWS.GameLift.Types.MatchedPlayerSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchedPlayerSession where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a new player session that is created as a result of a
-- successful FlexMatch match. A successful match automatically creates new
-- player sessions for every player ID in the original matchmaking request.
--
-- When players connect to the match\'s game session, they must include
-- both player ID and player session ID in order to claim their assigned
-- player slot.
--
-- /See:/ 'newMatchedPlayerSession' smart constructor.
data MatchedPlayerSession = MatchedPlayerSession'
  { -- | A unique identifier for a player
    playerId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player session
    playerSessionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MatchedPlayerSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerId', 'matchedPlayerSession_playerId' - A unique identifier for a player
--
-- 'playerSessionId', 'matchedPlayerSession_playerSessionId' - A unique identifier for a player session
newMatchedPlayerSession ::
  MatchedPlayerSession
newMatchedPlayerSession =
  MatchedPlayerSession'
    { playerId = Prelude.Nothing,
      playerSessionId = Prelude.Nothing
    }

-- | A unique identifier for a player
matchedPlayerSession_playerId :: Lens.Lens' MatchedPlayerSession (Prelude.Maybe Prelude.Text)
matchedPlayerSession_playerId = Lens.lens (\MatchedPlayerSession' {playerId} -> playerId) (\s@MatchedPlayerSession' {} a -> s {playerId = a} :: MatchedPlayerSession)

-- | A unique identifier for a player session
matchedPlayerSession_playerSessionId :: Lens.Lens' MatchedPlayerSession (Prelude.Maybe Prelude.Text)
matchedPlayerSession_playerSessionId = Lens.lens (\MatchedPlayerSession' {playerSessionId} -> playerSessionId) (\s@MatchedPlayerSession' {} a -> s {playerSessionId = a} :: MatchedPlayerSession)

instance Prelude.FromJSON MatchedPlayerSession where
  parseJSON =
    Prelude.withObject
      "MatchedPlayerSession"
      ( \x ->
          MatchedPlayerSession'
            Prelude.<$> (x Prelude..:? "PlayerId")
            Prelude.<*> (x Prelude..:? "PlayerSessionId")
      )

instance Prelude.Hashable MatchedPlayerSession

instance Prelude.NFData MatchedPlayerSession
