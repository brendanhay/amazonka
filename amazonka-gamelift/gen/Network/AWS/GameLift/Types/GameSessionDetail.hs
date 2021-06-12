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
-- Module      : Network.AWS.GameLift.Types.GameSessionDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.GameSession
import Network.AWS.GameLift.Types.ProtectionPolicy
import qualified Network.AWS.Lens as Lens

-- | A game session\'s properties plus the protection policy currently in
-- force.
--
-- /See:/ 'newGameSessionDetail' smart constructor.
data GameSessionDetail = GameSessionDetail'
  { -- | Object that describes a game session.
    gameSession :: Core.Maybe GameSession,
    -- | Current status of protection for the game session.
    --
    -- -   __NoProtection__ -- The game session can be terminated during a
    --     scale-down event.
    --
    -- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
    --     it cannot be terminated during a scale-down event.
    protectionPolicy :: Core.Maybe ProtectionPolicy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GameSessionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSession', 'gameSessionDetail_gameSession' - Object that describes a game session.
--
-- 'protectionPolicy', 'gameSessionDetail_protectionPolicy' - Current status of protection for the game session.
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
newGameSessionDetail ::
  GameSessionDetail
newGameSessionDetail =
  GameSessionDetail'
    { gameSession = Core.Nothing,
      protectionPolicy = Core.Nothing
    }

-- | Object that describes a game session.
gameSessionDetail_gameSession :: Lens.Lens' GameSessionDetail (Core.Maybe GameSession)
gameSessionDetail_gameSession = Lens.lens (\GameSessionDetail' {gameSession} -> gameSession) (\s@GameSessionDetail' {} a -> s {gameSession = a} :: GameSessionDetail)

-- | Current status of protection for the game session.
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
gameSessionDetail_protectionPolicy :: Lens.Lens' GameSessionDetail (Core.Maybe ProtectionPolicy)
gameSessionDetail_protectionPolicy = Lens.lens (\GameSessionDetail' {protectionPolicy} -> protectionPolicy) (\s@GameSessionDetail' {} a -> s {protectionPolicy = a} :: GameSessionDetail)

instance Core.FromJSON GameSessionDetail where
  parseJSON =
    Core.withObject
      "GameSessionDetail"
      ( \x ->
          GameSessionDetail'
            Core.<$> (x Core..:? "GameSession")
            Core.<*> (x Core..:? "ProtectionPolicy")
      )

instance Core.Hashable GameSessionDetail

instance Core.NFData GameSessionDetail
