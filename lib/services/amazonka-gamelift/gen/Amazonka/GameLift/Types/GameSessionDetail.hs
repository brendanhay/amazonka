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
-- Module      : Amazonka.GameLift.Types.GameSessionDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameSessionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types.GameSession
import Amazonka.GameLift.Types.ProtectionPolicy
import qualified Amazonka.Prelude as Prelude

-- | A game session\'s properties plus the protection policy currently in
-- force.
--
-- /See:/ 'newGameSessionDetail' smart constructor.
data GameSessionDetail = GameSessionDetail'
  { -- | Current status of protection for the game session.
    --
    -- -   __NoProtection__ -- The game session can be terminated during a
    --     scale-down event.
    --
    -- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
    --     it cannot be terminated during a scale-down event.
    protectionPolicy :: Prelude.Maybe ProtectionPolicy,
    -- | Object that describes a game session.
    gameSession :: Prelude.Maybe GameSession
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameSessionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionPolicy', 'gameSessionDetail_protectionPolicy' - Current status of protection for the game session.
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
--
-- 'gameSession', 'gameSessionDetail_gameSession' - Object that describes a game session.
newGameSessionDetail ::
  GameSessionDetail
newGameSessionDetail =
  GameSessionDetail'
    { protectionPolicy =
        Prelude.Nothing,
      gameSession = Prelude.Nothing
    }

-- | Current status of protection for the game session.
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
gameSessionDetail_protectionPolicy :: Lens.Lens' GameSessionDetail (Prelude.Maybe ProtectionPolicy)
gameSessionDetail_protectionPolicy = Lens.lens (\GameSessionDetail' {protectionPolicy} -> protectionPolicy) (\s@GameSessionDetail' {} a -> s {protectionPolicy = a} :: GameSessionDetail)

-- | Object that describes a game session.
gameSessionDetail_gameSession :: Lens.Lens' GameSessionDetail (Prelude.Maybe GameSession)
gameSessionDetail_gameSession = Lens.lens (\GameSessionDetail' {gameSession} -> gameSession) (\s@GameSessionDetail' {} a -> s {gameSession = a} :: GameSessionDetail)

instance Core.FromJSON GameSessionDetail where
  parseJSON =
    Core.withObject
      "GameSessionDetail"
      ( \x ->
          GameSessionDetail'
            Prelude.<$> (x Core..:? "ProtectionPolicy")
            Prelude.<*> (x Core..:? "GameSession")
      )

instance Prelude.Hashable GameSessionDetail where
  hashWithSalt _salt GameSessionDetail' {..} =
    _salt `Prelude.hashWithSalt` protectionPolicy
      `Prelude.hashWithSalt` gameSession

instance Prelude.NFData GameSessionDetail where
  rnf GameSessionDetail' {..} =
    Prelude.rnf protectionPolicy
      `Prelude.seq` Prelude.rnf gameSession
