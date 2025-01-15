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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameSessionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.GameSession
import Amazonka.GameLift.Types.ProtectionPolicy
import qualified Amazonka.Prelude as Prelude

-- | A game session\'s properties plus the protection policy currently in
-- force.
--
-- /See:/ 'newGameSessionDetail' smart constructor.
data GameSessionDetail = GameSessionDetail'
  { -- | Object that describes a game session.
    gameSession :: Prelude.Maybe GameSession,
    -- | Current status of protection for the game session.
    --
    -- -   __NoProtection__ -- The game session can be terminated during a
    --     scale-down event.
    --
    -- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
    --     it cannot be terminated during a scale-down event.
    protectionPolicy :: Prelude.Maybe ProtectionPolicy
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
    { gameSession = Prelude.Nothing,
      protectionPolicy = Prelude.Nothing
    }

-- | Object that describes a game session.
gameSessionDetail_gameSession :: Lens.Lens' GameSessionDetail (Prelude.Maybe GameSession)
gameSessionDetail_gameSession = Lens.lens (\GameSessionDetail' {gameSession} -> gameSession) (\s@GameSessionDetail' {} a -> s {gameSession = a} :: GameSessionDetail)

-- | Current status of protection for the game session.
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
gameSessionDetail_protectionPolicy :: Lens.Lens' GameSessionDetail (Prelude.Maybe ProtectionPolicy)
gameSessionDetail_protectionPolicy = Lens.lens (\GameSessionDetail' {protectionPolicy} -> protectionPolicy) (\s@GameSessionDetail' {} a -> s {protectionPolicy = a} :: GameSessionDetail)

instance Data.FromJSON GameSessionDetail where
  parseJSON =
    Data.withObject
      "GameSessionDetail"
      ( \x ->
          GameSessionDetail'
            Prelude.<$> (x Data..:? "GameSession")
            Prelude.<*> (x Data..:? "ProtectionPolicy")
      )

instance Prelude.Hashable GameSessionDetail where
  hashWithSalt _salt GameSessionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` gameSession
      `Prelude.hashWithSalt` protectionPolicy

instance Prelude.NFData GameSessionDetail where
  rnf GameSessionDetail' {..} =
    Prelude.rnf gameSession `Prelude.seq`
      Prelude.rnf protectionPolicy
