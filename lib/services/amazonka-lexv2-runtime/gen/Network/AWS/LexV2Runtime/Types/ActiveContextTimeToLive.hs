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
-- Module      : Network.AWS.LexV2Runtime.Types.ActiveContextTimeToLive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.ActiveContextTimeToLive where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The time that a context is active. You can specify the time to live in
-- seconds or in conversation turns.
--
-- /See:/ 'newActiveContextTimeToLive' smart constructor.
data ActiveContextTimeToLive = ActiveContextTimeToLive'
  { -- | The number of seconds that the context is active. You can specify
    -- between 5 and 86400 seconds (24 hours).
    timeToLiveInSeconds :: Prelude.Natural,
    -- | The number of turns that the context is active. You can specify up to 20
    -- turns. Each request and response from the bot is a turn.
    turnsToLive :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActiveContextTimeToLive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeToLiveInSeconds', 'activeContextTimeToLive_timeToLiveInSeconds' - The number of seconds that the context is active. You can specify
-- between 5 and 86400 seconds (24 hours).
--
-- 'turnsToLive', 'activeContextTimeToLive_turnsToLive' - The number of turns that the context is active. You can specify up to 20
-- turns. Each request and response from the bot is a turn.
newActiveContextTimeToLive ::
  -- | 'timeToLiveInSeconds'
  Prelude.Natural ->
  -- | 'turnsToLive'
  Prelude.Natural ->
  ActiveContextTimeToLive
newActiveContextTimeToLive
  pTimeToLiveInSeconds_
  pTurnsToLive_ =
    ActiveContextTimeToLive'
      { timeToLiveInSeconds =
          pTimeToLiveInSeconds_,
        turnsToLive = pTurnsToLive_
      }

-- | The number of seconds that the context is active. You can specify
-- between 5 and 86400 seconds (24 hours).
activeContextTimeToLive_timeToLiveInSeconds :: Lens.Lens' ActiveContextTimeToLive Prelude.Natural
activeContextTimeToLive_timeToLiveInSeconds = Lens.lens (\ActiveContextTimeToLive' {timeToLiveInSeconds} -> timeToLiveInSeconds) (\s@ActiveContextTimeToLive' {} a -> s {timeToLiveInSeconds = a} :: ActiveContextTimeToLive)

-- | The number of turns that the context is active. You can specify up to 20
-- turns. Each request and response from the bot is a turn.
activeContextTimeToLive_turnsToLive :: Lens.Lens' ActiveContextTimeToLive Prelude.Natural
activeContextTimeToLive_turnsToLive = Lens.lens (\ActiveContextTimeToLive' {turnsToLive} -> turnsToLive) (\s@ActiveContextTimeToLive' {} a -> s {turnsToLive = a} :: ActiveContextTimeToLive)

instance Core.FromJSON ActiveContextTimeToLive where
  parseJSON =
    Core.withObject
      "ActiveContextTimeToLive"
      ( \x ->
          ActiveContextTimeToLive'
            Prelude.<$> (x Core..: "timeToLiveInSeconds")
            Prelude.<*> (x Core..: "turnsToLive")
      )

instance Prelude.Hashable ActiveContextTimeToLive

instance Prelude.NFData ActiveContextTimeToLive

instance Core.ToJSON ActiveContextTimeToLive where
  toJSON ActiveContextTimeToLive' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("timeToLiveInSeconds" Core..= timeToLiveInSeconds),
            Prelude.Just ("turnsToLive" Core..= turnsToLive)
          ]
      )
