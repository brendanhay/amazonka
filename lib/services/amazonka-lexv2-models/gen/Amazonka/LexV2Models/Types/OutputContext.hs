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
-- Module      : Amazonka.LexV2Models.Types.OutputContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.OutputContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a session context that is activated when an intent is
-- fulfilled.
--
-- /See:/ 'newOutputContext' smart constructor.
data OutputContext = OutputContext'
  { -- | The name of the output context.
    name :: Prelude.Text,
    -- | The amount of time, in seconds, that the output context should remain
    -- active. The time is figured from the first time the context is sent to
    -- the user.
    timeToLiveInSeconds :: Prelude.Natural,
    -- | The number of conversation turns that the output context should remain
    -- active. The number of turns is counted from the first time that the
    -- context is sent to the user.
    turnsToLive :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'outputContext_name' - The name of the output context.
--
-- 'timeToLiveInSeconds', 'outputContext_timeToLiveInSeconds' - The amount of time, in seconds, that the output context should remain
-- active. The time is figured from the first time the context is sent to
-- the user.
--
-- 'turnsToLive', 'outputContext_turnsToLive' - The number of conversation turns that the output context should remain
-- active. The number of turns is counted from the first time that the
-- context is sent to the user.
newOutputContext ::
  -- | 'name'
  Prelude.Text ->
  -- | 'timeToLiveInSeconds'
  Prelude.Natural ->
  -- | 'turnsToLive'
  Prelude.Natural ->
  OutputContext
newOutputContext
  pName_
  pTimeToLiveInSeconds_
  pTurnsToLive_ =
    OutputContext'
      { name = pName_,
        timeToLiveInSeconds = pTimeToLiveInSeconds_,
        turnsToLive = pTurnsToLive_
      }

-- | The name of the output context.
outputContext_name :: Lens.Lens' OutputContext Prelude.Text
outputContext_name = Lens.lens (\OutputContext' {name} -> name) (\s@OutputContext' {} a -> s {name = a} :: OutputContext)

-- | The amount of time, in seconds, that the output context should remain
-- active. The time is figured from the first time the context is sent to
-- the user.
outputContext_timeToLiveInSeconds :: Lens.Lens' OutputContext Prelude.Natural
outputContext_timeToLiveInSeconds = Lens.lens (\OutputContext' {timeToLiveInSeconds} -> timeToLiveInSeconds) (\s@OutputContext' {} a -> s {timeToLiveInSeconds = a} :: OutputContext)

-- | The number of conversation turns that the output context should remain
-- active. The number of turns is counted from the first time that the
-- context is sent to the user.
outputContext_turnsToLive :: Lens.Lens' OutputContext Prelude.Natural
outputContext_turnsToLive = Lens.lens (\OutputContext' {turnsToLive} -> turnsToLive) (\s@OutputContext' {} a -> s {turnsToLive = a} :: OutputContext)

instance Data.FromJSON OutputContext where
  parseJSON =
    Data.withObject
      "OutputContext"
      ( \x ->
          OutputContext'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "timeToLiveInSeconds")
            Prelude.<*> (x Data..: "turnsToLive")
      )

instance Prelude.Hashable OutputContext where
  hashWithSalt _salt OutputContext' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` timeToLiveInSeconds
      `Prelude.hashWithSalt` turnsToLive

instance Prelude.NFData OutputContext where
  rnf OutputContext' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf timeToLiveInSeconds
      `Prelude.seq` Prelude.rnf turnsToLive

instance Data.ToJSON OutputContext where
  toJSON OutputContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("timeToLiveInSeconds" Data..= timeToLiveInSeconds),
            Prelude.Just ("turnsToLive" Data..= turnsToLive)
          ]
      )
