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
-- Module      : Amazonka.LexRuntime.Types.ActiveContextTimeToLive
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.ActiveContextTimeToLive where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The length of time or number of turns that a context remains active.
--
-- /See:/ 'newActiveContextTimeToLive' smart constructor.
data ActiveContextTimeToLive = ActiveContextTimeToLive'
  { -- | The number of seconds that the context should be active after it is
    -- first sent in a @PostContent@ or @PostText@ response. You can set the
    -- value between 5 and 86,400 seconds (24 hours).
    timeToLiveInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The number of conversation turns that the context should be active. A
    -- conversation turn is one @PostContent@ or @PostText@ request and the
    -- corresponding response from Amazon Lex.
    turnsToLive :: Prelude.Maybe Prelude.Natural
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
-- 'timeToLiveInSeconds', 'activeContextTimeToLive_timeToLiveInSeconds' - The number of seconds that the context should be active after it is
-- first sent in a @PostContent@ or @PostText@ response. You can set the
-- value between 5 and 86,400 seconds (24 hours).
--
-- 'turnsToLive', 'activeContextTimeToLive_turnsToLive' - The number of conversation turns that the context should be active. A
-- conversation turn is one @PostContent@ or @PostText@ request and the
-- corresponding response from Amazon Lex.
newActiveContextTimeToLive ::
  ActiveContextTimeToLive
newActiveContextTimeToLive =
  ActiveContextTimeToLive'
    { timeToLiveInSeconds =
        Prelude.Nothing,
      turnsToLive = Prelude.Nothing
    }

-- | The number of seconds that the context should be active after it is
-- first sent in a @PostContent@ or @PostText@ response. You can set the
-- value between 5 and 86,400 seconds (24 hours).
activeContextTimeToLive_timeToLiveInSeconds :: Lens.Lens' ActiveContextTimeToLive (Prelude.Maybe Prelude.Natural)
activeContextTimeToLive_timeToLiveInSeconds = Lens.lens (\ActiveContextTimeToLive' {timeToLiveInSeconds} -> timeToLiveInSeconds) (\s@ActiveContextTimeToLive' {} a -> s {timeToLiveInSeconds = a} :: ActiveContextTimeToLive)

-- | The number of conversation turns that the context should be active. A
-- conversation turn is one @PostContent@ or @PostText@ request and the
-- corresponding response from Amazon Lex.
activeContextTimeToLive_turnsToLive :: Lens.Lens' ActiveContextTimeToLive (Prelude.Maybe Prelude.Natural)
activeContextTimeToLive_turnsToLive = Lens.lens (\ActiveContextTimeToLive' {turnsToLive} -> turnsToLive) (\s@ActiveContextTimeToLive' {} a -> s {turnsToLive = a} :: ActiveContextTimeToLive)

instance Data.FromJSON ActiveContextTimeToLive where
  parseJSON =
    Data.withObject
      "ActiveContextTimeToLive"
      ( \x ->
          ActiveContextTimeToLive'
            Prelude.<$> (x Data..:? "timeToLiveInSeconds")
            Prelude.<*> (x Data..:? "turnsToLive")
      )

instance Prelude.Hashable ActiveContextTimeToLive where
  hashWithSalt _salt ActiveContextTimeToLive' {..} =
    _salt `Prelude.hashWithSalt` timeToLiveInSeconds
      `Prelude.hashWithSalt` turnsToLive

instance Prelude.NFData ActiveContextTimeToLive where
  rnf ActiveContextTimeToLive' {..} =
    Prelude.rnf timeToLiveInSeconds
      `Prelude.seq` Prelude.rnf turnsToLive

instance Data.ToJSON ActiveContextTimeToLive where
  toJSON ActiveContextTimeToLive' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("timeToLiveInSeconds" Data..=)
              Prelude.<$> timeToLiveInSeconds,
            ("turnsToLive" Data..=) Prelude.<$> turnsToLive
          ]
      )
