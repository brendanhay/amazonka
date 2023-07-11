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
-- Module      : Amazonka.LexModels.Types.OutputContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.OutputContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The specification of an output context that is set when an intent is
-- fulfilled.
--
-- /See:/ 'newOutputContext' smart constructor.
data OutputContext = OutputContext'
  { -- | The name of the context.
    name :: Prelude.Text,
    -- | The number of seconds that the context should be active after it is
    -- first sent in a @PostContent@ or @PostText@ response. You can set the
    -- value between 5 and 86,400 seconds (24 hours).
    timeToLiveInSeconds :: Prelude.Natural,
    -- | The number of conversation turns that the context should be active. A
    -- conversation turn is one @PostContent@ or @PostText@ request and the
    -- corresponding response from Amazon Lex.
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
-- 'name', 'outputContext_name' - The name of the context.
--
-- 'timeToLiveInSeconds', 'outputContext_timeToLiveInSeconds' - The number of seconds that the context should be active after it is
-- first sent in a @PostContent@ or @PostText@ response. You can set the
-- value between 5 and 86,400 seconds (24 hours).
--
-- 'turnsToLive', 'outputContext_turnsToLive' - The number of conversation turns that the context should be active. A
-- conversation turn is one @PostContent@ or @PostText@ request and the
-- corresponding response from Amazon Lex.
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

-- | The name of the context.
outputContext_name :: Lens.Lens' OutputContext Prelude.Text
outputContext_name = Lens.lens (\OutputContext' {name} -> name) (\s@OutputContext' {} a -> s {name = a} :: OutputContext)

-- | The number of seconds that the context should be active after it is
-- first sent in a @PostContent@ or @PostText@ response. You can set the
-- value between 5 and 86,400 seconds (24 hours).
outputContext_timeToLiveInSeconds :: Lens.Lens' OutputContext Prelude.Natural
outputContext_timeToLiveInSeconds = Lens.lens (\OutputContext' {timeToLiveInSeconds} -> timeToLiveInSeconds) (\s@OutputContext' {} a -> s {timeToLiveInSeconds = a} :: OutputContext)

-- | The number of conversation turns that the context should be active. A
-- conversation turn is one @PostContent@ or @PostText@ request and the
-- corresponding response from Amazon Lex.
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
    _salt
      `Prelude.hashWithSalt` name
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
