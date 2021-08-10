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
-- Module      : Network.AWS.LexModels.Types.OutputContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.OutputContext where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON OutputContext where
  parseJSON =
    Core.withObject
      "OutputContext"
      ( \x ->
          OutputContext'
            Prelude.<$> (x Core..: "name")
            Prelude.<*> (x Core..: "timeToLiveInSeconds")
            Prelude.<*> (x Core..: "turnsToLive")
      )

instance Prelude.Hashable OutputContext

instance Prelude.NFData OutputContext

instance Core.ToJSON OutputContext where
  toJSON OutputContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("timeToLiveInSeconds" Core..= timeToLiveInSeconds),
            Prelude.Just ("turnsToLive" Core..= turnsToLive)
          ]
      )
