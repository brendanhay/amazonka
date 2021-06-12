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

-- | The specification of an output context that is set when an intent is
-- fulfilled.
--
-- /See:/ 'newOutputContext' smart constructor.
data OutputContext = OutputContext'
  { -- | The name of the context.
    name :: Core.Text,
    -- | The number of seconds that the context should be active after it is
    -- first sent in a @PostContent@ or @PostText@ response. You can set the
    -- value between 5 and 86,400 seconds (24 hours).
    timeToLiveInSeconds :: Core.Natural,
    -- | The number of conversation turns that the context should be active. A
    -- conversation turn is one @PostContent@ or @PostText@ request and the
    -- corresponding response from Amazon Lex.
    turnsToLive :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'timeToLiveInSeconds'
  Core.Natural ->
  -- | 'turnsToLive'
  Core.Natural ->
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
outputContext_name :: Lens.Lens' OutputContext Core.Text
outputContext_name = Lens.lens (\OutputContext' {name} -> name) (\s@OutputContext' {} a -> s {name = a} :: OutputContext)

-- | The number of seconds that the context should be active after it is
-- first sent in a @PostContent@ or @PostText@ response. You can set the
-- value between 5 and 86,400 seconds (24 hours).
outputContext_timeToLiveInSeconds :: Lens.Lens' OutputContext Core.Natural
outputContext_timeToLiveInSeconds = Lens.lens (\OutputContext' {timeToLiveInSeconds} -> timeToLiveInSeconds) (\s@OutputContext' {} a -> s {timeToLiveInSeconds = a} :: OutputContext)

-- | The number of conversation turns that the context should be active. A
-- conversation turn is one @PostContent@ or @PostText@ request and the
-- corresponding response from Amazon Lex.
outputContext_turnsToLive :: Lens.Lens' OutputContext Core.Natural
outputContext_turnsToLive = Lens.lens (\OutputContext' {turnsToLive} -> turnsToLive) (\s@OutputContext' {} a -> s {turnsToLive = a} :: OutputContext)

instance Core.FromJSON OutputContext where
  parseJSON =
    Core.withObject
      "OutputContext"
      ( \x ->
          OutputContext'
            Core.<$> (x Core..: "name")
            Core.<*> (x Core..: "timeToLiveInSeconds")
            Core.<*> (x Core..: "turnsToLive")
      )

instance Core.Hashable OutputContext

instance Core.NFData OutputContext

instance Core.ToJSON OutputContext where
  toJSON OutputContext' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just
              ("timeToLiveInSeconds" Core..= timeToLiveInSeconds),
            Core.Just ("turnsToLive" Core..= turnsToLive)
          ]
      )
