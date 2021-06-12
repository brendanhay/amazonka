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
-- Module      : Network.AWS.S3.Types.StatsEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.StatsEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Stats

-- | Container for the Stats Event.
--
-- /See:/ 'newStatsEvent' smart constructor.
data StatsEvent = StatsEvent'
  { -- | The Stats event details.
    details :: Core.Maybe Stats
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StatsEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'statsEvent_details' - The Stats event details.
newStatsEvent ::
  StatsEvent
newStatsEvent = StatsEvent' {details = Core.Nothing}

-- | The Stats event details.
statsEvent_details :: Lens.Lens' StatsEvent (Core.Maybe Stats)
statsEvent_details = Lens.lens (\StatsEvent' {details} -> details) (\s@StatsEvent' {} a -> s {details = a} :: StatsEvent)

instance Core.FromXML StatsEvent where
  parseXML x =
    StatsEvent' Core.<$> (x Core..@? "Details")

instance Core.Hashable StatsEvent

instance Core.NFData StatsEvent
