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
-- Module      : Network.AWS.CloudWatchEvents.Types.ReplayDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ReplayDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A @ReplayDestination@ object that contains details about a replay.
--
-- /See:/ 'newReplayDestination' smart constructor.
data ReplayDestination = ReplayDestination'
  { -- | A list of ARNs for rules to replay events to.
    filterArns :: Core.Maybe [Core.Text],
    -- | The ARN of the event bus to replay event to. You can replay events only
    -- to the event bus specified to create the archive.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplayDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterArns', 'replayDestination_filterArns' - A list of ARNs for rules to replay events to.
--
-- 'arn', 'replayDestination_arn' - The ARN of the event bus to replay event to. You can replay events only
-- to the event bus specified to create the archive.
newReplayDestination ::
  -- | 'arn'
  Core.Text ->
  ReplayDestination
newReplayDestination pArn_ =
  ReplayDestination'
    { filterArns = Core.Nothing,
      arn = pArn_
    }

-- | A list of ARNs for rules to replay events to.
replayDestination_filterArns :: Lens.Lens' ReplayDestination (Core.Maybe [Core.Text])
replayDestination_filterArns = Lens.lens (\ReplayDestination' {filterArns} -> filterArns) (\s@ReplayDestination' {} a -> s {filterArns = a} :: ReplayDestination) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the event bus to replay event to. You can replay events only
-- to the event bus specified to create the archive.
replayDestination_arn :: Lens.Lens' ReplayDestination Core.Text
replayDestination_arn = Lens.lens (\ReplayDestination' {arn} -> arn) (\s@ReplayDestination' {} a -> s {arn = a} :: ReplayDestination)

instance Core.FromJSON ReplayDestination where
  parseJSON =
    Core.withObject
      "ReplayDestination"
      ( \x ->
          ReplayDestination'
            Core.<$> (x Core..:? "FilterArns" Core..!= Core.mempty)
            Core.<*> (x Core..: "Arn")
      )

instance Core.Hashable ReplayDestination

instance Core.NFData ReplayDestination

instance Core.ToJSON ReplayDestination where
  toJSON ReplayDestination' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FilterArns" Core..=) Core.<$> filterArns,
            Core.Just ("Arn" Core..= arn)
          ]
      )
