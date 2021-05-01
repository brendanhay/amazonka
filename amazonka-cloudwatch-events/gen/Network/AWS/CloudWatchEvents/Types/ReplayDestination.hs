{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A @ReplayDestination@ object that contains details about a replay.
--
-- /See:/ 'newReplayDestination' smart constructor.
data ReplayDestination = ReplayDestination'
  { -- | A list of ARNs for rules to replay events to.
    filterArns :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the event bus to replay event to. You can replay events only
    -- to the event bus specified to create the archive.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ReplayDestination
newReplayDestination pArn_ =
  ReplayDestination'
    { filterArns = Prelude.Nothing,
      arn = pArn_
    }

-- | A list of ARNs for rules to replay events to.
replayDestination_filterArns :: Lens.Lens' ReplayDestination (Prelude.Maybe [Prelude.Text])
replayDestination_filterArns = Lens.lens (\ReplayDestination' {filterArns} -> filterArns) (\s@ReplayDestination' {} a -> s {filterArns = a} :: ReplayDestination) Prelude.. Lens.mapping Prelude._Coerce

-- | The ARN of the event bus to replay event to. You can replay events only
-- to the event bus specified to create the archive.
replayDestination_arn :: Lens.Lens' ReplayDestination Prelude.Text
replayDestination_arn = Lens.lens (\ReplayDestination' {arn} -> arn) (\s@ReplayDestination' {} a -> s {arn = a} :: ReplayDestination)

instance Prelude.FromJSON ReplayDestination where
  parseJSON =
    Prelude.withObject
      "ReplayDestination"
      ( \x ->
          ReplayDestination'
            Prelude.<$> ( x Prelude..:? "FilterArns"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "Arn")
      )

instance Prelude.Hashable ReplayDestination

instance Prelude.NFData ReplayDestination

instance Prelude.ToJSON ReplayDestination where
  toJSON ReplayDestination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FilterArns" Prelude..=) Prelude.<$> filterArns,
            Prelude.Just ("Arn" Prelude..= arn)
          ]
      )
