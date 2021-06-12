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
-- Module      : Network.AWS.GameLift.Types.GameSessionQueueDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionQueueDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Fleet designated in a game session queue. Requests for new game sessions
-- in the queue are fulfilled by starting a new game session on any
-- destination that is configured for a queue.
--
-- -   CreateGameSessionQueue
--
-- -   DescribeGameSessionQueues
--
-- -   UpdateGameSessionQueue
--
-- -   DeleteGameSessionQueue
--
-- /See:/ 'newGameSessionQueueDestination' smart constructor.
data GameSessionQueueDestination = GameSessionQueueDestination'
  { -- | The Amazon Resource Name (ARN) that is assigned to fleet or fleet alias.
    -- ARNs, which include a fleet ID or alias ID and a Region name, provide a
    -- unique identifier across all Regions.
    destinationArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GameSessionQueueDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'gameSessionQueueDestination_destinationArn' - The Amazon Resource Name (ARN) that is assigned to fleet or fleet alias.
-- ARNs, which include a fleet ID or alias ID and a Region name, provide a
-- unique identifier across all Regions.
newGameSessionQueueDestination ::
  GameSessionQueueDestination
newGameSessionQueueDestination =
  GameSessionQueueDestination'
    { destinationArn =
        Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that is assigned to fleet or fleet alias.
-- ARNs, which include a fleet ID or alias ID and a Region name, provide a
-- unique identifier across all Regions.
gameSessionQueueDestination_destinationArn :: Lens.Lens' GameSessionQueueDestination (Core.Maybe Core.Text)
gameSessionQueueDestination_destinationArn = Lens.lens (\GameSessionQueueDestination' {destinationArn} -> destinationArn) (\s@GameSessionQueueDestination' {} a -> s {destinationArn = a} :: GameSessionQueueDestination)

instance Core.FromJSON GameSessionQueueDestination where
  parseJSON =
    Core.withObject
      "GameSessionQueueDestination"
      ( \x ->
          GameSessionQueueDestination'
            Core.<$> (x Core..:? "DestinationArn")
      )

instance Core.Hashable GameSessionQueueDestination

instance Core.NFData GameSessionQueueDestination

instance Core.ToJSON GameSessionQueueDestination where
  toJSON GameSessionQueueDestination' {..} =
    Core.object
      ( Core.catMaybes
          [("DestinationArn" Core..=) Core.<$> destinationArn]
      )
