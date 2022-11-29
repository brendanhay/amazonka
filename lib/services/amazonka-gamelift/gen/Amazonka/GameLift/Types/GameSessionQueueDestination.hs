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
-- Module      : Amazonka.GameLift.Types.GameSessionQueueDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameSessionQueueDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A fleet or alias designated in a game session queue. Queues fulfill
-- requests for new game sessions by placing a new game session on any of
-- the queue\'s destinations.
--
-- Destinations are part of a GameSessionQueue.
--
-- /See:/ 'newGameSessionQueueDestination' smart constructor.
data GameSessionQueueDestination = GameSessionQueueDestination'
  { -- | The Amazon Resource Name (ARN) that is assigned to fleet or fleet alias.
    -- ARNs, which include a fleet ID or alias ID and a Region name, provide a
    -- unique identifier across all Regions.
    destinationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that is assigned to fleet or fleet alias.
-- ARNs, which include a fleet ID or alias ID and a Region name, provide a
-- unique identifier across all Regions.
gameSessionQueueDestination_destinationArn :: Lens.Lens' GameSessionQueueDestination (Prelude.Maybe Prelude.Text)
gameSessionQueueDestination_destinationArn = Lens.lens (\GameSessionQueueDestination' {destinationArn} -> destinationArn) (\s@GameSessionQueueDestination' {} a -> s {destinationArn = a} :: GameSessionQueueDestination)

instance Core.FromJSON GameSessionQueueDestination where
  parseJSON =
    Core.withObject
      "GameSessionQueueDestination"
      ( \x ->
          GameSessionQueueDestination'
            Prelude.<$> (x Core..:? "DestinationArn")
      )

instance Prelude.Hashable GameSessionQueueDestination where
  hashWithSalt _salt GameSessionQueueDestination' {..} =
    _salt `Prelude.hashWithSalt` destinationArn

instance Prelude.NFData GameSessionQueueDestination where
  rnf GameSessionQueueDestination' {..} =
    Prelude.rnf destinationArn

instance Core.ToJSON GameSessionQueueDestination where
  toJSON GameSessionQueueDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DestinationArn" Core..=)
              Prelude.<$> destinationArn
          ]
      )
