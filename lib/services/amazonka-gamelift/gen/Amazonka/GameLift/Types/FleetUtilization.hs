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
-- Module      : Amazonka.GameLift.Types.FleetUtilization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.FleetUtilization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Current resource utilization statistics in a specified fleet or
-- location. The location value might refer to a fleet\'s remote location
-- or its home Region.
--
-- __Related actions__
--
-- /See:/ 'newFleetUtilization' smart constructor.
data FleetUtilization = FleetUtilization'
  { -- | The number of active game sessions that are currently being hosted
    -- across all instances in the fleet location.
    activeGameSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of server processes in @ACTIVE@ status that are currently
    -- running across all instances in the fleet location.
    activeServerProcessCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of active player sessions that are currently being hosted
    -- across all instances in the fleet location.
    currentPlayerSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet associated with the location.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The fleet location for the fleet utilization information, expressed as
    -- an Amazon Web Services Region code, such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of players allowed across all game sessions that are
    -- currently being hosted across all instances in the fleet location.
    maximumPlayerSessionCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeGameSessionCount', 'fleetUtilization_activeGameSessionCount' - The number of active game sessions that are currently being hosted
-- across all instances in the fleet location.
--
-- 'activeServerProcessCount', 'fleetUtilization_activeServerProcessCount' - The number of server processes in @ACTIVE@ status that are currently
-- running across all instances in the fleet location.
--
-- 'currentPlayerSessionCount', 'fleetUtilization_currentPlayerSessionCount' - The number of active player sessions that are currently being hosted
-- across all instances in the fleet location.
--
-- 'fleetArn', 'fleetUtilization_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'fleetId', 'fleetUtilization_fleetId' - A unique identifier for the fleet associated with the location.
--
-- 'location', 'fleetUtilization_location' - The fleet location for the fleet utilization information, expressed as
-- an Amazon Web Services Region code, such as @us-west-2@.
--
-- 'maximumPlayerSessionCount', 'fleetUtilization_maximumPlayerSessionCount' - The maximum number of players allowed across all game sessions that are
-- currently being hosted across all instances in the fleet location.
newFleetUtilization ::
  FleetUtilization
newFleetUtilization =
  FleetUtilization'
    { activeGameSessionCount =
        Prelude.Nothing,
      activeServerProcessCount = Prelude.Nothing,
      currentPlayerSessionCount = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      location = Prelude.Nothing,
      maximumPlayerSessionCount = Prelude.Nothing
    }

-- | The number of active game sessions that are currently being hosted
-- across all instances in the fleet location.
fleetUtilization_activeGameSessionCount :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Natural)
fleetUtilization_activeGameSessionCount = Lens.lens (\FleetUtilization' {activeGameSessionCount} -> activeGameSessionCount) (\s@FleetUtilization' {} a -> s {activeGameSessionCount = a} :: FleetUtilization)

-- | The number of server processes in @ACTIVE@ status that are currently
-- running across all instances in the fleet location.
fleetUtilization_activeServerProcessCount :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Natural)
fleetUtilization_activeServerProcessCount = Lens.lens (\FleetUtilization' {activeServerProcessCount} -> activeServerProcessCount) (\s@FleetUtilization' {} a -> s {activeServerProcessCount = a} :: FleetUtilization)

-- | The number of active player sessions that are currently being hosted
-- across all instances in the fleet location.
fleetUtilization_currentPlayerSessionCount :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Natural)
fleetUtilization_currentPlayerSessionCount = Lens.lens (\FleetUtilization' {currentPlayerSessionCount} -> currentPlayerSessionCount) (\s@FleetUtilization' {} a -> s {currentPlayerSessionCount = a} :: FleetUtilization)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
fleetUtilization_fleetArn :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Text)
fleetUtilization_fleetArn = Lens.lens (\FleetUtilization' {fleetArn} -> fleetArn) (\s@FleetUtilization' {} a -> s {fleetArn = a} :: FleetUtilization)

-- | A unique identifier for the fleet associated with the location.
fleetUtilization_fleetId :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Text)
fleetUtilization_fleetId = Lens.lens (\FleetUtilization' {fleetId} -> fleetId) (\s@FleetUtilization' {} a -> s {fleetId = a} :: FleetUtilization)

-- | The fleet location for the fleet utilization information, expressed as
-- an Amazon Web Services Region code, such as @us-west-2@.
fleetUtilization_location :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Text)
fleetUtilization_location = Lens.lens (\FleetUtilization' {location} -> location) (\s@FleetUtilization' {} a -> s {location = a} :: FleetUtilization)

-- | The maximum number of players allowed across all game sessions that are
-- currently being hosted across all instances in the fleet location.
fleetUtilization_maximumPlayerSessionCount :: Lens.Lens' FleetUtilization (Prelude.Maybe Prelude.Natural)
fleetUtilization_maximumPlayerSessionCount = Lens.lens (\FleetUtilization' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@FleetUtilization' {} a -> s {maximumPlayerSessionCount = a} :: FleetUtilization)

instance Data.FromJSON FleetUtilization where
  parseJSON =
    Data.withObject
      "FleetUtilization"
      ( \x ->
          FleetUtilization'
            Prelude.<$> (x Data..:? "ActiveGameSessionCount")
            Prelude.<*> (x Data..:? "ActiveServerProcessCount")
            Prelude.<*> (x Data..:? "CurrentPlayerSessionCount")
            Prelude.<*> (x Data..:? "FleetArn")
            Prelude.<*> (x Data..:? "FleetId")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "MaximumPlayerSessionCount")
      )

instance Prelude.Hashable FleetUtilization where
  hashWithSalt _salt FleetUtilization' {..} =
    _salt `Prelude.hashWithSalt` activeGameSessionCount
      `Prelude.hashWithSalt` activeServerProcessCount
      `Prelude.hashWithSalt` currentPlayerSessionCount
      `Prelude.hashWithSalt` fleetArn
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` maximumPlayerSessionCount

instance Prelude.NFData FleetUtilization where
  rnf FleetUtilization' {..} =
    Prelude.rnf activeGameSessionCount
      `Prelude.seq` Prelude.rnf activeServerProcessCount
      `Prelude.seq` Prelude.rnf currentPlayerSessionCount
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf maximumPlayerSessionCount
