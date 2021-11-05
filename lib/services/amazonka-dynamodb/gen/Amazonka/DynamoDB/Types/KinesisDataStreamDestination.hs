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
-- Module      : Amazonka.DynamoDB.Types.KinesisDataStreamDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.KinesisDataStreamDestination where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.DestinationStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a Kinesis data stream destination.
--
-- /See:/ 'newKinesisDataStreamDestination' smart constructor.
data KinesisDataStreamDestination = KinesisDataStreamDestination'
  { -- | The current status of replication.
    destinationStatus :: Prelude.Maybe DestinationStatus,
    -- | The ARN for a specific Kinesis data stream.
    streamArn :: Prelude.Maybe Prelude.Text,
    -- | The human-readable string that corresponds to the replica status.
    destinationStatusDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisDataStreamDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationStatus', 'kinesisDataStreamDestination_destinationStatus' - The current status of replication.
--
-- 'streamArn', 'kinesisDataStreamDestination_streamArn' - The ARN for a specific Kinesis data stream.
--
-- 'destinationStatusDescription', 'kinesisDataStreamDestination_destinationStatusDescription' - The human-readable string that corresponds to the replica status.
newKinesisDataStreamDestination ::
  KinesisDataStreamDestination
newKinesisDataStreamDestination =
  KinesisDataStreamDestination'
    { destinationStatus =
        Prelude.Nothing,
      streamArn = Prelude.Nothing,
      destinationStatusDescription =
        Prelude.Nothing
    }

-- | The current status of replication.
kinesisDataStreamDestination_destinationStatus :: Lens.Lens' KinesisDataStreamDestination (Prelude.Maybe DestinationStatus)
kinesisDataStreamDestination_destinationStatus = Lens.lens (\KinesisDataStreamDestination' {destinationStatus} -> destinationStatus) (\s@KinesisDataStreamDestination' {} a -> s {destinationStatus = a} :: KinesisDataStreamDestination)

-- | The ARN for a specific Kinesis data stream.
kinesisDataStreamDestination_streamArn :: Lens.Lens' KinesisDataStreamDestination (Prelude.Maybe Prelude.Text)
kinesisDataStreamDestination_streamArn = Lens.lens (\KinesisDataStreamDestination' {streamArn} -> streamArn) (\s@KinesisDataStreamDestination' {} a -> s {streamArn = a} :: KinesisDataStreamDestination)

-- | The human-readable string that corresponds to the replica status.
kinesisDataStreamDestination_destinationStatusDescription :: Lens.Lens' KinesisDataStreamDestination (Prelude.Maybe Prelude.Text)
kinesisDataStreamDestination_destinationStatusDescription = Lens.lens (\KinesisDataStreamDestination' {destinationStatusDescription} -> destinationStatusDescription) (\s@KinesisDataStreamDestination' {} a -> s {destinationStatusDescription = a} :: KinesisDataStreamDestination)

instance Core.FromJSON KinesisDataStreamDestination where
  parseJSON =
    Core.withObject
      "KinesisDataStreamDestination"
      ( \x ->
          KinesisDataStreamDestination'
            Prelude.<$> (x Core..:? "DestinationStatus")
            Prelude.<*> (x Core..:? "StreamArn")
            Prelude.<*> (x Core..:? "DestinationStatusDescription")
      )

instance
  Prelude.Hashable
    KinesisDataStreamDestination

instance Prelude.NFData KinesisDataStreamDestination
