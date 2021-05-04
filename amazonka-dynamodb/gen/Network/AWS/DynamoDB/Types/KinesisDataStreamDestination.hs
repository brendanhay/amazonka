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
-- Module      : Network.AWS.DynamoDB.Types.KinesisDataStreamDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KinesisDataStreamDestination where

import Network.AWS.DynamoDB.Types.DestinationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Kinesis data stream destination.
--
-- /See:/ 'newKinesisDataStreamDestination' smart constructor.
data KinesisDataStreamDestination = KinesisDataStreamDestination'
  { -- | The human-readable string that corresponds to the replica status.
    destinationStatusDescription :: Prelude.Maybe Prelude.Text,
    -- | The ARN for a specific Kinesis data stream.
    streamArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of replication.
    destinationStatus :: Prelude.Maybe DestinationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisDataStreamDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationStatusDescription', 'kinesisDataStreamDestination_destinationStatusDescription' - The human-readable string that corresponds to the replica status.
--
-- 'streamArn', 'kinesisDataStreamDestination_streamArn' - The ARN for a specific Kinesis data stream.
--
-- 'destinationStatus', 'kinesisDataStreamDestination_destinationStatus' - The current status of replication.
newKinesisDataStreamDestination ::
  KinesisDataStreamDestination
newKinesisDataStreamDestination =
  KinesisDataStreamDestination'
    { destinationStatusDescription =
        Prelude.Nothing,
      streamArn = Prelude.Nothing,
      destinationStatus = Prelude.Nothing
    }

-- | The human-readable string that corresponds to the replica status.
kinesisDataStreamDestination_destinationStatusDescription :: Lens.Lens' KinesisDataStreamDestination (Prelude.Maybe Prelude.Text)
kinesisDataStreamDestination_destinationStatusDescription = Lens.lens (\KinesisDataStreamDestination' {destinationStatusDescription} -> destinationStatusDescription) (\s@KinesisDataStreamDestination' {} a -> s {destinationStatusDescription = a} :: KinesisDataStreamDestination)

-- | The ARN for a specific Kinesis data stream.
kinesisDataStreamDestination_streamArn :: Lens.Lens' KinesisDataStreamDestination (Prelude.Maybe Prelude.Text)
kinesisDataStreamDestination_streamArn = Lens.lens (\KinesisDataStreamDestination' {streamArn} -> streamArn) (\s@KinesisDataStreamDestination' {} a -> s {streamArn = a} :: KinesisDataStreamDestination)

-- | The current status of replication.
kinesisDataStreamDestination_destinationStatus :: Lens.Lens' KinesisDataStreamDestination (Prelude.Maybe DestinationStatus)
kinesisDataStreamDestination_destinationStatus = Lens.lens (\KinesisDataStreamDestination' {destinationStatus} -> destinationStatus) (\s@KinesisDataStreamDestination' {} a -> s {destinationStatus = a} :: KinesisDataStreamDestination)

instance
  Prelude.FromJSON
    KinesisDataStreamDestination
  where
  parseJSON =
    Prelude.withObject
      "KinesisDataStreamDestination"
      ( \x ->
          KinesisDataStreamDestination'
            Prelude.<$> (x Prelude..:? "DestinationStatusDescription")
            Prelude.<*> (x Prelude..:? "StreamArn")
            Prelude.<*> (x Prelude..:? "DestinationStatus")
      )

instance
  Prelude.Hashable
    KinesisDataStreamDestination

instance Prelude.NFData KinesisDataStreamDestination
