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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.KinesisDataStreamDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.DestinationStatus
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Describes a Kinesis data stream destination.
--
-- /See:/ 'newKinesisDataStreamDestination' smart constructor.
data KinesisDataStreamDestination = KinesisDataStreamDestination'
  { -- | The current status of replication.
    destinationStatus :: Prelude.Maybe DestinationStatus,
    -- | The human-readable string that corresponds to the replica status.
    destinationStatusDescription :: Prelude.Maybe Prelude.Text,
    -- | The ARN for a specific Kinesis data stream.
    streamArn :: Prelude.Maybe Prelude.Text
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
-- 'destinationStatusDescription', 'kinesisDataStreamDestination_destinationStatusDescription' - The human-readable string that corresponds to the replica status.
--
-- 'streamArn', 'kinesisDataStreamDestination_streamArn' - The ARN for a specific Kinesis data stream.
newKinesisDataStreamDestination ::
  KinesisDataStreamDestination
newKinesisDataStreamDestination =
  KinesisDataStreamDestination'
    { destinationStatus =
        Prelude.Nothing,
      destinationStatusDescription =
        Prelude.Nothing,
      streamArn = Prelude.Nothing
    }

-- | The current status of replication.
kinesisDataStreamDestination_destinationStatus :: Lens.Lens' KinesisDataStreamDestination (Prelude.Maybe DestinationStatus)
kinesisDataStreamDestination_destinationStatus = Lens.lens (\KinesisDataStreamDestination' {destinationStatus} -> destinationStatus) (\s@KinesisDataStreamDestination' {} a -> s {destinationStatus = a} :: KinesisDataStreamDestination)

-- | The human-readable string that corresponds to the replica status.
kinesisDataStreamDestination_destinationStatusDescription :: Lens.Lens' KinesisDataStreamDestination (Prelude.Maybe Prelude.Text)
kinesisDataStreamDestination_destinationStatusDescription = Lens.lens (\KinesisDataStreamDestination' {destinationStatusDescription} -> destinationStatusDescription) (\s@KinesisDataStreamDestination' {} a -> s {destinationStatusDescription = a} :: KinesisDataStreamDestination)

-- | The ARN for a specific Kinesis data stream.
kinesisDataStreamDestination_streamArn :: Lens.Lens' KinesisDataStreamDestination (Prelude.Maybe Prelude.Text)
kinesisDataStreamDestination_streamArn = Lens.lens (\KinesisDataStreamDestination' {streamArn} -> streamArn) (\s@KinesisDataStreamDestination' {} a -> s {streamArn = a} :: KinesisDataStreamDestination)

instance Data.FromJSON KinesisDataStreamDestination where
  parseJSON =
    Data.withObject
      "KinesisDataStreamDestination"
      ( \x ->
          KinesisDataStreamDestination'
            Prelude.<$> (x Data..:? "DestinationStatus")
            Prelude.<*> (x Data..:? "DestinationStatusDescription")
            Prelude.<*> (x Data..:? "StreamArn")
      )

instance
  Prelude.Hashable
    KinesisDataStreamDestination
  where
  hashWithSalt _salt KinesisDataStreamDestination' {..} =
    _salt
      `Prelude.hashWithSalt` destinationStatus
      `Prelude.hashWithSalt` destinationStatusDescription
      `Prelude.hashWithSalt` streamArn

instance Prelude.NFData KinesisDataStreamDestination where
  rnf KinesisDataStreamDestination' {..} =
    Prelude.rnf destinationStatus
      `Prelude.seq` Prelude.rnf destinationStatusDescription
      `Prelude.seq` Prelude.rnf streamArn
