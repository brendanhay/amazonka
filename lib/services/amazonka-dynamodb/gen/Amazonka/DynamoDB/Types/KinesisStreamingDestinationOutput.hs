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
-- Module      : Amazonka.DynamoDB.Types.KinesisStreamingDestinationOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.KinesisStreamingDestinationOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.DestinationStatus
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newKinesisStreamingDestinationOutput' smart constructor.
data KinesisStreamingDestinationOutput = KinesisStreamingDestinationOutput'
  { -- | The current status of the replication.
    destinationStatus :: Prelude.Maybe DestinationStatus,
    -- | The ARN for the specific Kinesis data stream.
    streamArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the table being modified.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamingDestinationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationStatus', 'kinesisStreamingDestinationOutput_destinationStatus' - The current status of the replication.
--
-- 'streamArn', 'kinesisStreamingDestinationOutput_streamArn' - The ARN for the specific Kinesis data stream.
--
-- 'tableName', 'kinesisStreamingDestinationOutput_tableName' - The name of the table being modified.
newKinesisStreamingDestinationOutput ::
  KinesisStreamingDestinationOutput
newKinesisStreamingDestinationOutput =
  KinesisStreamingDestinationOutput'
    { destinationStatus =
        Prelude.Nothing,
      streamArn = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | The current status of the replication.
kinesisStreamingDestinationOutput_destinationStatus :: Lens.Lens' KinesisStreamingDestinationOutput (Prelude.Maybe DestinationStatus)
kinesisStreamingDestinationOutput_destinationStatus = Lens.lens (\KinesisStreamingDestinationOutput' {destinationStatus} -> destinationStatus) (\s@KinesisStreamingDestinationOutput' {} a -> s {destinationStatus = a} :: KinesisStreamingDestinationOutput)

-- | The ARN for the specific Kinesis data stream.
kinesisStreamingDestinationOutput_streamArn :: Lens.Lens' KinesisStreamingDestinationOutput (Prelude.Maybe Prelude.Text)
kinesisStreamingDestinationOutput_streamArn = Lens.lens (\KinesisStreamingDestinationOutput' {streamArn} -> streamArn) (\s@KinesisStreamingDestinationOutput' {} a -> s {streamArn = a} :: KinesisStreamingDestinationOutput)

-- | The name of the table being modified.
kinesisStreamingDestinationOutput_tableName :: Lens.Lens' KinesisStreamingDestinationOutput (Prelude.Maybe Prelude.Text)
kinesisStreamingDestinationOutput_tableName = Lens.lens (\KinesisStreamingDestinationOutput' {tableName} -> tableName) (\s@KinesisStreamingDestinationOutput' {} a -> s {tableName = a} :: KinesisStreamingDestinationOutput)

instance
  Data.FromJSON
    KinesisStreamingDestinationOutput
  where
  parseJSON =
    Data.withObject
      "KinesisStreamingDestinationOutput"
      ( \x ->
          KinesisStreamingDestinationOutput'
            Prelude.<$> (x Data..:? "DestinationStatus")
            Prelude.<*> (x Data..:? "StreamArn")
            Prelude.<*> (x Data..:? "TableName")
      )

instance
  Prelude.Hashable
    KinesisStreamingDestinationOutput
  where
  hashWithSalt
    _salt
    KinesisStreamingDestinationOutput' {..} =
      _salt `Prelude.hashWithSalt` destinationStatus
        `Prelude.hashWithSalt` streamArn
        `Prelude.hashWithSalt` tableName

instance
  Prelude.NFData
    KinesisStreamingDestinationOutput
  where
  rnf KinesisStreamingDestinationOutput' {..} =
    Prelude.rnf destinationStatus
      `Prelude.seq` Prelude.rnf streamArn
      `Prelude.seq` Prelude.rnf tableName
