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
-- Module      : Network.AWS.DynamoDB.Types.KinesisStreamingDestinationOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KinesisStreamingDestinationOutput where

import Network.AWS.DynamoDB.Types.DestinationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newKinesisStreamingDestinationOutput' smart constructor.
data KinesisStreamingDestinationOutput = KinesisStreamingDestinationOutput'
  { -- | The name of the table being modified.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the specific Kinesis data stream.
    streamArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the replication.
    destinationStatus :: Prelude.Maybe DestinationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamingDestinationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'kinesisStreamingDestinationOutput_tableName' - The name of the table being modified.
--
-- 'streamArn', 'kinesisStreamingDestinationOutput_streamArn' - The ARN for the specific Kinesis data stream.
--
-- 'destinationStatus', 'kinesisStreamingDestinationOutput_destinationStatus' - The current status of the replication.
newKinesisStreamingDestinationOutput ::
  KinesisStreamingDestinationOutput
newKinesisStreamingDestinationOutput =
  KinesisStreamingDestinationOutput'
    { tableName =
        Prelude.Nothing,
      streamArn = Prelude.Nothing,
      destinationStatus = Prelude.Nothing
    }

-- | The name of the table being modified.
kinesisStreamingDestinationOutput_tableName :: Lens.Lens' KinesisStreamingDestinationOutput (Prelude.Maybe Prelude.Text)
kinesisStreamingDestinationOutput_tableName = Lens.lens (\KinesisStreamingDestinationOutput' {tableName} -> tableName) (\s@KinesisStreamingDestinationOutput' {} a -> s {tableName = a} :: KinesisStreamingDestinationOutput)

-- | The ARN for the specific Kinesis data stream.
kinesisStreamingDestinationOutput_streamArn :: Lens.Lens' KinesisStreamingDestinationOutput (Prelude.Maybe Prelude.Text)
kinesisStreamingDestinationOutput_streamArn = Lens.lens (\KinesisStreamingDestinationOutput' {streamArn} -> streamArn) (\s@KinesisStreamingDestinationOutput' {} a -> s {streamArn = a} :: KinesisStreamingDestinationOutput)

-- | The current status of the replication.
kinesisStreamingDestinationOutput_destinationStatus :: Lens.Lens' KinesisStreamingDestinationOutput (Prelude.Maybe DestinationStatus)
kinesisStreamingDestinationOutput_destinationStatus = Lens.lens (\KinesisStreamingDestinationOutput' {destinationStatus} -> destinationStatus) (\s@KinesisStreamingDestinationOutput' {} a -> s {destinationStatus = a} :: KinesisStreamingDestinationOutput)

instance
  Prelude.FromJSON
    KinesisStreamingDestinationOutput
  where
  parseJSON =
    Prelude.withObject
      "KinesisStreamingDestinationOutput"
      ( \x ->
          KinesisStreamingDestinationOutput'
            Prelude.<$> (x Prelude..:? "TableName")
            Prelude.<*> (x Prelude..:? "StreamArn")
            Prelude.<*> (x Prelude..:? "DestinationStatus")
      )

instance
  Prelude.Hashable
    KinesisStreamingDestinationOutput

instance
  Prelude.NFData
    KinesisStreamingDestinationOutput
