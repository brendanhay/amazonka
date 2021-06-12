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

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.DestinationStatus
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newKinesisStreamingDestinationOutput' smart constructor.
data KinesisStreamingDestinationOutput = KinesisStreamingDestinationOutput'
  { -- | The name of the table being modified.
    tableName :: Core.Maybe Core.Text,
    -- | The ARN for the specific Kinesis data stream.
    streamArn :: Core.Maybe Core.Text,
    -- | The current status of the replication.
    destinationStatus :: Core.Maybe DestinationStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      streamArn = Core.Nothing,
      destinationStatus = Core.Nothing
    }

-- | The name of the table being modified.
kinesisStreamingDestinationOutput_tableName :: Lens.Lens' KinesisStreamingDestinationOutput (Core.Maybe Core.Text)
kinesisStreamingDestinationOutput_tableName = Lens.lens (\KinesisStreamingDestinationOutput' {tableName} -> tableName) (\s@KinesisStreamingDestinationOutput' {} a -> s {tableName = a} :: KinesisStreamingDestinationOutput)

-- | The ARN for the specific Kinesis data stream.
kinesisStreamingDestinationOutput_streamArn :: Lens.Lens' KinesisStreamingDestinationOutput (Core.Maybe Core.Text)
kinesisStreamingDestinationOutput_streamArn = Lens.lens (\KinesisStreamingDestinationOutput' {streamArn} -> streamArn) (\s@KinesisStreamingDestinationOutput' {} a -> s {streamArn = a} :: KinesisStreamingDestinationOutput)

-- | The current status of the replication.
kinesisStreamingDestinationOutput_destinationStatus :: Lens.Lens' KinesisStreamingDestinationOutput (Core.Maybe DestinationStatus)
kinesisStreamingDestinationOutput_destinationStatus = Lens.lens (\KinesisStreamingDestinationOutput' {destinationStatus} -> destinationStatus) (\s@KinesisStreamingDestinationOutput' {} a -> s {destinationStatus = a} :: KinesisStreamingDestinationOutput)

instance
  Core.FromJSON
    KinesisStreamingDestinationOutput
  where
  parseJSON =
    Core.withObject
      "KinesisStreamingDestinationOutput"
      ( \x ->
          KinesisStreamingDestinationOutput'
            Core.<$> (x Core..:? "TableName")
            Core.<*> (x Core..:? "StreamArn")
            Core.<*> (x Core..:? "DestinationStatus")
      )

instance
  Core.Hashable
    KinesisStreamingDestinationOutput

instance
  Core.NFData
    KinesisStreamingDestinationOutput
