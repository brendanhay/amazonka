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
-- Module      : Network.AWS.KafkaConnect.Types.WorkerLogDeliveryDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KafkaConnect.Types.WorkerLogDeliveryDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.KafkaConnect.Types.CloudWatchLogsLogDeliveryDescription
import Network.AWS.KafkaConnect.Types.FirehoseLogDeliveryDescription
import Network.AWS.KafkaConnect.Types.S3LogDeliveryDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Workers can send worker logs to different destination types. This
-- configuration specifies the details of these destinations.
--
-- /See:/ 'newWorkerLogDeliveryDescription' smart constructor.
data WorkerLogDeliveryDescription = WorkerLogDeliveryDescription'
  { -- | Details about delivering logs to Amazon CloudWatch Logs.
    cloudWatchLogs :: Prelude.Maybe CloudWatchLogsLogDeliveryDescription,
    -- | Details about delivering logs to Amazon Kinesis Data Firehose.
    firehose :: Prelude.Maybe FirehoseLogDeliveryDescription,
    -- | Details about delivering logs to Amazon S3.
    s3 :: Prelude.Maybe S3LogDeliveryDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerLogDeliveryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogs', 'workerLogDeliveryDescription_cloudWatchLogs' - Details about delivering logs to Amazon CloudWatch Logs.
--
-- 'firehose', 'workerLogDeliveryDescription_firehose' - Details about delivering logs to Amazon Kinesis Data Firehose.
--
-- 's3', 'workerLogDeliveryDescription_s3' - Details about delivering logs to Amazon S3.
newWorkerLogDeliveryDescription ::
  WorkerLogDeliveryDescription
newWorkerLogDeliveryDescription =
  WorkerLogDeliveryDescription'
    { cloudWatchLogs =
        Prelude.Nothing,
      firehose = Prelude.Nothing,
      s3 = Prelude.Nothing
    }

-- | Details about delivering logs to Amazon CloudWatch Logs.
workerLogDeliveryDescription_cloudWatchLogs :: Lens.Lens' WorkerLogDeliveryDescription (Prelude.Maybe CloudWatchLogsLogDeliveryDescription)
workerLogDeliveryDescription_cloudWatchLogs = Lens.lens (\WorkerLogDeliveryDescription' {cloudWatchLogs} -> cloudWatchLogs) (\s@WorkerLogDeliveryDescription' {} a -> s {cloudWatchLogs = a} :: WorkerLogDeliveryDescription)

-- | Details about delivering logs to Amazon Kinesis Data Firehose.
workerLogDeliveryDescription_firehose :: Lens.Lens' WorkerLogDeliveryDescription (Prelude.Maybe FirehoseLogDeliveryDescription)
workerLogDeliveryDescription_firehose = Lens.lens (\WorkerLogDeliveryDescription' {firehose} -> firehose) (\s@WorkerLogDeliveryDescription' {} a -> s {firehose = a} :: WorkerLogDeliveryDescription)

-- | Details about delivering logs to Amazon S3.
workerLogDeliveryDescription_s3 :: Lens.Lens' WorkerLogDeliveryDescription (Prelude.Maybe S3LogDeliveryDescription)
workerLogDeliveryDescription_s3 = Lens.lens (\WorkerLogDeliveryDescription' {s3} -> s3) (\s@WorkerLogDeliveryDescription' {} a -> s {s3 = a} :: WorkerLogDeliveryDescription)

instance Core.FromJSON WorkerLogDeliveryDescription where
  parseJSON =
    Core.withObject
      "WorkerLogDeliveryDescription"
      ( \x ->
          WorkerLogDeliveryDescription'
            Prelude.<$> (x Core..:? "cloudWatchLogs")
            Prelude.<*> (x Core..:? "firehose")
            Prelude.<*> (x Core..:? "s3")
      )

instance
  Prelude.Hashable
    WorkerLogDeliveryDescription

instance Prelude.NFData WorkerLogDeliveryDescription
