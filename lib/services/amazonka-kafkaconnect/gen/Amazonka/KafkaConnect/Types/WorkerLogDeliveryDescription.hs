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
-- Module      : Amazonka.KafkaConnect.Types.WorkerLogDeliveryDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.WorkerLogDeliveryDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.CloudWatchLogsLogDeliveryDescription
import Amazonka.KafkaConnect.Types.FirehoseLogDeliveryDescription
import Amazonka.KafkaConnect.Types.S3LogDeliveryDescription
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON WorkerLogDeliveryDescription where
  parseJSON =
    Data.withObject
      "WorkerLogDeliveryDescription"
      ( \x ->
          WorkerLogDeliveryDescription'
            Prelude.<$> (x Data..:? "cloudWatchLogs")
            Prelude.<*> (x Data..:? "firehose")
            Prelude.<*> (x Data..:? "s3")
      )

instance
  Prelude.Hashable
    WorkerLogDeliveryDescription
  where
  hashWithSalt _salt WorkerLogDeliveryDescription' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogs
      `Prelude.hashWithSalt` firehose
      `Prelude.hashWithSalt` s3

instance Prelude.NFData WorkerLogDeliveryDescription where
  rnf WorkerLogDeliveryDescription' {..} =
    Prelude.rnf cloudWatchLogs
      `Prelude.seq` Prelude.rnf firehose
      `Prelude.seq` Prelude.rnf s3
