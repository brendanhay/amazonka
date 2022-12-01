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
-- Module      : Amazonka.KafkaConnect.Types.WorkerLogDelivery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.WorkerLogDelivery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KafkaConnect.Types.CloudWatchLogsLogDelivery
import Amazonka.KafkaConnect.Types.FirehoseLogDelivery
import Amazonka.KafkaConnect.Types.S3LogDelivery
import qualified Amazonka.Prelude as Prelude

-- | Workers can send worker logs to different destination types. This
-- configuration specifies the details of these destinations.
--
-- /See:/ 'newWorkerLogDelivery' smart constructor.
data WorkerLogDelivery = WorkerLogDelivery'
  { -- | Details about delivering logs to Amazon CloudWatch Logs.
    cloudWatchLogs :: Prelude.Maybe CloudWatchLogsLogDelivery,
    -- | Details about delivering logs to Amazon Kinesis Data Firehose.
    firehose :: Prelude.Maybe FirehoseLogDelivery,
    -- | Details about delivering logs to Amazon S3.
    s3 :: Prelude.Maybe S3LogDelivery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerLogDelivery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogs', 'workerLogDelivery_cloudWatchLogs' - Details about delivering logs to Amazon CloudWatch Logs.
--
-- 'firehose', 'workerLogDelivery_firehose' - Details about delivering logs to Amazon Kinesis Data Firehose.
--
-- 's3', 'workerLogDelivery_s3' - Details about delivering logs to Amazon S3.
newWorkerLogDelivery ::
  WorkerLogDelivery
newWorkerLogDelivery =
  WorkerLogDelivery'
    { cloudWatchLogs =
        Prelude.Nothing,
      firehose = Prelude.Nothing,
      s3 = Prelude.Nothing
    }

-- | Details about delivering logs to Amazon CloudWatch Logs.
workerLogDelivery_cloudWatchLogs :: Lens.Lens' WorkerLogDelivery (Prelude.Maybe CloudWatchLogsLogDelivery)
workerLogDelivery_cloudWatchLogs = Lens.lens (\WorkerLogDelivery' {cloudWatchLogs} -> cloudWatchLogs) (\s@WorkerLogDelivery' {} a -> s {cloudWatchLogs = a} :: WorkerLogDelivery)

-- | Details about delivering logs to Amazon Kinesis Data Firehose.
workerLogDelivery_firehose :: Lens.Lens' WorkerLogDelivery (Prelude.Maybe FirehoseLogDelivery)
workerLogDelivery_firehose = Lens.lens (\WorkerLogDelivery' {firehose} -> firehose) (\s@WorkerLogDelivery' {} a -> s {firehose = a} :: WorkerLogDelivery)

-- | Details about delivering logs to Amazon S3.
workerLogDelivery_s3 :: Lens.Lens' WorkerLogDelivery (Prelude.Maybe S3LogDelivery)
workerLogDelivery_s3 = Lens.lens (\WorkerLogDelivery' {s3} -> s3) (\s@WorkerLogDelivery' {} a -> s {s3 = a} :: WorkerLogDelivery)

instance Prelude.Hashable WorkerLogDelivery where
  hashWithSalt _salt WorkerLogDelivery' {..} =
    _salt `Prelude.hashWithSalt` cloudWatchLogs
      `Prelude.hashWithSalt` firehose
      `Prelude.hashWithSalt` s3

instance Prelude.NFData WorkerLogDelivery where
  rnf WorkerLogDelivery' {..} =
    Prelude.rnf cloudWatchLogs
      `Prelude.seq` Prelude.rnf firehose
      `Prelude.seq` Prelude.rnf s3

instance Core.ToJSON WorkerLogDelivery where
  toJSON WorkerLogDelivery' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("cloudWatchLogs" Core..=)
              Prelude.<$> cloudWatchLogs,
            ("firehose" Core..=) Prelude.<$> firehose,
            ("s3" Core..=) Prelude.<$> s3
          ]
      )
