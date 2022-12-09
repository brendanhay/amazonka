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
-- Module      : Amazonka.IVSChat.Types.DestinationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSChat.Types.DestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types.CloudWatchLogsDestinationConfiguration
import Amazonka.IVSChat.Types.FirehoseDestinationConfiguration
import Amazonka.IVSChat.Types.S3DestinationConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A complex type that describes a location where chat logs will be stored.
-- Each member represents the configuration of one log destination. For
-- logging, you define only one type of destination (for CloudWatch Logs,
-- Kinesis Firehose, or S3).
--
-- /See:/ 'newDestinationConfiguration' smart constructor.
data DestinationConfiguration = DestinationConfiguration'
  { -- | An Amazon CloudWatch Logs destination configuration where chat activity
    -- will be logged.
    cloudWatchLogs :: Prelude.Maybe CloudWatchLogsDestinationConfiguration,
    -- | An Amazon Kinesis Data Firehose destination configuration where chat
    -- activity will be logged.
    firehose :: Prelude.Maybe FirehoseDestinationConfiguration,
    -- | An Amazon S3 destination configuration where chat activity will be
    -- logged.
    s3 :: Prelude.Maybe S3DestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogs', 'destinationConfiguration_cloudWatchLogs' - An Amazon CloudWatch Logs destination configuration where chat activity
-- will be logged.
--
-- 'firehose', 'destinationConfiguration_firehose' - An Amazon Kinesis Data Firehose destination configuration where chat
-- activity will be logged.
--
-- 's3', 'destinationConfiguration_s3' - An Amazon S3 destination configuration where chat activity will be
-- logged.
newDestinationConfiguration ::
  DestinationConfiguration
newDestinationConfiguration =
  DestinationConfiguration'
    { cloudWatchLogs =
        Prelude.Nothing,
      firehose = Prelude.Nothing,
      s3 = Prelude.Nothing
    }

-- | An Amazon CloudWatch Logs destination configuration where chat activity
-- will be logged.
destinationConfiguration_cloudWatchLogs :: Lens.Lens' DestinationConfiguration (Prelude.Maybe CloudWatchLogsDestinationConfiguration)
destinationConfiguration_cloudWatchLogs = Lens.lens (\DestinationConfiguration' {cloudWatchLogs} -> cloudWatchLogs) (\s@DestinationConfiguration' {} a -> s {cloudWatchLogs = a} :: DestinationConfiguration)

-- | An Amazon Kinesis Data Firehose destination configuration where chat
-- activity will be logged.
destinationConfiguration_firehose :: Lens.Lens' DestinationConfiguration (Prelude.Maybe FirehoseDestinationConfiguration)
destinationConfiguration_firehose = Lens.lens (\DestinationConfiguration' {firehose} -> firehose) (\s@DestinationConfiguration' {} a -> s {firehose = a} :: DestinationConfiguration)

-- | An Amazon S3 destination configuration where chat activity will be
-- logged.
destinationConfiguration_s3 :: Lens.Lens' DestinationConfiguration (Prelude.Maybe S3DestinationConfiguration)
destinationConfiguration_s3 = Lens.lens (\DestinationConfiguration' {s3} -> s3) (\s@DestinationConfiguration' {} a -> s {s3 = a} :: DestinationConfiguration)

instance Data.FromJSON DestinationConfiguration where
  parseJSON =
    Data.withObject
      "DestinationConfiguration"
      ( \x ->
          DestinationConfiguration'
            Prelude.<$> (x Data..:? "cloudWatchLogs")
            Prelude.<*> (x Data..:? "firehose")
            Prelude.<*> (x Data..:? "s3")
      )

instance Prelude.Hashable DestinationConfiguration where
  hashWithSalt _salt DestinationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` cloudWatchLogs
      `Prelude.hashWithSalt` firehose
      `Prelude.hashWithSalt` s3

instance Prelude.NFData DestinationConfiguration where
  rnf DestinationConfiguration' {..} =
    Prelude.rnf cloudWatchLogs
      `Prelude.seq` Prelude.rnf firehose
      `Prelude.seq` Prelude.rnf s3

instance Data.ToJSON DestinationConfiguration where
  toJSON DestinationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cloudWatchLogs" Data..=)
              Prelude.<$> cloudWatchLogs,
            ("firehose" Data..=) Prelude.<$> firehose,
            ("s3" Data..=) Prelude.<$> s3
          ]
      )
