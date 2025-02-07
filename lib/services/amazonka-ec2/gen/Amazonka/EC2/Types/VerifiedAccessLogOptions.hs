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
-- Module      : Amazonka.EC2.Types.VerifiedAccessLogOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessLogOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessLogCloudWatchLogsDestinationOptions
import Amazonka.EC2.Types.VerifiedAccessLogKinesisDataFirehoseDestinationOptions
import Amazonka.EC2.Types.VerifiedAccessLogS3DestinationOptions
import qualified Amazonka.Prelude as Prelude

-- | Describes the destinations for Verified Access logs.
--
-- /See:/ 'newVerifiedAccessLogOptions' smart constructor.
data VerifiedAccessLogOptions = VerifiedAccessLogOptions'
  { -- | Sends Verified Access logs to CloudWatch Logs.
    cloudWatchLogs :: Prelude.Maybe VerifiedAccessLogCloudWatchLogsDestinationOptions,
    -- | Sends Verified Access logs to Kinesis.
    kinesisDataFirehose :: Prelude.Maybe VerifiedAccessLogKinesisDataFirehoseDestinationOptions,
    -- | Sends Verified Access logs to Amazon S3.
    s3 :: Prelude.Maybe VerifiedAccessLogS3DestinationOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessLogOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogs', 'verifiedAccessLogOptions_cloudWatchLogs' - Sends Verified Access logs to CloudWatch Logs.
--
-- 'kinesisDataFirehose', 'verifiedAccessLogOptions_kinesisDataFirehose' - Sends Verified Access logs to Kinesis.
--
-- 's3', 'verifiedAccessLogOptions_s3' - Sends Verified Access logs to Amazon S3.
newVerifiedAccessLogOptions ::
  VerifiedAccessLogOptions
newVerifiedAccessLogOptions =
  VerifiedAccessLogOptions'
    { cloudWatchLogs =
        Prelude.Nothing,
      kinesisDataFirehose = Prelude.Nothing,
      s3 = Prelude.Nothing
    }

-- | Sends Verified Access logs to CloudWatch Logs.
verifiedAccessLogOptions_cloudWatchLogs :: Lens.Lens' VerifiedAccessLogOptions (Prelude.Maybe VerifiedAccessLogCloudWatchLogsDestinationOptions)
verifiedAccessLogOptions_cloudWatchLogs = Lens.lens (\VerifiedAccessLogOptions' {cloudWatchLogs} -> cloudWatchLogs) (\s@VerifiedAccessLogOptions' {} a -> s {cloudWatchLogs = a} :: VerifiedAccessLogOptions)

-- | Sends Verified Access logs to Kinesis.
verifiedAccessLogOptions_kinesisDataFirehose :: Lens.Lens' VerifiedAccessLogOptions (Prelude.Maybe VerifiedAccessLogKinesisDataFirehoseDestinationOptions)
verifiedAccessLogOptions_kinesisDataFirehose = Lens.lens (\VerifiedAccessLogOptions' {kinesisDataFirehose} -> kinesisDataFirehose) (\s@VerifiedAccessLogOptions' {} a -> s {kinesisDataFirehose = a} :: VerifiedAccessLogOptions)

-- | Sends Verified Access logs to Amazon S3.
verifiedAccessLogOptions_s3 :: Lens.Lens' VerifiedAccessLogOptions (Prelude.Maybe VerifiedAccessLogS3DestinationOptions)
verifiedAccessLogOptions_s3 = Lens.lens (\VerifiedAccessLogOptions' {s3} -> s3) (\s@VerifiedAccessLogOptions' {} a -> s {s3 = a} :: VerifiedAccessLogOptions)

instance Prelude.Hashable VerifiedAccessLogOptions where
  hashWithSalt _salt VerifiedAccessLogOptions' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogs
      `Prelude.hashWithSalt` kinesisDataFirehose
      `Prelude.hashWithSalt` s3

instance Prelude.NFData VerifiedAccessLogOptions where
  rnf VerifiedAccessLogOptions' {..} =
    Prelude.rnf cloudWatchLogs `Prelude.seq`
      Prelude.rnf kinesisDataFirehose `Prelude.seq`
        Prelude.rnf s3

instance Data.ToQuery VerifiedAccessLogOptions where
  toQuery VerifiedAccessLogOptions' {..} =
    Prelude.mconcat
      [ "CloudWatchLogs" Data.=: cloudWatchLogs,
        "KinesisDataFirehose" Data.=: kinesisDataFirehose,
        "S3" Data.=: s3
      ]
