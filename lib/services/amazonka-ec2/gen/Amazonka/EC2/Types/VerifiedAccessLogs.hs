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
-- Module      : Amazonka.EC2.Types.VerifiedAccessLogs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessLogs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessLogCloudWatchLogsDestination
import Amazonka.EC2.Types.VerifiedAccessLogKinesisDataFirehoseDestination
import Amazonka.EC2.Types.VerifiedAccessLogS3Destination
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for Verified Access logs.
--
-- /See:/ 'newVerifiedAccessLogs' smart constructor.
data VerifiedAccessLogs = VerifiedAccessLogs'
  { -- | CloudWatch Logs logging destination.
    cloudWatchLogs :: Prelude.Maybe VerifiedAccessLogCloudWatchLogsDestination,
    -- | Describes current setting for including trust data into the logs.
    includeTrustContext :: Prelude.Maybe Prelude.Bool,
    -- | Kinesis logging destination.
    kinesisDataFirehose :: Prelude.Maybe VerifiedAccessLogKinesisDataFirehoseDestination,
    -- | Describes current setting for the logging version.
    logVersion :: Prelude.Maybe Prelude.Text,
    -- | Amazon S3 logging options.
    s3 :: Prelude.Maybe VerifiedAccessLogS3Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogs', 'verifiedAccessLogs_cloudWatchLogs' - CloudWatch Logs logging destination.
--
-- 'includeTrustContext', 'verifiedAccessLogs_includeTrustContext' - Describes current setting for including trust data into the logs.
--
-- 'kinesisDataFirehose', 'verifiedAccessLogs_kinesisDataFirehose' - Kinesis logging destination.
--
-- 'logVersion', 'verifiedAccessLogs_logVersion' - Describes current setting for the logging version.
--
-- 's3', 'verifiedAccessLogs_s3' - Amazon S3 logging options.
newVerifiedAccessLogs ::
  VerifiedAccessLogs
newVerifiedAccessLogs =
  VerifiedAccessLogs'
    { cloudWatchLogs =
        Prelude.Nothing,
      includeTrustContext = Prelude.Nothing,
      kinesisDataFirehose = Prelude.Nothing,
      logVersion = Prelude.Nothing,
      s3 = Prelude.Nothing
    }

-- | CloudWatch Logs logging destination.
verifiedAccessLogs_cloudWatchLogs :: Lens.Lens' VerifiedAccessLogs (Prelude.Maybe VerifiedAccessLogCloudWatchLogsDestination)
verifiedAccessLogs_cloudWatchLogs = Lens.lens (\VerifiedAccessLogs' {cloudWatchLogs} -> cloudWatchLogs) (\s@VerifiedAccessLogs' {} a -> s {cloudWatchLogs = a} :: VerifiedAccessLogs)

-- | Describes current setting for including trust data into the logs.
verifiedAccessLogs_includeTrustContext :: Lens.Lens' VerifiedAccessLogs (Prelude.Maybe Prelude.Bool)
verifiedAccessLogs_includeTrustContext = Lens.lens (\VerifiedAccessLogs' {includeTrustContext} -> includeTrustContext) (\s@VerifiedAccessLogs' {} a -> s {includeTrustContext = a} :: VerifiedAccessLogs)

-- | Kinesis logging destination.
verifiedAccessLogs_kinesisDataFirehose :: Lens.Lens' VerifiedAccessLogs (Prelude.Maybe VerifiedAccessLogKinesisDataFirehoseDestination)
verifiedAccessLogs_kinesisDataFirehose = Lens.lens (\VerifiedAccessLogs' {kinesisDataFirehose} -> kinesisDataFirehose) (\s@VerifiedAccessLogs' {} a -> s {kinesisDataFirehose = a} :: VerifiedAccessLogs)

-- | Describes current setting for the logging version.
verifiedAccessLogs_logVersion :: Lens.Lens' VerifiedAccessLogs (Prelude.Maybe Prelude.Text)
verifiedAccessLogs_logVersion = Lens.lens (\VerifiedAccessLogs' {logVersion} -> logVersion) (\s@VerifiedAccessLogs' {} a -> s {logVersion = a} :: VerifiedAccessLogs)

-- | Amazon S3 logging options.
verifiedAccessLogs_s3 :: Lens.Lens' VerifiedAccessLogs (Prelude.Maybe VerifiedAccessLogS3Destination)
verifiedAccessLogs_s3 = Lens.lens (\VerifiedAccessLogs' {s3} -> s3) (\s@VerifiedAccessLogs' {} a -> s {s3 = a} :: VerifiedAccessLogs)

instance Data.FromXML VerifiedAccessLogs where
  parseXML x =
    VerifiedAccessLogs'
      Prelude.<$> (x Data..@? "cloudWatchLogs")
      Prelude.<*> (x Data..@? "includeTrustContext")
      Prelude.<*> (x Data..@? "kinesisDataFirehose")
      Prelude.<*> (x Data..@? "logVersion")
      Prelude.<*> (x Data..@? "s3")

instance Prelude.Hashable VerifiedAccessLogs where
  hashWithSalt _salt VerifiedAccessLogs' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogs
      `Prelude.hashWithSalt` includeTrustContext
      `Prelude.hashWithSalt` kinesisDataFirehose
      `Prelude.hashWithSalt` logVersion
      `Prelude.hashWithSalt` s3

instance Prelude.NFData VerifiedAccessLogs where
  rnf VerifiedAccessLogs' {..} =
    Prelude.rnf cloudWatchLogs
      `Prelude.seq` Prelude.rnf includeTrustContext
      `Prelude.seq` Prelude.rnf kinesisDataFirehose
      `Prelude.seq` Prelude.rnf logVersion
      `Prelude.seq` Prelude.rnf s3
