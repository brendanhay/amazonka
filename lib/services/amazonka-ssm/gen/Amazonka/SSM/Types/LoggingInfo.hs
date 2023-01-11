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
-- Module      : Amazonka.SSM.Types.LoggingInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.LoggingInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an Amazon Simple Storage Service (Amazon S3) bucket to
-- write managed node-level logs to.
--
-- @LoggingInfo@ has been deprecated. To specify an Amazon Simple Storage
-- Service (Amazon S3) bucket to contain logs, instead use the
-- @OutputS3BucketName@ and @OutputS3KeyPrefix@ options in the
-- @TaskInvocationParameters@ structure. For information about how Amazon
-- Web Services Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- /See:/ 'newLoggingInfo' smart constructor.
data LoggingInfo = LoggingInfo'
  { -- | (Optional) The S3 bucket subfolder.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of an S3 bucket where execution logs are stored.
    s3BucketName :: Prelude.Text,
    -- | The Amazon Web Services Region where the S3 bucket is located.
    s3Region :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KeyPrefix', 'loggingInfo_s3KeyPrefix' - (Optional) The S3 bucket subfolder.
--
-- 's3BucketName', 'loggingInfo_s3BucketName' - The name of an S3 bucket where execution logs are stored.
--
-- 's3Region', 'loggingInfo_s3Region' - The Amazon Web Services Region where the S3 bucket is located.
newLoggingInfo ::
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 's3Region'
  Prelude.Text ->
  LoggingInfo
newLoggingInfo pS3BucketName_ pS3Region_ =
  LoggingInfo'
    { s3KeyPrefix = Prelude.Nothing,
      s3BucketName = pS3BucketName_,
      s3Region = pS3Region_
    }

-- | (Optional) The S3 bucket subfolder.
loggingInfo_s3KeyPrefix :: Lens.Lens' LoggingInfo (Prelude.Maybe Prelude.Text)
loggingInfo_s3KeyPrefix = Lens.lens (\LoggingInfo' {s3KeyPrefix} -> s3KeyPrefix) (\s@LoggingInfo' {} a -> s {s3KeyPrefix = a} :: LoggingInfo)

-- | The name of an S3 bucket where execution logs are stored.
loggingInfo_s3BucketName :: Lens.Lens' LoggingInfo Prelude.Text
loggingInfo_s3BucketName = Lens.lens (\LoggingInfo' {s3BucketName} -> s3BucketName) (\s@LoggingInfo' {} a -> s {s3BucketName = a} :: LoggingInfo)

-- | The Amazon Web Services Region where the S3 bucket is located.
loggingInfo_s3Region :: Lens.Lens' LoggingInfo Prelude.Text
loggingInfo_s3Region = Lens.lens (\LoggingInfo' {s3Region} -> s3Region) (\s@LoggingInfo' {} a -> s {s3Region = a} :: LoggingInfo)

instance Data.FromJSON LoggingInfo where
  parseJSON =
    Data.withObject
      "LoggingInfo"
      ( \x ->
          LoggingInfo'
            Prelude.<$> (x Data..:? "S3KeyPrefix")
            Prelude.<*> (x Data..: "S3BucketName")
            Prelude.<*> (x Data..: "S3Region")
      )

instance Prelude.Hashable LoggingInfo where
  hashWithSalt _salt LoggingInfo' {..} =
    _salt `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3Region

instance Prelude.NFData LoggingInfo where
  rnf LoggingInfo' {..} =
    Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3Region

instance Data.ToJSON LoggingInfo where
  toJSON LoggingInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3KeyPrefix" Data..=) Prelude.<$> s3KeyPrefix,
            Prelude.Just ("S3BucketName" Data..= s3BucketName),
            Prelude.Just ("S3Region" Data..= s3Region)
          ]
      )
