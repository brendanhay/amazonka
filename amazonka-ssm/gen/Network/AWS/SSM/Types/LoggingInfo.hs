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
-- Module      : Network.AWS.SSM.Types.LoggingInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.LoggingInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an S3 bucket to write instance-level logs to.
--
-- @LoggingInfo@ has been deprecated. To specify an S3 bucket to contain
-- logs, instead use the @OutputS3BucketName@ and @OutputS3KeyPrefix@
-- options in the @TaskInvocationParameters@ structure. For information
-- about how Systems Manager handles these options for the supported
-- maintenance window task types, see
-- MaintenanceWindowTaskInvocationParameters.
--
-- /See:/ 'newLoggingInfo' smart constructor.
data LoggingInfo = LoggingInfo'
  { -- | (Optional) The S3 bucket subfolder.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of an S3 bucket where execution logs are stored .
    s3BucketName :: Prelude.Text,
    -- | The Region where the S3 bucket is located.
    s3Region :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 's3BucketName', 'loggingInfo_s3BucketName' - The name of an S3 bucket where execution logs are stored .
--
-- 's3Region', 'loggingInfo_s3Region' - The Region where the S3 bucket is located.
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

-- | The name of an S3 bucket where execution logs are stored .
loggingInfo_s3BucketName :: Lens.Lens' LoggingInfo Prelude.Text
loggingInfo_s3BucketName = Lens.lens (\LoggingInfo' {s3BucketName} -> s3BucketName) (\s@LoggingInfo' {} a -> s {s3BucketName = a} :: LoggingInfo)

-- | The Region where the S3 bucket is located.
loggingInfo_s3Region :: Lens.Lens' LoggingInfo Prelude.Text
loggingInfo_s3Region = Lens.lens (\LoggingInfo' {s3Region} -> s3Region) (\s@LoggingInfo' {} a -> s {s3Region = a} :: LoggingInfo)

instance Prelude.FromJSON LoggingInfo where
  parseJSON =
    Prelude.withObject
      "LoggingInfo"
      ( \x ->
          LoggingInfo'
            Prelude.<$> (x Prelude..:? "S3KeyPrefix")
            Prelude.<*> (x Prelude..: "S3BucketName")
            Prelude.<*> (x Prelude..: "S3Region")
      )

instance Prelude.Hashable LoggingInfo

instance Prelude.NFData LoggingInfo

instance Prelude.ToJSON LoggingInfo where
  toJSON LoggingInfo' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("S3KeyPrefix" Prelude..=) Prelude.<$> s3KeyPrefix,
            Prelude.Just
              ("S3BucketName" Prelude..= s3BucketName),
            Prelude.Just ("S3Region" Prelude..= s3Region)
          ]
      )
