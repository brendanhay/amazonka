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
-- Module      : Amazonka.CodeBuild.Types.S3LogsConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.S3LogsConfig where

import Amazonka.CodeBuild.Types.BucketOwnerAccess
import Amazonka.CodeBuild.Types.LogsConfigStatusType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about S3 logs for a build project.
--
-- /See:/ 'newS3LogsConfig' smart constructor.
data S3LogsConfig = S3LogsConfig'
  { bucketOwnerAccess :: Prelude.Maybe BucketOwnerAccess,
    -- | Set to true if you do not want your S3 build log output encrypted. By
    -- default S3 build logs are encrypted.
    encryptionDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of an S3 bucket and the path prefix for S3 logs. If your Amazon
    -- S3 bucket name is @my-bucket@, and your path prefix is @build-log@, then
    -- acceptable formats are @my-bucket\/build-log@ or
    -- @arn:aws:s3:::my-bucket\/build-log@.
    location :: Prelude.Maybe Prelude.Text,
    -- | The current status of the S3 build logs. Valid values are:
    --
    -- -   @ENABLED@: S3 build logs are enabled for this build project.
    --
    -- -   @DISABLED@: S3 build logs are not enabled for this build project.
    status :: LogsConfigStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3LogsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketOwnerAccess', 's3LogsConfig_bucketOwnerAccess' - Undocumented member.
--
-- 'encryptionDisabled', 's3LogsConfig_encryptionDisabled' - Set to true if you do not want your S3 build log output encrypted. By
-- default S3 build logs are encrypted.
--
-- 'location', 's3LogsConfig_location' - The ARN of an S3 bucket and the path prefix for S3 logs. If your Amazon
-- S3 bucket name is @my-bucket@, and your path prefix is @build-log@, then
-- acceptable formats are @my-bucket\/build-log@ or
-- @arn:aws:s3:::my-bucket\/build-log@.
--
-- 'status', 's3LogsConfig_status' - The current status of the S3 build logs. Valid values are:
--
-- -   @ENABLED@: S3 build logs are enabled for this build project.
--
-- -   @DISABLED@: S3 build logs are not enabled for this build project.
newS3LogsConfig ::
  -- | 'status'
  LogsConfigStatusType ->
  S3LogsConfig
newS3LogsConfig pStatus_ =
  S3LogsConfig'
    { bucketOwnerAccess = Prelude.Nothing,
      encryptionDisabled = Prelude.Nothing,
      location = Prelude.Nothing,
      status = pStatus_
    }

-- | Undocumented member.
s3LogsConfig_bucketOwnerAccess :: Lens.Lens' S3LogsConfig (Prelude.Maybe BucketOwnerAccess)
s3LogsConfig_bucketOwnerAccess = Lens.lens (\S3LogsConfig' {bucketOwnerAccess} -> bucketOwnerAccess) (\s@S3LogsConfig' {} a -> s {bucketOwnerAccess = a} :: S3LogsConfig)

-- | Set to true if you do not want your S3 build log output encrypted. By
-- default S3 build logs are encrypted.
s3LogsConfig_encryptionDisabled :: Lens.Lens' S3LogsConfig (Prelude.Maybe Prelude.Bool)
s3LogsConfig_encryptionDisabled = Lens.lens (\S3LogsConfig' {encryptionDisabled} -> encryptionDisabled) (\s@S3LogsConfig' {} a -> s {encryptionDisabled = a} :: S3LogsConfig)

-- | The ARN of an S3 bucket and the path prefix for S3 logs. If your Amazon
-- S3 bucket name is @my-bucket@, and your path prefix is @build-log@, then
-- acceptable formats are @my-bucket\/build-log@ or
-- @arn:aws:s3:::my-bucket\/build-log@.
s3LogsConfig_location :: Lens.Lens' S3LogsConfig (Prelude.Maybe Prelude.Text)
s3LogsConfig_location = Lens.lens (\S3LogsConfig' {location} -> location) (\s@S3LogsConfig' {} a -> s {location = a} :: S3LogsConfig)

-- | The current status of the S3 build logs. Valid values are:
--
-- -   @ENABLED@: S3 build logs are enabled for this build project.
--
-- -   @DISABLED@: S3 build logs are not enabled for this build project.
s3LogsConfig_status :: Lens.Lens' S3LogsConfig LogsConfigStatusType
s3LogsConfig_status = Lens.lens (\S3LogsConfig' {status} -> status) (\s@S3LogsConfig' {} a -> s {status = a} :: S3LogsConfig)

instance Data.FromJSON S3LogsConfig where
  parseJSON =
    Data.withObject
      "S3LogsConfig"
      ( \x ->
          S3LogsConfig'
            Prelude.<$> (x Data..:? "bucketOwnerAccess")
            Prelude.<*> (x Data..:? "encryptionDisabled")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable S3LogsConfig where
  hashWithSalt _salt S3LogsConfig' {..} =
    _salt `Prelude.hashWithSalt` bucketOwnerAccess
      `Prelude.hashWithSalt` encryptionDisabled
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` status

instance Prelude.NFData S3LogsConfig where
  rnf S3LogsConfig' {..} =
    Prelude.rnf bucketOwnerAccess
      `Prelude.seq` Prelude.rnf encryptionDisabled
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON S3LogsConfig where
  toJSON S3LogsConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucketOwnerAccess" Data..=)
              Prelude.<$> bucketOwnerAccess,
            ("encryptionDisabled" Data..=)
              Prelude.<$> encryptionDisabled,
            ("location" Data..=) Prelude.<$> location,
            Prelude.Just ("status" Data..= status)
          ]
      )
