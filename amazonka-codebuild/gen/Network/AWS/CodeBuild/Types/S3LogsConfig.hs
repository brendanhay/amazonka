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
-- Module      : Network.AWS.CodeBuild.Types.S3LogsConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.S3LogsConfig where

import Network.AWS.CodeBuild.Types.LogsConfigStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about S3 logs for a build project.
--
-- /See:/ 'newS3LogsConfig' smart constructor.
data S3LogsConfig = S3LogsConfig'
  { -- | Set to true if you do not want your S3 build log output encrypted. By
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3LogsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
    { encryptionDisabled = Prelude.Nothing,
      location = Prelude.Nothing,
      status = pStatus_
    }

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

instance Prelude.FromJSON S3LogsConfig where
  parseJSON =
    Prelude.withObject
      "S3LogsConfig"
      ( \x ->
          S3LogsConfig'
            Prelude.<$> (x Prelude..:? "encryptionDisabled")
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..: "status")
      )

instance Prelude.Hashable S3LogsConfig

instance Prelude.NFData S3LogsConfig

instance Prelude.ToJSON S3LogsConfig where
  toJSON S3LogsConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("encryptionDisabled" Prelude..=)
              Prelude.<$> encryptionDisabled,
            ("location" Prelude..=) Prelude.<$> location,
            Prelude.Just ("status" Prelude..= status)
          ]
      )
