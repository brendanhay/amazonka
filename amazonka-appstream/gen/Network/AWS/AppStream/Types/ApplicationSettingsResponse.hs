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
-- Module      : Network.AWS.AppStream.Types.ApplicationSettingsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ApplicationSettingsResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the persistent application settings for users of a stack.
--
-- /See:/ 'newApplicationSettingsResponse' smart constructor.
data ApplicationSettingsResponse = ApplicationSettingsResponse'
  { -- | Specifies whether persistent application settings are enabled for users
    -- during their streaming sessions.
    enabled :: Core.Maybe Core.Bool,
    -- | The path prefix for the S3 bucket where users’ persistent application
    -- settings are stored.
    settingsGroup :: Core.Maybe Core.Text,
    -- | The S3 bucket where users’ persistent application settings are stored.
    -- When persistent application settings are enabled for the first time for
    -- an account in an AWS Region, an S3 bucket is created. The bucket is
    -- unique to the AWS account and the Region.
    s3BucketName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'applicationSettingsResponse_enabled' - Specifies whether persistent application settings are enabled for users
-- during their streaming sessions.
--
-- 'settingsGroup', 'applicationSettingsResponse_settingsGroup' - The path prefix for the S3 bucket where users’ persistent application
-- settings are stored.
--
-- 's3BucketName', 'applicationSettingsResponse_s3BucketName' - The S3 bucket where users’ persistent application settings are stored.
-- When persistent application settings are enabled for the first time for
-- an account in an AWS Region, an S3 bucket is created. The bucket is
-- unique to the AWS account and the Region.
newApplicationSettingsResponse ::
  ApplicationSettingsResponse
newApplicationSettingsResponse =
  ApplicationSettingsResponse'
    { enabled =
        Core.Nothing,
      settingsGroup = Core.Nothing,
      s3BucketName = Core.Nothing
    }

-- | Specifies whether persistent application settings are enabled for users
-- during their streaming sessions.
applicationSettingsResponse_enabled :: Lens.Lens' ApplicationSettingsResponse (Core.Maybe Core.Bool)
applicationSettingsResponse_enabled = Lens.lens (\ApplicationSettingsResponse' {enabled} -> enabled) (\s@ApplicationSettingsResponse' {} a -> s {enabled = a} :: ApplicationSettingsResponse)

-- | The path prefix for the S3 bucket where users’ persistent application
-- settings are stored.
applicationSettingsResponse_settingsGroup :: Lens.Lens' ApplicationSettingsResponse (Core.Maybe Core.Text)
applicationSettingsResponse_settingsGroup = Lens.lens (\ApplicationSettingsResponse' {settingsGroup} -> settingsGroup) (\s@ApplicationSettingsResponse' {} a -> s {settingsGroup = a} :: ApplicationSettingsResponse)

-- | The S3 bucket where users’ persistent application settings are stored.
-- When persistent application settings are enabled for the first time for
-- an account in an AWS Region, an S3 bucket is created. The bucket is
-- unique to the AWS account and the Region.
applicationSettingsResponse_s3BucketName :: Lens.Lens' ApplicationSettingsResponse (Core.Maybe Core.Text)
applicationSettingsResponse_s3BucketName = Lens.lens (\ApplicationSettingsResponse' {s3BucketName} -> s3BucketName) (\s@ApplicationSettingsResponse' {} a -> s {s3BucketName = a} :: ApplicationSettingsResponse)

instance Core.FromJSON ApplicationSettingsResponse where
  parseJSON =
    Core.withObject
      "ApplicationSettingsResponse"
      ( \x ->
          ApplicationSettingsResponse'
            Core.<$> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "SettingsGroup")
            Core.<*> (x Core..:? "S3BucketName")
      )

instance Core.Hashable ApplicationSettingsResponse

instance Core.NFData ApplicationSettingsResponse
