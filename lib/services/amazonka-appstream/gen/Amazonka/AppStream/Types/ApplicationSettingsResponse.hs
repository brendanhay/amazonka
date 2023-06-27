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
-- Module      : Amazonka.AppStream.Types.ApplicationSettingsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ApplicationSettingsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the persistent application settings for users of a stack.
--
-- /See:/ 'newApplicationSettingsResponse' smart constructor.
data ApplicationSettingsResponse = ApplicationSettingsResponse'
  { -- | Specifies whether persistent application settings are enabled for users
    -- during their streaming sessions.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The S3 bucket where users’ persistent application settings are stored.
    -- When persistent application settings are enabled for the first time for
    -- an account in an AWS Region, an S3 bucket is created. The bucket is
    -- unique to the AWS account and the Region.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The path prefix for the S3 bucket where users’ persistent application
    -- settings are stored.
    settingsGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 's3BucketName', 'applicationSettingsResponse_s3BucketName' - The S3 bucket where users’ persistent application settings are stored.
-- When persistent application settings are enabled for the first time for
-- an account in an AWS Region, an S3 bucket is created. The bucket is
-- unique to the AWS account and the Region.
--
-- 'settingsGroup', 'applicationSettingsResponse_settingsGroup' - The path prefix for the S3 bucket where users’ persistent application
-- settings are stored.
newApplicationSettingsResponse ::
  ApplicationSettingsResponse
newApplicationSettingsResponse =
  ApplicationSettingsResponse'
    { enabled =
        Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      settingsGroup = Prelude.Nothing
    }

-- | Specifies whether persistent application settings are enabled for users
-- during their streaming sessions.
applicationSettingsResponse_enabled :: Lens.Lens' ApplicationSettingsResponse (Prelude.Maybe Prelude.Bool)
applicationSettingsResponse_enabled = Lens.lens (\ApplicationSettingsResponse' {enabled} -> enabled) (\s@ApplicationSettingsResponse' {} a -> s {enabled = a} :: ApplicationSettingsResponse)

-- | The S3 bucket where users’ persistent application settings are stored.
-- When persistent application settings are enabled for the first time for
-- an account in an AWS Region, an S3 bucket is created. The bucket is
-- unique to the AWS account and the Region.
applicationSettingsResponse_s3BucketName :: Lens.Lens' ApplicationSettingsResponse (Prelude.Maybe Prelude.Text)
applicationSettingsResponse_s3BucketName = Lens.lens (\ApplicationSettingsResponse' {s3BucketName} -> s3BucketName) (\s@ApplicationSettingsResponse' {} a -> s {s3BucketName = a} :: ApplicationSettingsResponse)

-- | The path prefix for the S3 bucket where users’ persistent application
-- settings are stored.
applicationSettingsResponse_settingsGroup :: Lens.Lens' ApplicationSettingsResponse (Prelude.Maybe Prelude.Text)
applicationSettingsResponse_settingsGroup = Lens.lens (\ApplicationSettingsResponse' {settingsGroup} -> settingsGroup) (\s@ApplicationSettingsResponse' {} a -> s {settingsGroup = a} :: ApplicationSettingsResponse)

instance Data.FromJSON ApplicationSettingsResponse where
  parseJSON =
    Data.withObject
      "ApplicationSettingsResponse"
      ( \x ->
          ApplicationSettingsResponse'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "S3BucketName")
            Prelude.<*> (x Data..:? "SettingsGroup")
      )

instance Prelude.Hashable ApplicationSettingsResponse where
  hashWithSalt _salt ApplicationSettingsResponse' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` settingsGroup

instance Prelude.NFData ApplicationSettingsResponse where
  rnf ApplicationSettingsResponse' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf settingsGroup
