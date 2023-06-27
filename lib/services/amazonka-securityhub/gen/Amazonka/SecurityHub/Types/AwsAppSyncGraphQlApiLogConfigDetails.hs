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
-- Module      : Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiLogConfigDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiLogConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the logging configuration when writing GraphQL operations and
-- tracing to Amazon CloudWatch for an AppSync GraphQL API.
--
-- /See:/ 'newAwsAppSyncGraphQlApiLogConfigDetails' smart constructor.
data AwsAppSyncGraphQlApiLogConfigDetails = AwsAppSyncGraphQlApiLogConfigDetails'
  { -- | The Amazon Resource Name (ARN) of the service role that AppSync assumes
    -- to publish to CloudWatch Logs in your account.
    cloudWatchLogsRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Set to @TRUE@ to exclude sections that contain information such as
    -- headers, context, and evaluated mapping templates, regardless of logging
    -- level.
    excludeVerboseContent :: Prelude.Maybe Prelude.Bool,
    -- | The field logging level.
    fieldLogLevel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAppSyncGraphQlApiLogConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogsRoleArn', 'awsAppSyncGraphQlApiLogConfigDetails_cloudWatchLogsRoleArn' - The Amazon Resource Name (ARN) of the service role that AppSync assumes
-- to publish to CloudWatch Logs in your account.
--
-- 'excludeVerboseContent', 'awsAppSyncGraphQlApiLogConfigDetails_excludeVerboseContent' - Set to @TRUE@ to exclude sections that contain information such as
-- headers, context, and evaluated mapping templates, regardless of logging
-- level.
--
-- 'fieldLogLevel', 'awsAppSyncGraphQlApiLogConfigDetails_fieldLogLevel' - The field logging level.
newAwsAppSyncGraphQlApiLogConfigDetails ::
  AwsAppSyncGraphQlApiLogConfigDetails
newAwsAppSyncGraphQlApiLogConfigDetails =
  AwsAppSyncGraphQlApiLogConfigDetails'
    { cloudWatchLogsRoleArn =
        Prelude.Nothing,
      excludeVerboseContent =
        Prelude.Nothing,
      fieldLogLevel = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the service role that AppSync assumes
-- to publish to CloudWatch Logs in your account.
awsAppSyncGraphQlApiLogConfigDetails_cloudWatchLogsRoleArn :: Lens.Lens' AwsAppSyncGraphQlApiLogConfigDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiLogConfigDetails_cloudWatchLogsRoleArn = Lens.lens (\AwsAppSyncGraphQlApiLogConfigDetails' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@AwsAppSyncGraphQlApiLogConfigDetails' {} a -> s {cloudWatchLogsRoleArn = a} :: AwsAppSyncGraphQlApiLogConfigDetails)

-- | Set to @TRUE@ to exclude sections that contain information such as
-- headers, context, and evaluated mapping templates, regardless of logging
-- level.
awsAppSyncGraphQlApiLogConfigDetails_excludeVerboseContent :: Lens.Lens' AwsAppSyncGraphQlApiLogConfigDetails (Prelude.Maybe Prelude.Bool)
awsAppSyncGraphQlApiLogConfigDetails_excludeVerboseContent = Lens.lens (\AwsAppSyncGraphQlApiLogConfigDetails' {excludeVerboseContent} -> excludeVerboseContent) (\s@AwsAppSyncGraphQlApiLogConfigDetails' {} a -> s {excludeVerboseContent = a} :: AwsAppSyncGraphQlApiLogConfigDetails)

-- | The field logging level.
awsAppSyncGraphQlApiLogConfigDetails_fieldLogLevel :: Lens.Lens' AwsAppSyncGraphQlApiLogConfigDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiLogConfigDetails_fieldLogLevel = Lens.lens (\AwsAppSyncGraphQlApiLogConfigDetails' {fieldLogLevel} -> fieldLogLevel) (\s@AwsAppSyncGraphQlApiLogConfigDetails' {} a -> s {fieldLogLevel = a} :: AwsAppSyncGraphQlApiLogConfigDetails)

instance
  Data.FromJSON
    AwsAppSyncGraphQlApiLogConfigDetails
  where
  parseJSON =
    Data.withObject
      "AwsAppSyncGraphQlApiLogConfigDetails"
      ( \x ->
          AwsAppSyncGraphQlApiLogConfigDetails'
            Prelude.<$> (x Data..:? "CloudWatchLogsRoleArn")
            Prelude.<*> (x Data..:? "ExcludeVerboseContent")
            Prelude.<*> (x Data..:? "FieldLogLevel")
      )

instance
  Prelude.Hashable
    AwsAppSyncGraphQlApiLogConfigDetails
  where
  hashWithSalt
    _salt
    AwsAppSyncGraphQlApiLogConfigDetails' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchLogsRoleArn
        `Prelude.hashWithSalt` excludeVerboseContent
        `Prelude.hashWithSalt` fieldLogLevel

instance
  Prelude.NFData
    AwsAppSyncGraphQlApiLogConfigDetails
  where
  rnf AwsAppSyncGraphQlApiLogConfigDetails' {..} =
    Prelude.rnf cloudWatchLogsRoleArn
      `Prelude.seq` Prelude.rnf excludeVerboseContent
      `Prelude.seq` Prelude.rnf fieldLogLevel

instance
  Data.ToJSON
    AwsAppSyncGraphQlApiLogConfigDetails
  where
  toJSON AwsAppSyncGraphQlApiLogConfigDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLogsRoleArn" Data..=)
              Prelude.<$> cloudWatchLogsRoleArn,
            ("ExcludeVerboseContent" Data..=)
              Prelude.<$> excludeVerboseContent,
            ("FieldLogLevel" Data..=) Prelude.<$> fieldLogLevel
          ]
      )
