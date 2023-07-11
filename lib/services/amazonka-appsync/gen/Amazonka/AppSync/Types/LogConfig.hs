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
-- Module      : Amazonka.AppSync.Types.LogConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.LogConfig where

import Amazonka.AppSync.Types.FieldLogLevel
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon CloudWatch Logs configuration.
--
-- /See:/ 'newLogConfig' smart constructor.
data LogConfig = LogConfig'
  { -- | Set to TRUE to exclude sections that contain information such as
    -- headers, context, and evaluated mapping templates, regardless of logging
    -- level.
    excludeVerboseContent :: Prelude.Maybe Prelude.Bool,
    -- | The field logging level. Values can be NONE, ERROR, or ALL.
    --
    -- -   __NONE__: No field-level logs are captured.
    --
    -- -   __ERROR__: Logs the following information only for the fields that
    --     are in error:
    --
    --     -   The error section in the server response.
    --
    --     -   Field-level errors.
    --
    --     -   The generated request\/response functions that got resolved for
    --         error fields.
    --
    -- -   __ALL__: The following information is logged for all fields in the
    --     query:
    --
    --     -   Field-level tracing information.
    --
    --     -   The generated request\/response functions that got resolved for
    --         each field.
    fieldLogLevel :: FieldLogLevel,
    -- | The service role that AppSync assumes to publish to CloudWatch logs in
    -- your account.
    cloudWatchLogsRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludeVerboseContent', 'logConfig_excludeVerboseContent' - Set to TRUE to exclude sections that contain information such as
-- headers, context, and evaluated mapping templates, regardless of logging
-- level.
--
-- 'fieldLogLevel', 'logConfig_fieldLogLevel' - The field logging level. Values can be NONE, ERROR, or ALL.
--
-- -   __NONE__: No field-level logs are captured.
--
-- -   __ERROR__: Logs the following information only for the fields that
--     are in error:
--
--     -   The error section in the server response.
--
--     -   Field-level errors.
--
--     -   The generated request\/response functions that got resolved for
--         error fields.
--
-- -   __ALL__: The following information is logged for all fields in the
--     query:
--
--     -   Field-level tracing information.
--
--     -   The generated request\/response functions that got resolved for
--         each field.
--
-- 'cloudWatchLogsRoleArn', 'logConfig_cloudWatchLogsRoleArn' - The service role that AppSync assumes to publish to CloudWatch logs in
-- your account.
newLogConfig ::
  -- | 'fieldLogLevel'
  FieldLogLevel ->
  -- | 'cloudWatchLogsRoleArn'
  Prelude.Text ->
  LogConfig
newLogConfig pFieldLogLevel_ pCloudWatchLogsRoleArn_ =
  LogConfig'
    { excludeVerboseContent = Prelude.Nothing,
      fieldLogLevel = pFieldLogLevel_,
      cloudWatchLogsRoleArn = pCloudWatchLogsRoleArn_
    }

-- | Set to TRUE to exclude sections that contain information such as
-- headers, context, and evaluated mapping templates, regardless of logging
-- level.
logConfig_excludeVerboseContent :: Lens.Lens' LogConfig (Prelude.Maybe Prelude.Bool)
logConfig_excludeVerboseContent = Lens.lens (\LogConfig' {excludeVerboseContent} -> excludeVerboseContent) (\s@LogConfig' {} a -> s {excludeVerboseContent = a} :: LogConfig)

-- | The field logging level. Values can be NONE, ERROR, or ALL.
--
-- -   __NONE__: No field-level logs are captured.
--
-- -   __ERROR__: Logs the following information only for the fields that
--     are in error:
--
--     -   The error section in the server response.
--
--     -   Field-level errors.
--
--     -   The generated request\/response functions that got resolved for
--         error fields.
--
-- -   __ALL__: The following information is logged for all fields in the
--     query:
--
--     -   Field-level tracing information.
--
--     -   The generated request\/response functions that got resolved for
--         each field.
logConfig_fieldLogLevel :: Lens.Lens' LogConfig FieldLogLevel
logConfig_fieldLogLevel = Lens.lens (\LogConfig' {fieldLogLevel} -> fieldLogLevel) (\s@LogConfig' {} a -> s {fieldLogLevel = a} :: LogConfig)

-- | The service role that AppSync assumes to publish to CloudWatch logs in
-- your account.
logConfig_cloudWatchLogsRoleArn :: Lens.Lens' LogConfig Prelude.Text
logConfig_cloudWatchLogsRoleArn = Lens.lens (\LogConfig' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@LogConfig' {} a -> s {cloudWatchLogsRoleArn = a} :: LogConfig)

instance Data.FromJSON LogConfig where
  parseJSON =
    Data.withObject
      "LogConfig"
      ( \x ->
          LogConfig'
            Prelude.<$> (x Data..:? "excludeVerboseContent")
            Prelude.<*> (x Data..: "fieldLogLevel")
            Prelude.<*> (x Data..: "cloudWatchLogsRoleArn")
      )

instance Prelude.Hashable LogConfig where
  hashWithSalt _salt LogConfig' {..} =
    _salt
      `Prelude.hashWithSalt` excludeVerboseContent
      `Prelude.hashWithSalt` fieldLogLevel
      `Prelude.hashWithSalt` cloudWatchLogsRoleArn

instance Prelude.NFData LogConfig where
  rnf LogConfig' {..} =
    Prelude.rnf excludeVerboseContent
      `Prelude.seq` Prelude.rnf fieldLogLevel
      `Prelude.seq` Prelude.rnf cloudWatchLogsRoleArn

instance Data.ToJSON LogConfig where
  toJSON LogConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("excludeVerboseContent" Data..=)
              Prelude.<$> excludeVerboseContent,
            Prelude.Just ("fieldLogLevel" Data..= fieldLogLevel),
            Prelude.Just
              ( "cloudWatchLogsRoleArn"
                  Data..= cloudWatchLogsRoleArn
              )
          ]
      )
