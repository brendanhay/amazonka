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
-- Module      : Network.AWS.AppSync.Types.LogConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.LogConfig where

import Network.AWS.AppSync.Types.FieldLogLevel
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The CloudWatch Logs configuration.
--
-- /See:/ 'newLogConfig' smart constructor.
data LogConfig = LogConfig'
  { -- | Set to TRUE to exclude sections that contain information such as
    -- headers, context, and evaluated mapping templates, regardless of logging
    -- level.
    excludeVerboseContent :: Core.Maybe Core.Bool,
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
    -- | The service role that AWS AppSync will assume to publish to Amazon
    -- CloudWatch logs in your account.
    cloudWatchLogsRoleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'cloudWatchLogsRoleArn', 'logConfig_cloudWatchLogsRoleArn' - The service role that AWS AppSync will assume to publish to Amazon
-- CloudWatch logs in your account.
newLogConfig ::
  -- | 'fieldLogLevel'
  FieldLogLevel ->
  -- | 'cloudWatchLogsRoleArn'
  Core.Text ->
  LogConfig
newLogConfig pFieldLogLevel_ pCloudWatchLogsRoleArn_ =
  LogConfig'
    { excludeVerboseContent = Core.Nothing,
      fieldLogLevel = pFieldLogLevel_,
      cloudWatchLogsRoleArn = pCloudWatchLogsRoleArn_
    }

-- | Set to TRUE to exclude sections that contain information such as
-- headers, context, and evaluated mapping templates, regardless of logging
-- level.
logConfig_excludeVerboseContent :: Lens.Lens' LogConfig (Core.Maybe Core.Bool)
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

-- | The service role that AWS AppSync will assume to publish to Amazon
-- CloudWatch logs in your account.
logConfig_cloudWatchLogsRoleArn :: Lens.Lens' LogConfig Core.Text
logConfig_cloudWatchLogsRoleArn = Lens.lens (\LogConfig' {cloudWatchLogsRoleArn} -> cloudWatchLogsRoleArn) (\s@LogConfig' {} a -> s {cloudWatchLogsRoleArn = a} :: LogConfig)

instance Core.FromJSON LogConfig where
  parseJSON =
    Core.withObject
      "LogConfig"
      ( \x ->
          LogConfig'
            Core.<$> (x Core..:? "excludeVerboseContent")
            Core.<*> (x Core..: "fieldLogLevel")
            Core.<*> (x Core..: "cloudWatchLogsRoleArn")
      )

instance Core.Hashable LogConfig

instance Core.NFData LogConfig

instance Core.ToJSON LogConfig where
  toJSON LogConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("excludeVerboseContent" Core..=)
              Core.<$> excludeVerboseContent,
            Core.Just ("fieldLogLevel" Core..= fieldLogLevel),
            Core.Just
              ( "cloudWatchLogsRoleArn"
                  Core..= cloudWatchLogsRoleArn
              )
          ]
      )
