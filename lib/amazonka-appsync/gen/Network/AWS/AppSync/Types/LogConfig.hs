{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.LogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.LogConfig
  ( LogConfig (..),

    -- * Smart constructor
    mkLogConfig,

    -- * Lenses
    lcFieldLogLevel,
    lcCloudWatchLogsRoleArn,
    lcExcludeVerboseContent,
  )
where

import qualified Network.AWS.AppSync.Types.FieldLogLevel as Types
import qualified Network.AWS.AppSync.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The CloudWatch Logs configuration.
--
-- /See:/ 'mkLogConfig' smart constructor.
data LogConfig = LogConfig'
  { -- | The field logging level. Values can be NONE, ERROR, or ALL.
    --
    --
    --     * __NONE__ : No field-level logs are captured.
    --
    --
    --     * __ERROR__ : Logs the following information only for the fields that are in error:
    --
    --     * The error section in the server response.
    --
    --
    --     * Field-level errors.
    --
    --
    --     * The generated request/response functions that got resolved for error fields.
    --
    --
    --
    --
    --     * __ALL__ : The following information is logged for all fields in the query:
    --
    --     * Field-level tracing information.
    --
    --
    --     * The generated request/response functions that got resolved for each field.
    fieldLogLevel :: Types.FieldLogLevel,
    -- | The service role that AWS AppSync will assume to publish to Amazon CloudWatch logs in your account.
    cloudWatchLogsRoleArn :: Types.String,
    -- | Set to TRUE to exclude sections that contain information such as headers, context, and evaluated mapping templates, regardless of logging level.
    excludeVerboseContent :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogConfig' value with any optional fields omitted.
mkLogConfig ::
  -- | 'fieldLogLevel'
  Types.FieldLogLevel ->
  -- | 'cloudWatchLogsRoleArn'
  Types.String ->
  LogConfig
mkLogConfig fieldLogLevel cloudWatchLogsRoleArn =
  LogConfig'
    { fieldLogLevel,
      cloudWatchLogsRoleArn,
      excludeVerboseContent = Core.Nothing
    }

-- | The field logging level. Values can be NONE, ERROR, or ALL.
--
--
--     * __NONE__ : No field-level logs are captured.
--
--
--     * __ERROR__ : Logs the following information only for the fields that are in error:
--
--     * The error section in the server response.
--
--
--     * Field-level errors.
--
--
--     * The generated request/response functions that got resolved for error fields.
--
--
--
--
--     * __ALL__ : The following information is logged for all fields in the query:
--
--     * Field-level tracing information.
--
--
--     * The generated request/response functions that got resolved for each field.
--
--
--
--
--
-- /Note:/ Consider using 'fieldLogLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcFieldLogLevel :: Lens.Lens' LogConfig Types.FieldLogLevel
lcFieldLogLevel = Lens.field @"fieldLogLevel"
{-# DEPRECATED lcFieldLogLevel "Use generic-lens or generic-optics with 'fieldLogLevel' instead." #-}

-- | The service role that AWS AppSync will assume to publish to Amazon CloudWatch logs in your account.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCloudWatchLogsRoleArn :: Lens.Lens' LogConfig Types.String
lcCloudWatchLogsRoleArn = Lens.field @"cloudWatchLogsRoleArn"
{-# DEPRECATED lcCloudWatchLogsRoleArn "Use generic-lens or generic-optics with 'cloudWatchLogsRoleArn' instead." #-}

-- | Set to TRUE to exclude sections that contain information such as headers, context, and evaluated mapping templates, regardless of logging level.
--
-- /Note:/ Consider using 'excludeVerboseContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcExcludeVerboseContent :: Lens.Lens' LogConfig (Core.Maybe Core.Bool)
lcExcludeVerboseContent = Lens.field @"excludeVerboseContent"
{-# DEPRECATED lcExcludeVerboseContent "Use generic-lens or generic-optics with 'excludeVerboseContent' instead." #-}

instance Core.FromJSON LogConfig where
  toJSON LogConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("fieldLogLevel" Core..= fieldLogLevel),
            Core.Just ("cloudWatchLogsRoleArn" Core..= cloudWatchLogsRoleArn),
            ("excludeVerboseContent" Core..=) Core.<$> excludeVerboseContent
          ]
      )

instance Core.FromJSON LogConfig where
  parseJSON =
    Core.withObject "LogConfig" Core.$
      \x ->
        LogConfig'
          Core.<$> (x Core..: "fieldLogLevel")
          Core.<*> (x Core..: "cloudWatchLogsRoleArn")
          Core.<*> (x Core..:? "excludeVerboseContent")
