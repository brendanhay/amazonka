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
    lcExcludeVerboseContent,
    lcFieldLogLevel,
    lcCloudWatchLogsRoleARN,
  )
where

import Network.AWS.AppSync.Types.FieldLogLevel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The CloudWatch Logs configuration.
--
-- /See:/ 'mkLogConfig' smart constructor.
data LogConfig = LogConfig'
  { excludeVerboseContent ::
      Lude.Maybe Lude.Bool,
    fieldLogLevel :: FieldLogLevel,
    cloudWatchLogsRoleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogConfig' with the minimum fields required to make a request.
--
-- * 'cloudWatchLogsRoleARN' - The service role that AWS AppSync will assume to publish to Amazon CloudWatch logs in your account.
-- * 'excludeVerboseContent' - Set to TRUE to exclude sections that contain information such as headers, context, and evaluated mapping templates, regardless of logging level.
-- * 'fieldLogLevel' - The field logging level. Values can be NONE, ERROR, or ALL.
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
mkLogConfig ::
  -- | 'fieldLogLevel'
  FieldLogLevel ->
  -- | 'cloudWatchLogsRoleARN'
  Lude.Text ->
  LogConfig
mkLogConfig pFieldLogLevel_ pCloudWatchLogsRoleARN_ =
  LogConfig'
    { excludeVerboseContent = Lude.Nothing,
      fieldLogLevel = pFieldLogLevel_,
      cloudWatchLogsRoleARN = pCloudWatchLogsRoleARN_
    }

-- | Set to TRUE to exclude sections that contain information such as headers, context, and evaluated mapping templates, regardless of logging level.
--
-- /Note:/ Consider using 'excludeVerboseContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcExcludeVerboseContent :: Lens.Lens' LogConfig (Lude.Maybe Lude.Bool)
lcExcludeVerboseContent = Lens.lens (excludeVerboseContent :: LogConfig -> Lude.Maybe Lude.Bool) (\s a -> s {excludeVerboseContent = a} :: LogConfig)
{-# DEPRECATED lcExcludeVerboseContent "Use generic-lens or generic-optics with 'excludeVerboseContent' instead." #-}

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
lcFieldLogLevel :: Lens.Lens' LogConfig FieldLogLevel
lcFieldLogLevel = Lens.lens (fieldLogLevel :: LogConfig -> FieldLogLevel) (\s a -> s {fieldLogLevel = a} :: LogConfig)
{-# DEPRECATED lcFieldLogLevel "Use generic-lens or generic-optics with 'fieldLogLevel' instead." #-}

-- | The service role that AWS AppSync will assume to publish to Amazon CloudWatch logs in your account.
--
-- /Note:/ Consider using 'cloudWatchLogsRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCloudWatchLogsRoleARN :: Lens.Lens' LogConfig Lude.Text
lcCloudWatchLogsRoleARN = Lens.lens (cloudWatchLogsRoleARN :: LogConfig -> Lude.Text) (\s a -> s {cloudWatchLogsRoleARN = a} :: LogConfig)
{-# DEPRECATED lcCloudWatchLogsRoleARN "Use generic-lens or generic-optics with 'cloudWatchLogsRoleARN' instead." #-}

instance Lude.FromJSON LogConfig where
  parseJSON =
    Lude.withObject
      "LogConfig"
      ( \x ->
          LogConfig'
            Lude.<$> (x Lude..:? "excludeVerboseContent")
            Lude.<*> (x Lude..: "fieldLogLevel")
            Lude.<*> (x Lude..: "cloudWatchLogsRoleArn")
      )

instance Lude.ToJSON LogConfig where
  toJSON LogConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("excludeVerboseContent" Lude..=) Lude.<$> excludeVerboseContent,
            Lude.Just ("fieldLogLevel" Lude..= fieldLogLevel),
            Lude.Just ("cloudWatchLogsRoleArn" Lude..= cloudWatchLogsRoleARN)
          ]
      )
