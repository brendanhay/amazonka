{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.LogSettingsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.LogSettingsResponse
  ( LogSettingsResponse (..),

    -- * Smart constructor
    mkLogSettingsResponse,

    -- * Lenses
    lsrDestination,
    lsrKmsKeyArn,
    lsrLogType,
    lsrResourceArn,
    lsrResourcePrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.Destination as Types
import qualified Network.AWS.LexModels.Types.KmsKeyArn as Types
import qualified Network.AWS.LexModels.Types.LogType as Types
import qualified Network.AWS.LexModels.Types.ResourceArn as Types
import qualified Network.AWS.LexModels.Types.ResourcePrefix as Types
import qualified Network.AWS.Prelude as Core

-- | The settings for conversation logs.
--
-- /See:/ 'mkLogSettingsResponse' smart constructor.
data LogSettingsResponse = LogSettingsResponse'
  { -- | The destination where logs are delivered.
    destination :: Core.Maybe Types.Destination,
    -- | The Amazon Resource Name (ARN) of the key used to encrypt audio logs in an S3 bucket.
    kmsKeyArn :: Core.Maybe Types.KmsKeyArn,
    -- | The type of logging that is enabled.
    logType :: Core.Maybe Types.LogType,
    -- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs are delivered.
    resourceArn :: Core.Maybe Types.ResourceArn,
    -- | The resource prefix is the first part of the S3 object key within the S3 bucket that you specified to contain audio logs. For CloudWatch Logs it is the prefix of the log stream name within the log group that you specified.
    resourcePrefix :: Core.Maybe Types.ResourcePrefix
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogSettingsResponse' value with any optional fields omitted.
mkLogSettingsResponse ::
  LogSettingsResponse
mkLogSettingsResponse =
  LogSettingsResponse'
    { destination = Core.Nothing,
      kmsKeyArn = Core.Nothing,
      logType = Core.Nothing,
      resourceArn = Core.Nothing,
      resourcePrefix = Core.Nothing
    }

-- | The destination where logs are delivered.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrDestination :: Lens.Lens' LogSettingsResponse (Core.Maybe Types.Destination)
lsrDestination = Lens.field @"destination"
{-# DEPRECATED lsrDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The Amazon Resource Name (ARN) of the key used to encrypt audio logs in an S3 bucket.
--
-- /Note:/ Consider using 'kmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrKmsKeyArn :: Lens.Lens' LogSettingsResponse (Core.Maybe Types.KmsKeyArn)
lsrKmsKeyArn = Lens.field @"kmsKeyArn"
{-# DEPRECATED lsrKmsKeyArn "Use generic-lens or generic-optics with 'kmsKeyArn' instead." #-}

-- | The type of logging that is enabled.
--
-- /Note:/ Consider using 'logType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrLogType :: Lens.Lens' LogSettingsResponse (Core.Maybe Types.LogType)
lsrLogType = Lens.field @"logType"
{-# DEPRECATED lsrLogType "Use generic-lens or generic-optics with 'logType' instead." #-}

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs are delivered.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrResourceArn :: Lens.Lens' LogSettingsResponse (Core.Maybe Types.ResourceArn)
lsrResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED lsrResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | The resource prefix is the first part of the S3 object key within the S3 bucket that you specified to contain audio logs. For CloudWatch Logs it is the prefix of the log stream name within the log group that you specified.
--
-- /Note:/ Consider using 'resourcePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrResourcePrefix :: Lens.Lens' LogSettingsResponse (Core.Maybe Types.ResourcePrefix)
lsrResourcePrefix = Lens.field @"resourcePrefix"
{-# DEPRECATED lsrResourcePrefix "Use generic-lens or generic-optics with 'resourcePrefix' instead." #-}

instance Core.FromJSON LogSettingsResponse where
  parseJSON =
    Core.withObject "LogSettingsResponse" Core.$
      \x ->
        LogSettingsResponse'
          Core.<$> (x Core..:? "destination")
          Core.<*> (x Core..:? "kmsKeyArn")
          Core.<*> (x Core..:? "logType")
          Core.<*> (x Core..:? "resourceArn")
          Core.<*> (x Core..:? "resourcePrefix")
