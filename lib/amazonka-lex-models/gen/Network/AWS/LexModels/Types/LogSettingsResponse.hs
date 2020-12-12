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
    lsDestination,
    lsKmsKeyARN,
    lsLogType,
    lsResourceARN,
    lsResourcePrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Destination
import Network.AWS.LexModels.Types.LogType
import qualified Network.AWS.Prelude as Lude

-- | The settings for conversation logs.
--
-- /See:/ 'mkLogSettingsResponse' smart constructor.
data LogSettingsResponse = LogSettingsResponse'
  { destination ::
      Lude.Maybe Destination,
    kmsKeyARN :: Lude.Maybe Lude.Text,
    logType :: Lude.Maybe LogType,
    resourceARN :: Lude.Maybe Lude.Text,
    resourcePrefix :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogSettingsResponse' with the minimum fields required to make a request.
--
-- * 'destination' - The destination where logs are delivered.
-- * 'kmsKeyARN' - The Amazon Resource Name (ARN) of the key used to encrypt audio logs in an S3 bucket.
-- * 'logType' - The type of logging that is enabled.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs are delivered.
-- * 'resourcePrefix' - The resource prefix is the first part of the S3 object key within the S3 bucket that you specified to contain audio logs. For CloudWatch Logs it is the prefix of the log stream name within the log group that you specified.
mkLogSettingsResponse ::
  LogSettingsResponse
mkLogSettingsResponse =
  LogSettingsResponse'
    { destination = Lude.Nothing,
      kmsKeyARN = Lude.Nothing,
      logType = Lude.Nothing,
      resourceARN = Lude.Nothing,
      resourcePrefix = Lude.Nothing
    }

-- | The destination where logs are delivered.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsDestination :: Lens.Lens' LogSettingsResponse (Lude.Maybe Destination)
lsDestination = Lens.lens (destination :: LogSettingsResponse -> Lude.Maybe Destination) (\s a -> s {destination = a} :: LogSettingsResponse)
{-# DEPRECATED lsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The Amazon Resource Name (ARN) of the key used to encrypt audio logs in an S3 bucket.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsKmsKeyARN :: Lens.Lens' LogSettingsResponse (Lude.Maybe Lude.Text)
lsKmsKeyARN = Lens.lens (kmsKeyARN :: LogSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: LogSettingsResponse)
{-# DEPRECATED lsKmsKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | The type of logging that is enabled.
--
-- /Note:/ Consider using 'logType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLogType :: Lens.Lens' LogSettingsResponse (Lude.Maybe LogType)
lsLogType = Lens.lens (logType :: LogSettingsResponse -> Lude.Maybe LogType) (\s a -> s {logType = a} :: LogSettingsResponse)
{-# DEPRECATED lsLogType "Use generic-lens or generic-optics with 'logType' instead." #-}

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs are delivered.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsResourceARN :: Lens.Lens' LogSettingsResponse (Lude.Maybe Lude.Text)
lsResourceARN = Lens.lens (resourceARN :: LogSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: LogSettingsResponse)
{-# DEPRECATED lsResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The resource prefix is the first part of the S3 object key within the S3 bucket that you specified to contain audio logs. For CloudWatch Logs it is the prefix of the log stream name within the log group that you specified.
--
-- /Note:/ Consider using 'resourcePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsResourcePrefix :: Lens.Lens' LogSettingsResponse (Lude.Maybe Lude.Text)
lsResourcePrefix = Lens.lens (resourcePrefix :: LogSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourcePrefix = a} :: LogSettingsResponse)
{-# DEPRECATED lsResourcePrefix "Use generic-lens or generic-optics with 'resourcePrefix' instead." #-}

instance Lude.FromJSON LogSettingsResponse where
  parseJSON =
    Lude.withObject
      "LogSettingsResponse"
      ( \x ->
          LogSettingsResponse'
            Lude.<$> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "kmsKeyArn")
            Lude.<*> (x Lude..:? "logType")
            Lude.<*> (x Lude..:? "resourceArn")
            Lude.<*> (x Lude..:? "resourcePrefix")
      )
