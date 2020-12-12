{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.LogSettingsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.LogSettingsRequest
  ( LogSettingsRequest (..),

    -- * Smart constructor
    mkLogSettingsRequest,

    -- * Lenses
    lsrKmsKeyARN,
    lsrLogType,
    lsrDestination,
    lsrResourceARN,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Destination
import Network.AWS.LexModels.Types.LogType
import qualified Network.AWS.Prelude as Lude

-- | Settings used to configure delivery mode and destination for conversation logs.
--
-- /See:/ 'mkLogSettingsRequest' smart constructor.
data LogSettingsRequest = LogSettingsRequest'
  { kmsKeyARN ::
      Lude.Maybe Lude.Text,
    logType :: LogType,
    destination :: Destination,
    resourceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogSettingsRequest' with the minimum fields required to make a request.
--
-- * 'destination' - Where the logs will be delivered. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
-- * 'kmsKeyARN' - The Amazon Resource Name (ARN) of the AWS KMS customer managed key for encrypting audio logs delivered to an S3 bucket. The key does not apply to CloudWatch Logs and is optional for S3 buckets.
-- * 'logType' - The type of logging to enable. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs should be delivered.
mkLogSettingsRequest ::
  -- | 'logType'
  LogType ->
  -- | 'destination'
  Destination ->
  -- | 'resourceARN'
  Lude.Text ->
  LogSettingsRequest
mkLogSettingsRequest pLogType_ pDestination_ pResourceARN_ =
  LogSettingsRequest'
    { kmsKeyARN = Lude.Nothing,
      logType = pLogType_,
      destination = pDestination_,
      resourceARN = pResourceARN_
    }

-- | The Amazon Resource Name (ARN) of the AWS KMS customer managed key for encrypting audio logs delivered to an S3 bucket. The key does not apply to CloudWatch Logs and is optional for S3 buckets.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrKmsKeyARN :: Lens.Lens' LogSettingsRequest (Lude.Maybe Lude.Text)
lsrKmsKeyARN = Lens.lens (kmsKeyARN :: LogSettingsRequest -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: LogSettingsRequest)
{-# DEPRECATED lsrKmsKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | The type of logging to enable. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
--
-- /Note:/ Consider using 'logType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrLogType :: Lens.Lens' LogSettingsRequest LogType
lsrLogType = Lens.lens (logType :: LogSettingsRequest -> LogType) (\s a -> s {logType = a} :: LogSettingsRequest)
{-# DEPRECATED lsrLogType "Use generic-lens or generic-optics with 'logType' instead." #-}

-- | Where the logs will be delivered. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrDestination :: Lens.Lens' LogSettingsRequest Destination
lsrDestination = Lens.lens (destination :: LogSettingsRequest -> Destination) (\s a -> s {destination = a} :: LogSettingsRequest)
{-# DEPRECATED lsrDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs should be delivered.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrResourceARN :: Lens.Lens' LogSettingsRequest Lude.Text
lsrResourceARN = Lens.lens (resourceARN :: LogSettingsRequest -> Lude.Text) (\s a -> s {resourceARN = a} :: LogSettingsRequest)
{-# DEPRECATED lsrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.ToJSON LogSettingsRequest where
  toJSON LogSettingsRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("kmsKeyArn" Lude..=) Lude.<$> kmsKeyARN,
            Lude.Just ("logType" Lude..= logType),
            Lude.Just ("destination" Lude..= destination),
            Lude.Just ("resourceArn" Lude..= resourceARN)
          ]
      )
