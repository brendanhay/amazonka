-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CloudWatchOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CloudWatchOutputConfig
  ( CloudWatchOutputConfig (..),

    -- * Smart constructor
    mkCloudWatchOutputConfig,

    -- * Lenses
    cwocCloudWatchLogGroupName,
    cwocCloudWatchOutputEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration options for sending command output to CloudWatch Logs.
--
-- /See:/ 'mkCloudWatchOutputConfig' smart constructor.
data CloudWatchOutputConfig = CloudWatchOutputConfig'
  { cloudWatchLogGroupName ::
      Lude.Maybe Lude.Text,
    cloudWatchOutputEnabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchOutputConfig' with the minimum fields required to make a request.
--
-- * 'cloudWatchLogGroupName' - The name of the CloudWatch log group where you want to send command output. If you don't specify a group name, Systems Manager automatically creates a log group for you. The log group uses the following naming format: aws/ssm//SystemsManagerDocumentName/ .
-- * 'cloudWatchOutputEnabled' - Enables Systems Manager to send command output to CloudWatch Logs.
mkCloudWatchOutputConfig ::
  CloudWatchOutputConfig
mkCloudWatchOutputConfig =
  CloudWatchOutputConfig'
    { cloudWatchLogGroupName = Lude.Nothing,
      cloudWatchOutputEnabled = Lude.Nothing
    }

-- | The name of the CloudWatch log group where you want to send command output. If you don't specify a group name, Systems Manager automatically creates a log group for you. The log group uses the following naming format: aws/ssm//SystemsManagerDocumentName/ .
--
-- /Note:/ Consider using 'cloudWatchLogGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwocCloudWatchLogGroupName :: Lens.Lens' CloudWatchOutputConfig (Lude.Maybe Lude.Text)
cwocCloudWatchLogGroupName = Lens.lens (cloudWatchLogGroupName :: CloudWatchOutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogGroupName = a} :: CloudWatchOutputConfig)
{-# DEPRECATED cwocCloudWatchLogGroupName "Use generic-lens or generic-optics with 'cloudWatchLogGroupName' instead." #-}

-- | Enables Systems Manager to send command output to CloudWatch Logs.
--
-- /Note:/ Consider using 'cloudWatchOutputEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwocCloudWatchOutputEnabled :: Lens.Lens' CloudWatchOutputConfig (Lude.Maybe Lude.Bool)
cwocCloudWatchOutputEnabled = Lens.lens (cloudWatchOutputEnabled :: CloudWatchOutputConfig -> Lude.Maybe Lude.Bool) (\s a -> s {cloudWatchOutputEnabled = a} :: CloudWatchOutputConfig)
{-# DEPRECATED cwocCloudWatchOutputEnabled "Use generic-lens or generic-optics with 'cloudWatchOutputEnabled' instead." #-}

instance Lude.FromJSON CloudWatchOutputConfig where
  parseJSON =
    Lude.withObject
      "CloudWatchOutputConfig"
      ( \x ->
          CloudWatchOutputConfig'
            Lude.<$> (x Lude..:? "CloudWatchLogGroupName")
            Lude.<*> (x Lude..:? "CloudWatchOutputEnabled")
      )

instance Lude.ToJSON CloudWatchOutputConfig where
  toJSON CloudWatchOutputConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CloudWatchLogGroupName" Lude..=)
              Lude.<$> cloudWatchLogGroupName,
            ("CloudWatchOutputEnabled" Lude..=)
              Lude.<$> cloudWatchOutputEnabled
          ]
      )
