{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.CloudWatchLoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.CloudWatchLoggingOptions
  ( CloudWatchLoggingOptions (..),

    -- * Smart constructor
    mkCloudWatchLoggingOptions,

    -- * Lenses
    cwloEnabled,
    cwloLogGroupName,
    cwloLogStreamName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Amazon CloudWatch logging options for your delivery stream.
--
-- /See:/ 'mkCloudWatchLoggingOptions' smart constructor.
data CloudWatchLoggingOptions = CloudWatchLoggingOptions'
  { enabled ::
      Lude.Maybe Lude.Bool,
    logGroupName :: Lude.Maybe Lude.Text,
    logStreamName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchLoggingOptions' with the minimum fields required to make a request.
--
-- * 'enabled' - Enables or disables CloudWatch logging.
-- * 'logGroupName' - The CloudWatch group name for logging. This value is required if CloudWatch logging is enabled.
-- * 'logStreamName' - The CloudWatch log stream name for logging. This value is required if CloudWatch logging is enabled.
mkCloudWatchLoggingOptions ::
  CloudWatchLoggingOptions
mkCloudWatchLoggingOptions =
  CloudWatchLoggingOptions'
    { enabled = Lude.Nothing,
      logGroupName = Lude.Nothing,
      logStreamName = Lude.Nothing
    }

-- | Enables or disables CloudWatch logging.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwloEnabled :: Lens.Lens' CloudWatchLoggingOptions (Lude.Maybe Lude.Bool)
cwloEnabled = Lens.lens (enabled :: CloudWatchLoggingOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: CloudWatchLoggingOptions)
{-# DEPRECATED cwloEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The CloudWatch group name for logging. This value is required if CloudWatch logging is enabled.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwloLogGroupName :: Lens.Lens' CloudWatchLoggingOptions (Lude.Maybe Lude.Text)
cwloLogGroupName = Lens.lens (logGroupName :: CloudWatchLoggingOptions -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: CloudWatchLoggingOptions)
{-# DEPRECATED cwloLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The CloudWatch log stream name for logging. This value is required if CloudWatch logging is enabled.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwloLogStreamName :: Lens.Lens' CloudWatchLoggingOptions (Lude.Maybe Lude.Text)
cwloLogStreamName = Lens.lens (logStreamName :: CloudWatchLoggingOptions -> Lude.Maybe Lude.Text) (\s a -> s {logStreamName = a} :: CloudWatchLoggingOptions)
{-# DEPRECATED cwloLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

instance Lude.FromJSON CloudWatchLoggingOptions where
  parseJSON =
    Lude.withObject
      "CloudWatchLoggingOptions"
      ( \x ->
          CloudWatchLoggingOptions'
            Lude.<$> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "LogGroupName")
            Lude.<*> (x Lude..:? "LogStreamName")
      )

instance Lude.ToJSON CloudWatchLoggingOptions where
  toJSON CloudWatchLoggingOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            ("LogGroupName" Lude..=) Lude.<$> logGroupName,
            ("LogStreamName" Lude..=) Lude.<$> logStreamName
          ]
      )
