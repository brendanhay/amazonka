{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.LogPublishingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.LogPublishingOption
  ( LogPublishingOption (..),

    -- * Smart constructor
    mkLogPublishingOption,

    -- * Lenses
    lpoEnabled,
    lpoCloudWatchLogsLogGroupARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Log Publishing option that is set for given domain.
--
-- Attributes and their details:
--     * CloudWatchLogsLogGroupArn: ARN of the Cloudwatch log group to which log needs to be published.
--
--     * Enabled: Whether the log publishing for given log type is enabled or not
--
--
--
-- /See:/ 'mkLogPublishingOption' smart constructor.
data LogPublishingOption = LogPublishingOption'
  { enabled ::
      Lude.Maybe Lude.Bool,
    cloudWatchLogsLogGroupARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogPublishingOption' with the minimum fields required to make a request.
--
-- * 'cloudWatchLogsLogGroupARN' - Undocumented field.
-- * 'enabled' - Specifies whether given log publishing option is enabled or not.
mkLogPublishingOption ::
  LogPublishingOption
mkLogPublishingOption =
  LogPublishingOption'
    { enabled = Lude.Nothing,
      cloudWatchLogsLogGroupARN = Lude.Nothing
    }

-- | Specifies whether given log publishing option is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpoEnabled :: Lens.Lens' LogPublishingOption (Lude.Maybe Lude.Bool)
lpoEnabled = Lens.lens (enabled :: LogPublishingOption -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: LogPublishingOption)
{-# DEPRECATED lpoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpoCloudWatchLogsLogGroupARN :: Lens.Lens' LogPublishingOption (Lude.Maybe Lude.Text)
lpoCloudWatchLogsLogGroupARN = Lens.lens (cloudWatchLogsLogGroupARN :: LogPublishingOption -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogsLogGroupARN = a} :: LogPublishingOption)
{-# DEPRECATED lpoCloudWatchLogsLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupARN' instead." #-}

instance Lude.FromJSON LogPublishingOption where
  parseJSON =
    Lude.withObject
      "LogPublishingOption"
      ( \x ->
          LogPublishingOption'
            Lude.<$> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "CloudWatchLogsLogGroupArn")
      )

instance Lude.ToJSON LogPublishingOption where
  toJSON LogPublishingOption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            ("CloudWatchLogsLogGroupArn" Lude..=)
              Lude.<$> cloudWatchLogsLogGroupARN
          ]
      )
