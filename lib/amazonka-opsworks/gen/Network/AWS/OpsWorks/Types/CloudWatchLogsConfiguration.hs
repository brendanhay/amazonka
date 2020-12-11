-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
  ( CloudWatchLogsConfiguration (..),

    -- * Smart constructor
    mkCloudWatchLogsConfiguration,

    -- * Lenses
    cwlcEnabled,
    cwlcLogStreams,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream
import qualified Network.AWS.Prelude as Lude

-- | Describes the Amazon CloudWatch logs configuration for a layer.
--
-- /See:/ 'mkCloudWatchLogsConfiguration' smart constructor.
data CloudWatchLogsConfiguration = CloudWatchLogsConfiguration'
  { enabled ::
      Lude.Maybe Lude.Bool,
    logStreams ::
      Lude.Maybe
        [CloudWatchLogsLogStream]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchLogsConfiguration' with the minimum fields required to make a request.
--
-- * 'enabled' - Whether CloudWatch Logs is enabled for a layer.
-- * 'logStreams' - A list of configuration options for CloudWatch Logs.
mkCloudWatchLogsConfiguration ::
  CloudWatchLogsConfiguration
mkCloudWatchLogsConfiguration =
  CloudWatchLogsConfiguration'
    { enabled = Lude.Nothing,
      logStreams = Lude.Nothing
    }

-- | Whether CloudWatch Logs is enabled for a layer.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlcEnabled :: Lens.Lens' CloudWatchLogsConfiguration (Lude.Maybe Lude.Bool)
cwlcEnabled = Lens.lens (enabled :: CloudWatchLogsConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: CloudWatchLogsConfiguration)
{-# DEPRECATED cwlcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | A list of configuration options for CloudWatch Logs.
--
-- /Note:/ Consider using 'logStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlcLogStreams :: Lens.Lens' CloudWatchLogsConfiguration (Lude.Maybe [CloudWatchLogsLogStream])
cwlcLogStreams = Lens.lens (logStreams :: CloudWatchLogsConfiguration -> Lude.Maybe [CloudWatchLogsLogStream]) (\s a -> s {logStreams = a} :: CloudWatchLogsConfiguration)
{-# DEPRECATED cwlcLogStreams "Use generic-lens or generic-optics with 'logStreams' instead." #-}

instance Lude.FromJSON CloudWatchLogsConfiguration where
  parseJSON =
    Lude.withObject
      "CloudWatchLogsConfiguration"
      ( \x ->
          CloudWatchLogsConfiguration'
            Lude.<$> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "LogStreams" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON CloudWatchLogsConfiguration where
  toJSON CloudWatchLogsConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            ("LogStreams" Lude..=) Lude.<$> logStreams
          ]
      )
