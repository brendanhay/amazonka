-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LoggingOptions
  ( LoggingOptions (..),

    -- * Smart constructor
    mkLoggingOptions,

    -- * Lenses
    loRoleARN,
    loLevel,
    loEnabled,
  )
where

import Network.AWS.IoTAnalytics.Types.LoggingLevel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about logging options.
--
-- /See:/ 'mkLoggingOptions' smart constructor.
data LoggingOptions = LoggingOptions'
  { roleARN :: Lude.Text,
    level :: LoggingLevel,
    enabled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoggingOptions' with the minimum fields required to make a request.
--
-- * 'enabled' - If true, logging is enabled for AWS IoT Analytics.
-- * 'level' - The logging level. Currently, only ERROR is supported.
-- * 'roleARN' - The ARN of the role that grants permission to AWS IoT Analytics to perform logging.
mkLoggingOptions ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'level'
  LoggingLevel ->
  -- | 'enabled'
  Lude.Bool ->
  LoggingOptions
mkLoggingOptions pRoleARN_ pLevel_ pEnabled_ =
  LoggingOptions'
    { roleARN = pRoleARN_,
      level = pLevel_,
      enabled = pEnabled_
    }

-- | The ARN of the role that grants permission to AWS IoT Analytics to perform logging.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loRoleARN :: Lens.Lens' LoggingOptions Lude.Text
loRoleARN = Lens.lens (roleARN :: LoggingOptions -> Lude.Text) (\s a -> s {roleARN = a} :: LoggingOptions)
{-# DEPRECATED loRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The logging level. Currently, only ERROR is supported.
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loLevel :: Lens.Lens' LoggingOptions LoggingLevel
loLevel = Lens.lens (level :: LoggingOptions -> LoggingLevel) (\s a -> s {level = a} :: LoggingOptions)
{-# DEPRECATED loLevel "Use generic-lens or generic-optics with 'level' instead." #-}

-- | If true, logging is enabled for AWS IoT Analytics.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loEnabled :: Lens.Lens' LoggingOptions Lude.Bool
loEnabled = Lens.lens (enabled :: LoggingOptions -> Lude.Bool) (\s a -> s {enabled = a} :: LoggingOptions)
{-# DEPRECATED loEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromJSON LoggingOptions where
  parseJSON =
    Lude.withObject
      "LoggingOptions"
      ( \x ->
          LoggingOptions'
            Lude.<$> (x Lude..: "roleArn")
            Lude.<*> (x Lude..: "level")
            Lude.<*> (x Lude..: "enabled")
      )

instance Lude.ToJSON LoggingOptions where
  toJSON LoggingOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("roleArn" Lude..= roleARN),
            Lude.Just ("level" Lude..= level),
            Lude.Just ("enabled" Lude..= enabled)
          ]
      )
