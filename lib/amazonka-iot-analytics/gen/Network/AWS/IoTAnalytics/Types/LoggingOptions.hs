{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    loRoleArn,
    loLevel,
    loEnabled,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.LoggingLevel as Types
import qualified Network.AWS.IoTAnalytics.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about logging options.
--
-- /See:/ 'mkLoggingOptions' smart constructor.
data LoggingOptions = LoggingOptions'
  { -- | The ARN of the role that grants permission to AWS IoT Analytics to perform logging.
    roleArn :: Types.RoleArn,
    -- | The logging level. Currently, only ERROR is supported.
    level :: Types.LoggingLevel,
    -- | If true, logging is enabled for AWS IoT Analytics.
    enabled :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoggingOptions' value with any optional fields omitted.
mkLoggingOptions ::
  -- | 'roleArn'
  Types.RoleArn ->
  -- | 'level'
  Types.LoggingLevel ->
  -- | 'enabled'
  Core.Bool ->
  LoggingOptions
mkLoggingOptions roleArn level enabled =
  LoggingOptions' {roleArn, level, enabled}

-- | The ARN of the role that grants permission to AWS IoT Analytics to perform logging.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loRoleArn :: Lens.Lens' LoggingOptions Types.RoleArn
loRoleArn = Lens.field @"roleArn"
{-# DEPRECATED loRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The logging level. Currently, only ERROR is supported.
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loLevel :: Lens.Lens' LoggingOptions Types.LoggingLevel
loLevel = Lens.field @"level"
{-# DEPRECATED loLevel "Use generic-lens or generic-optics with 'level' instead." #-}

-- | If true, logging is enabled for AWS IoT Analytics.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loEnabled :: Lens.Lens' LoggingOptions Core.Bool
loEnabled = Lens.field @"enabled"
{-# DEPRECATED loEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromJSON LoggingOptions where
  toJSON LoggingOptions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("level" Core..= level),
            Core.Just ("enabled" Core..= enabled)
          ]
      )

instance Core.FromJSON LoggingOptions where
  parseJSON =
    Core.withObject "LoggingOptions" Core.$
      \x ->
        LoggingOptions'
          Core.<$> (x Core..: "roleArn")
          Core.<*> (x Core..: "level")
          Core.<*> (x Core..: "enabled")
