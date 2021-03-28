{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LoggingOptionsPayload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.LoggingOptionsPayload
  ( LoggingOptionsPayload (..)
  -- * Smart constructor
  , mkLoggingOptionsPayload
  -- * Lenses
  , lopRoleArn
  , lopLogLevel
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.LogLevel as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the logging options payload.
--
-- /See:/ 'mkLoggingOptionsPayload' smart constructor.
data LoggingOptionsPayload = LoggingOptionsPayload'
  { roleArn :: Types.AwsArn
    -- ^ The ARN of the IAM role that grants access.
  , logLevel :: Core.Maybe Types.LogLevel
    -- ^ The log level.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoggingOptionsPayload' value with any optional fields omitted.
mkLoggingOptionsPayload
    :: Types.AwsArn -- ^ 'roleArn'
    -> LoggingOptionsPayload
mkLoggingOptionsPayload roleArn
  = LoggingOptionsPayload'{roleArn, logLevel = Core.Nothing}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopRoleArn :: Lens.Lens' LoggingOptionsPayload Types.AwsArn
lopRoleArn = Lens.field @"roleArn"
{-# INLINEABLE lopRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The log level.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopLogLevel :: Lens.Lens' LoggingOptionsPayload (Core.Maybe Types.LogLevel)
lopLogLevel = Lens.field @"logLevel"
{-# INLINEABLE lopLogLevel #-}
{-# DEPRECATED logLevel "Use generic-lens or generic-optics with 'logLevel' instead"  #-}

instance Core.FromJSON LoggingOptionsPayload where
        toJSON LoggingOptionsPayload{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArn" Core..= roleArn),
                  ("logLevel" Core..=) Core.<$> logLevel])
