{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.EnableIoTLoggingParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.EnableIoTLoggingParams
  ( EnableIoTLoggingParams (..)
  -- * Smart constructor
  , mkEnableIoTLoggingParams
  -- * Lenses
  , eitlpRoleArnForLogging
  , eitlpLogLevel
  ) where

import qualified Network.AWS.IoT.Types.LogLevel as Types
import qualified Network.AWS.IoT.Types.RoleArnForLogging as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Parameters used when defining a mitigation action that enable AWS IoT logging.
--
-- /See:/ 'mkEnableIoTLoggingParams' smart constructor.
data EnableIoTLoggingParams = EnableIoTLoggingParams'
  { roleArnForLogging :: Types.RoleArnForLogging
    -- ^ The ARN of the IAM role used for logging.
  , logLevel :: Types.LogLevel
    -- ^ Specifies the types of information to be logged.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableIoTLoggingParams' value with any optional fields omitted.
mkEnableIoTLoggingParams
    :: Types.RoleArnForLogging -- ^ 'roleArnForLogging'
    -> Types.LogLevel -- ^ 'logLevel'
    -> EnableIoTLoggingParams
mkEnableIoTLoggingParams roleArnForLogging logLevel
  = EnableIoTLoggingParams'{roleArnForLogging, logLevel}

-- | The ARN of the IAM role used for logging.
--
-- /Note:/ Consider using 'roleArnForLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitlpRoleArnForLogging :: Lens.Lens' EnableIoTLoggingParams Types.RoleArnForLogging
eitlpRoleArnForLogging = Lens.field @"roleArnForLogging"
{-# INLINEABLE eitlpRoleArnForLogging #-}
{-# DEPRECATED roleArnForLogging "Use generic-lens or generic-optics with 'roleArnForLogging' instead"  #-}

-- | Specifies the types of information to be logged.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eitlpLogLevel :: Lens.Lens' EnableIoTLoggingParams Types.LogLevel
eitlpLogLevel = Lens.field @"logLevel"
{-# INLINEABLE eitlpLogLevel #-}
{-# DEPRECATED logLevel "Use generic-lens or generic-optics with 'logLevel' instead"  #-}

instance Core.FromJSON EnableIoTLoggingParams where
        toJSON EnableIoTLoggingParams{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArnForLogging" Core..= roleArnForLogging),
                  Core.Just ("logLevel" Core..= logLevel)])

instance Core.FromJSON EnableIoTLoggingParams where
        parseJSON
          = Core.withObject "EnableIoTLoggingParams" Core.$
              \ x ->
                EnableIoTLoggingParams' Core.<$>
                  (x Core..: "roleArnForLogging") Core.<*> x Core..: "logLevel"
