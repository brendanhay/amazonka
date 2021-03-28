{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.Diagnostics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.Diagnostics
  ( Diagnostics (..)
  -- * Smart constructor
  , mkDiagnostics
  -- * Lenses
  , dErrorCode
  , dLogTail
  , dMessage
  , dScriptName
  ) where

import qualified Network.AWS.CodeDeploy.Types.LifecycleErrorCode as Types
import qualified Network.AWS.CodeDeploy.Types.LifecycleMessage as Types
import qualified Network.AWS.CodeDeploy.Types.LogTail as Types
import qualified Network.AWS.CodeDeploy.Types.ScriptName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Diagnostic information about executable scripts that are part of a deployment.
--
-- /See:/ 'mkDiagnostics' smart constructor.
data Diagnostics = Diagnostics'
  { errorCode :: Core.Maybe Types.LifecycleErrorCode
    -- ^ The associated error code:
--
--
--     * Success: The specified script ran.
--
--
--     * ScriptMissing: The specified script was not found in the specified location.
--
--
--     * ScriptNotExecutable: The specified script is not a recognized executable file type.
--
--
--     * ScriptTimedOut: The specified script did not finish running in the specified time period.
--
--
--     * ScriptFailed: The specified script failed to run as expected.
--
--
--     * UnknownError: The specified script did not run for an unknown reason.
--
--
  , logTail :: Core.Maybe Types.LogTail
    -- ^ The last portion of the diagnostic log.
--
-- If available, AWS CodeDeploy returns up to the last 4 KB of the diagnostic log.
  , message :: Core.Maybe Types.LifecycleMessage
    -- ^ The message associated with the error.
  , scriptName :: Core.Maybe Types.ScriptName
    -- ^ The name of the script.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Diagnostics' value with any optional fields omitted.
mkDiagnostics
    :: Diagnostics
mkDiagnostics
  = Diagnostics'{errorCode = Core.Nothing, logTail = Core.Nothing,
                 message = Core.Nothing, scriptName = Core.Nothing}

-- | The associated error code:
--
--
--     * Success: The specified script ran.
--
--
--     * ScriptMissing: The specified script was not found in the specified location.
--
--
--     * ScriptNotExecutable: The specified script is not a recognized executable file type.
--
--
--     * ScriptTimedOut: The specified script did not finish running in the specified time period.
--
--
--     * ScriptFailed: The specified script failed to run as expected.
--
--
--     * UnknownError: The specified script did not run for an unknown reason.
--
--
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dErrorCode :: Lens.Lens' Diagnostics (Core.Maybe Types.LifecycleErrorCode)
dErrorCode = Lens.field @"errorCode"
{-# INLINEABLE dErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The last portion of the diagnostic log.
--
-- If available, AWS CodeDeploy returns up to the last 4 KB of the diagnostic log.
--
-- /Note:/ Consider using 'logTail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLogTail :: Lens.Lens' Diagnostics (Core.Maybe Types.LogTail)
dLogTail = Lens.field @"logTail"
{-# INLINEABLE dLogTail #-}
{-# DEPRECATED logTail "Use generic-lens or generic-optics with 'logTail' instead"  #-}

-- | The message associated with the error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMessage :: Lens.Lens' Diagnostics (Core.Maybe Types.LifecycleMessage)
dMessage = Lens.field @"message"
{-# INLINEABLE dMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The name of the script.
--
-- /Note:/ Consider using 'scriptName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScriptName :: Lens.Lens' Diagnostics (Core.Maybe Types.ScriptName)
dScriptName = Lens.field @"scriptName"
{-# INLINEABLE dScriptName #-}
{-# DEPRECATED scriptName "Use generic-lens or generic-optics with 'scriptName' instead"  #-}

instance Core.FromJSON Diagnostics where
        parseJSON
          = Core.withObject "Diagnostics" Core.$
              \ x ->
                Diagnostics' Core.<$>
                  (x Core..:? "errorCode") Core.<*> x Core..:? "logTail" Core.<*>
                    x Core..:? "message"
                    Core.<*> x Core..:? "scriptName"
