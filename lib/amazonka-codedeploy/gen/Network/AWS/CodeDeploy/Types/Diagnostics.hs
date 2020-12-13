{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.Diagnostics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.Diagnostics
  ( Diagnostics (..),

    -- * Smart constructor
    mkDiagnostics,

    -- * Lenses
    dLogTail,
    dErrorCode,
    dScriptName,
    dMessage,
  )
where

import Network.AWS.CodeDeploy.Types.LifecycleErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Diagnostic information about executable scripts that are part of a deployment.
--
-- /See:/ 'mkDiagnostics' smart constructor.
data Diagnostics = Diagnostics'
  { -- | The last portion of the diagnostic log.
    --
    -- If available, AWS CodeDeploy returns up to the last 4 KB of the diagnostic log.
    logTail :: Lude.Maybe Lude.Text,
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
    errorCode :: Lude.Maybe LifecycleErrorCode,
    -- | The name of the script.
    scriptName :: Lude.Maybe Lude.Text,
    -- | The message associated with the error.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Diagnostics' with the minimum fields required to make a request.
--
-- * 'logTail' - The last portion of the diagnostic log.
--
-- If available, AWS CodeDeploy returns up to the last 4 KB of the diagnostic log.
-- * 'errorCode' - The associated error code:
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
-- * 'scriptName' - The name of the script.
-- * 'message' - The message associated with the error.
mkDiagnostics ::
  Diagnostics
mkDiagnostics =
  Diagnostics'
    { logTail = Lude.Nothing,
      errorCode = Lude.Nothing,
      scriptName = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The last portion of the diagnostic log.
--
-- If available, AWS CodeDeploy returns up to the last 4 KB of the diagnostic log.
--
-- /Note:/ Consider using 'logTail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLogTail :: Lens.Lens' Diagnostics (Lude.Maybe Lude.Text)
dLogTail = Lens.lens (logTail :: Diagnostics -> Lude.Maybe Lude.Text) (\s a -> s {logTail = a} :: Diagnostics)
{-# DEPRECATED dLogTail "Use generic-lens or generic-optics with 'logTail' instead." #-}

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
dErrorCode :: Lens.Lens' Diagnostics (Lude.Maybe LifecycleErrorCode)
dErrorCode = Lens.lens (errorCode :: Diagnostics -> Lude.Maybe LifecycleErrorCode) (\s a -> s {errorCode = a} :: Diagnostics)
{-# DEPRECATED dErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The name of the script.
--
-- /Note:/ Consider using 'scriptName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScriptName :: Lens.Lens' Diagnostics (Lude.Maybe Lude.Text)
dScriptName = Lens.lens (scriptName :: Diagnostics -> Lude.Maybe Lude.Text) (\s a -> s {scriptName = a} :: Diagnostics)
{-# DEPRECATED dScriptName "Use generic-lens or generic-optics with 'scriptName' instead." #-}

-- | The message associated with the error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMessage :: Lens.Lens' Diagnostics (Lude.Maybe Lude.Text)
dMessage = Lens.lens (message :: Diagnostics -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Diagnostics)
{-# DEPRECATED dMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON Diagnostics where
  parseJSON =
    Lude.withObject
      "Diagnostics"
      ( \x ->
          Diagnostics'
            Lude.<$> (x Lude..:? "logTail")
            Lude.<*> (x Lude..:? "errorCode")
            Lude.<*> (x Lude..:? "scriptName")
            Lude.<*> (x Lude..:? "message")
      )
