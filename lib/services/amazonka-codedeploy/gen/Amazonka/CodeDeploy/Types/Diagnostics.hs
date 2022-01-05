{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeDeploy.Types.Diagnostics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.Diagnostics where

import Amazonka.CodeDeploy.Types.LifecycleErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Diagnostic information about executable scripts that are part of a
-- deployment.
--
-- /See:/ 'newDiagnostics' smart constructor.
data Diagnostics = Diagnostics'
  { -- | The last portion of the diagnostic log.
    --
    -- If available, AWS CodeDeploy returns up to the last 4 KB of the
    -- diagnostic log.
    logTail :: Prelude.Maybe Prelude.Text,
    -- | The associated error code:
    --
    -- -   Success: The specified script ran.
    --
    -- -   ScriptMissing: The specified script was not found in the specified
    --     location.
    --
    -- -   ScriptNotExecutable: The specified script is not a recognized
    --     executable file type.
    --
    -- -   ScriptTimedOut: The specified script did not finish running in the
    --     specified time period.
    --
    -- -   ScriptFailed: The specified script failed to run as expected.
    --
    -- -   UnknownError: The specified script did not run for an unknown
    --     reason.
    errorCode :: Prelude.Maybe LifecycleErrorCode,
    -- | The name of the script.
    scriptName :: Prelude.Maybe Prelude.Text,
    -- | The message associated with the error.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Diagnostics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logTail', 'diagnostics_logTail' - The last portion of the diagnostic log.
--
-- If available, AWS CodeDeploy returns up to the last 4 KB of the
-- diagnostic log.
--
-- 'errorCode', 'diagnostics_errorCode' - The associated error code:
--
-- -   Success: The specified script ran.
--
-- -   ScriptMissing: The specified script was not found in the specified
--     location.
--
-- -   ScriptNotExecutable: The specified script is not a recognized
--     executable file type.
--
-- -   ScriptTimedOut: The specified script did not finish running in the
--     specified time period.
--
-- -   ScriptFailed: The specified script failed to run as expected.
--
-- -   UnknownError: The specified script did not run for an unknown
--     reason.
--
-- 'scriptName', 'diagnostics_scriptName' - The name of the script.
--
-- 'message', 'diagnostics_message' - The message associated with the error.
newDiagnostics ::
  Diagnostics
newDiagnostics =
  Diagnostics'
    { logTail = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      scriptName = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The last portion of the diagnostic log.
--
-- If available, AWS CodeDeploy returns up to the last 4 KB of the
-- diagnostic log.
diagnostics_logTail :: Lens.Lens' Diagnostics (Prelude.Maybe Prelude.Text)
diagnostics_logTail = Lens.lens (\Diagnostics' {logTail} -> logTail) (\s@Diagnostics' {} a -> s {logTail = a} :: Diagnostics)

-- | The associated error code:
--
-- -   Success: The specified script ran.
--
-- -   ScriptMissing: The specified script was not found in the specified
--     location.
--
-- -   ScriptNotExecutable: The specified script is not a recognized
--     executable file type.
--
-- -   ScriptTimedOut: The specified script did not finish running in the
--     specified time period.
--
-- -   ScriptFailed: The specified script failed to run as expected.
--
-- -   UnknownError: The specified script did not run for an unknown
--     reason.
diagnostics_errorCode :: Lens.Lens' Diagnostics (Prelude.Maybe LifecycleErrorCode)
diagnostics_errorCode = Lens.lens (\Diagnostics' {errorCode} -> errorCode) (\s@Diagnostics' {} a -> s {errorCode = a} :: Diagnostics)

-- | The name of the script.
diagnostics_scriptName :: Lens.Lens' Diagnostics (Prelude.Maybe Prelude.Text)
diagnostics_scriptName = Lens.lens (\Diagnostics' {scriptName} -> scriptName) (\s@Diagnostics' {} a -> s {scriptName = a} :: Diagnostics)

-- | The message associated with the error.
diagnostics_message :: Lens.Lens' Diagnostics (Prelude.Maybe Prelude.Text)
diagnostics_message = Lens.lens (\Diagnostics' {message} -> message) (\s@Diagnostics' {} a -> s {message = a} :: Diagnostics)

instance Core.FromJSON Diagnostics where
  parseJSON =
    Core.withObject
      "Diagnostics"
      ( \x ->
          Diagnostics'
            Prelude.<$> (x Core..:? "logTail")
            Prelude.<*> (x Core..:? "errorCode")
            Prelude.<*> (x Core..:? "scriptName")
            Prelude.<*> (x Core..:? "message")
      )

instance Prelude.Hashable Diagnostics where
  hashWithSalt _salt Diagnostics' {..} =
    _salt `Prelude.hashWithSalt` logTail
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` scriptName
      `Prelude.hashWithSalt` message

instance Prelude.NFData Diagnostics where
  rnf Diagnostics' {..} =
    Prelude.rnf logTail
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf scriptName
      `Prelude.seq` Prelude.rnf message
