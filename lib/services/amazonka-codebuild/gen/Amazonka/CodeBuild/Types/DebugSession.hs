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
-- Module      : Amazonka.CodeBuild.Types.DebugSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.DebugSession where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the debug session for a build. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
--
-- /See:/ 'newDebugSession' smart constructor.
data DebugSession = DebugSession'
  { -- | Specifies if session debugging is enabled for this build.
    sessionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Contains the identifier of the Session Manager session used for the
    -- build. To work with the paused build, you open this session to examine,
    -- control, and resume the build.
    sessionTarget :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DebugSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionEnabled', 'debugSession_sessionEnabled' - Specifies if session debugging is enabled for this build.
--
-- 'sessionTarget', 'debugSession_sessionTarget' - Contains the identifier of the Session Manager session used for the
-- build. To work with the paused build, you open this session to examine,
-- control, and resume the build.
newDebugSession ::
  DebugSession
newDebugSession =
  DebugSession'
    { sessionEnabled = Prelude.Nothing,
      sessionTarget = Prelude.Nothing
    }

-- | Specifies if session debugging is enabled for this build.
debugSession_sessionEnabled :: Lens.Lens' DebugSession (Prelude.Maybe Prelude.Bool)
debugSession_sessionEnabled = Lens.lens (\DebugSession' {sessionEnabled} -> sessionEnabled) (\s@DebugSession' {} a -> s {sessionEnabled = a} :: DebugSession)

-- | Contains the identifier of the Session Manager session used for the
-- build. To work with the paused build, you open this session to examine,
-- control, and resume the build.
debugSession_sessionTarget :: Lens.Lens' DebugSession (Prelude.Maybe Prelude.Text)
debugSession_sessionTarget = Lens.lens (\DebugSession' {sessionTarget} -> sessionTarget) (\s@DebugSession' {} a -> s {sessionTarget = a} :: DebugSession)

instance Core.FromJSON DebugSession where
  parseJSON =
    Core.withObject
      "DebugSession"
      ( \x ->
          DebugSession'
            Prelude.<$> (x Core..:? "sessionEnabled")
            Prelude.<*> (x Core..:? "sessionTarget")
      )

instance Prelude.Hashable DebugSession where
  hashWithSalt _salt DebugSession' {..} =
    _salt `Prelude.hashWithSalt` sessionEnabled
      `Prelude.hashWithSalt` sessionTarget

instance Prelude.NFData DebugSession where
  rnf DebugSession' {..} =
    Prelude.rnf sessionEnabled
      `Prelude.seq` Prelude.rnf sessionTarget
