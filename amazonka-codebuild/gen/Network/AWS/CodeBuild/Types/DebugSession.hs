{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.Types.DebugSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.DebugSession where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the debug session for a build. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
--
-- /See:/ 'newDebugSession' smart constructor.
data DebugSession = DebugSession'
  { -- | Contains the identifier of the Session Manager session used for the
    -- build. To work with the paused build, you open this session to examine,
    -- control, and resume the build.
    sessionTarget :: Prelude.Maybe Prelude.Text,
    -- | Specifies if session debugging is enabled for this build.
    sessionEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DebugSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionTarget', 'debugSession_sessionTarget' - Contains the identifier of the Session Manager session used for the
-- build. To work with the paused build, you open this session to examine,
-- control, and resume the build.
--
-- 'sessionEnabled', 'debugSession_sessionEnabled' - Specifies if session debugging is enabled for this build.
newDebugSession ::
  DebugSession
newDebugSession =
  DebugSession'
    { sessionTarget = Prelude.Nothing,
      sessionEnabled = Prelude.Nothing
    }

-- | Contains the identifier of the Session Manager session used for the
-- build. To work with the paused build, you open this session to examine,
-- control, and resume the build.
debugSession_sessionTarget :: Lens.Lens' DebugSession (Prelude.Maybe Prelude.Text)
debugSession_sessionTarget = Lens.lens (\DebugSession' {sessionTarget} -> sessionTarget) (\s@DebugSession' {} a -> s {sessionTarget = a} :: DebugSession)

-- | Specifies if session debugging is enabled for this build.
debugSession_sessionEnabled :: Lens.Lens' DebugSession (Prelude.Maybe Prelude.Bool)
debugSession_sessionEnabled = Lens.lens (\DebugSession' {sessionEnabled} -> sessionEnabled) (\s@DebugSession' {} a -> s {sessionEnabled = a} :: DebugSession)

instance Prelude.FromJSON DebugSession where
  parseJSON =
    Prelude.withObject
      "DebugSession"
      ( \x ->
          DebugSession'
            Prelude.<$> (x Prelude..:? "sessionTarget")
            Prelude.<*> (x Prelude..:? "sessionEnabled")
      )

instance Prelude.Hashable DebugSession

instance Prelude.NFData DebugSession
