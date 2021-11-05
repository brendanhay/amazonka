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
-- Module      : Amazonka.ECS.Types.Session
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Session where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of the execute command session.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | A URL back to managed agent on the container that the SSM Session
    -- Manager client uses to send commands and receive output from the
    -- container.
    streamUrl :: Prelude.Maybe Prelude.Text,
    -- | An encrypted token value containing session and caller information. Used
    -- to authenticate the connection to the container.
    tokenValue :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the execute command session.
    sessionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Session' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamUrl', 'session_streamUrl' - A URL back to managed agent on the container that the SSM Session
-- Manager client uses to send commands and receive output from the
-- container.
--
-- 'tokenValue', 'session_tokenValue' - An encrypted token value containing session and caller information. Used
-- to authenticate the connection to the container.
--
-- 'sessionId', 'session_sessionId' - The ID of the execute command session.
newSession ::
  Session
newSession =
  Session'
    { streamUrl = Prelude.Nothing,
      tokenValue = Prelude.Nothing,
      sessionId = Prelude.Nothing
    }

-- | A URL back to managed agent on the container that the SSM Session
-- Manager client uses to send commands and receive output from the
-- container.
session_streamUrl :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_streamUrl = Lens.lens (\Session' {streamUrl} -> streamUrl) (\s@Session' {} a -> s {streamUrl = a} :: Session)

-- | An encrypted token value containing session and caller information. Used
-- to authenticate the connection to the container.
session_tokenValue :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_tokenValue = Lens.lens (\Session' {tokenValue} -> tokenValue) (\s@Session' {} a -> s {tokenValue = a} :: Session) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the execute command session.
session_sessionId :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_sessionId = Lens.lens (\Session' {sessionId} -> sessionId) (\s@Session' {} a -> s {sessionId = a} :: Session)

instance Core.FromJSON Session where
  parseJSON =
    Core.withObject
      "Session"
      ( \x ->
          Session'
            Prelude.<$> (x Core..:? "streamUrl")
            Prelude.<*> (x Core..:? "tokenValue")
            Prelude.<*> (x Core..:? "sessionId")
      )

instance Prelude.Hashable Session

instance Prelude.NFData Session
