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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Session where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details for the execute command session.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | The ID of the execute command session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | A URL to the managed agent on the container that the SSM Session Manager
    -- client uses to send commands and receive output from the container.
    streamUrl :: Prelude.Maybe Prelude.Text,
    -- | An encrypted token value containing session and caller information.
    -- It\'s used to authenticate the connection to the container.
    tokenValue :: Prelude.Maybe (Data.Sensitive Prelude.Text)
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
-- 'sessionId', 'session_sessionId' - The ID of the execute command session.
--
-- 'streamUrl', 'session_streamUrl' - A URL to the managed agent on the container that the SSM Session Manager
-- client uses to send commands and receive output from the container.
--
-- 'tokenValue', 'session_tokenValue' - An encrypted token value containing session and caller information.
-- It\'s used to authenticate the connection to the container.
newSession ::
  Session
newSession =
  Session'
    { sessionId = Prelude.Nothing,
      streamUrl = Prelude.Nothing,
      tokenValue = Prelude.Nothing
    }

-- | The ID of the execute command session.
session_sessionId :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_sessionId = Lens.lens (\Session' {sessionId} -> sessionId) (\s@Session' {} a -> s {sessionId = a} :: Session)

-- | A URL to the managed agent on the container that the SSM Session Manager
-- client uses to send commands and receive output from the container.
session_streamUrl :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_streamUrl = Lens.lens (\Session' {streamUrl} -> streamUrl) (\s@Session' {} a -> s {streamUrl = a} :: Session)

-- | An encrypted token value containing session and caller information.
-- It\'s used to authenticate the connection to the container.
session_tokenValue :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_tokenValue = Lens.lens (\Session' {tokenValue} -> tokenValue) (\s@Session' {} a -> s {tokenValue = a} :: Session) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON Session where
  parseJSON =
    Data.withObject
      "Session"
      ( \x ->
          Session'
            Prelude.<$> (x Data..:? "sessionId")
            Prelude.<*> (x Data..:? "streamUrl")
            Prelude.<*> (x Data..:? "tokenValue")
      )

instance Prelude.Hashable Session where
  hashWithSalt _salt Session' {..} =
    _salt `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` streamUrl
      `Prelude.hashWithSalt` tokenValue

instance Prelude.NFData Session where
  rnf Session' {..} =
    Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf streamUrl
      `Prelude.seq` Prelude.rnf tokenValue
