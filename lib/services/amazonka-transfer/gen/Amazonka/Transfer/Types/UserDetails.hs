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
-- Module      : Amazonka.Transfer.Types.UserDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.UserDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the user name, server ID, and session ID for a workflow.
--
-- /See:/ 'newUserDetails' smart constructor.
data UserDetails = UserDetails'
  { -- | The system-assigned unique identifier for a session that corresponds to
    -- the workflow.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | A unique string that identifies a user account associated with a server.
    userName :: Prelude.Text,
    -- | The system-assigned unique identifier for a Transfer server instance.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'userDetails_sessionId' - The system-assigned unique identifier for a session that corresponds to
-- the workflow.
--
-- 'userName', 'userDetails_userName' - A unique string that identifies a user account associated with a server.
--
-- 'serverId', 'userDetails_serverId' - The system-assigned unique identifier for a Transfer server instance.
newUserDetails ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'serverId'
  Prelude.Text ->
  UserDetails
newUserDetails pUserName_ pServerId_ =
  UserDetails'
    { sessionId = Prelude.Nothing,
      userName = pUserName_,
      serverId = pServerId_
    }

-- | The system-assigned unique identifier for a session that corresponds to
-- the workflow.
userDetails_sessionId :: Lens.Lens' UserDetails (Prelude.Maybe Prelude.Text)
userDetails_sessionId = Lens.lens (\UserDetails' {sessionId} -> sessionId) (\s@UserDetails' {} a -> s {sessionId = a} :: UserDetails)

-- | A unique string that identifies a user account associated with a server.
userDetails_userName :: Lens.Lens' UserDetails Prelude.Text
userDetails_userName = Lens.lens (\UserDetails' {userName} -> userName) (\s@UserDetails' {} a -> s {userName = a} :: UserDetails)

-- | The system-assigned unique identifier for a Transfer server instance.
userDetails_serverId :: Lens.Lens' UserDetails Prelude.Text
userDetails_serverId = Lens.lens (\UserDetails' {serverId} -> serverId) (\s@UserDetails' {} a -> s {serverId = a} :: UserDetails)

instance Core.FromJSON UserDetails where
  parseJSON =
    Core.withObject
      "UserDetails"
      ( \x ->
          UserDetails'
            Prelude.<$> (x Core..:? "SessionId")
            Prelude.<*> (x Core..: "UserName")
            Prelude.<*> (x Core..: "ServerId")
      )

instance Prelude.Hashable UserDetails where
  hashWithSalt _salt UserDetails' {..} =
    _salt `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` serverId

instance Prelude.NFData UserDetails where
  rnf UserDetails' {..} =
    Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf serverId
