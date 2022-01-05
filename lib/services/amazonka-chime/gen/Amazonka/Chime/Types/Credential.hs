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
-- Module      : Amazonka.Chime.Types.Credential
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Credential where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The SIP credentials used to authenticate requests to your Amazon Chime
-- Voice Connector.
--
-- /See:/ 'newCredential' smart constructor.
data Credential = Credential'
  { -- | The RFC2617 compliant user name associated with the SIP credentials, in
    -- US-ASCII format.
    username :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The RFC2617 compliant password associated with the SIP credentials, in
    -- US-ASCII format.
    password :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Credential' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'credential_username' - The RFC2617 compliant user name associated with the SIP credentials, in
-- US-ASCII format.
--
-- 'password', 'credential_password' - The RFC2617 compliant password associated with the SIP credentials, in
-- US-ASCII format.
newCredential ::
  Credential
newCredential =
  Credential'
    { username = Prelude.Nothing,
      password = Prelude.Nothing
    }

-- | The RFC2617 compliant user name associated with the SIP credentials, in
-- US-ASCII format.
credential_username :: Lens.Lens' Credential (Prelude.Maybe Prelude.Text)
credential_username = Lens.lens (\Credential' {username} -> username) (\s@Credential' {} a -> s {username = a} :: Credential) Prelude.. Lens.mapping Core._Sensitive

-- | The RFC2617 compliant password associated with the SIP credentials, in
-- US-ASCII format.
credential_password :: Lens.Lens' Credential (Prelude.Maybe Prelude.Text)
credential_password = Lens.lens (\Credential' {password} -> password) (\s@Credential' {} a -> s {password = a} :: Credential) Prelude.. Lens.mapping Core._Sensitive

instance Prelude.Hashable Credential where
  hashWithSalt _salt Credential' {..} =
    _salt `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` password

instance Prelude.NFData Credential where
  rnf Credential' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance Core.ToJSON Credential where
  toJSON Credential' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Username" Core..=) Prelude.<$> username,
            ("Password" Core..=) Prelude.<$> password
          ]
      )
