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
-- Module      : Amazonka.ChimeSdkVoice.Types.Credential
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.Credential where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newCredential' smart constructor.
data Credential = Credential'
  { password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    username :: Prelude.Maybe (Data.Sensitive Prelude.Text)
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
-- 'password', 'credential_password' - Undocumented member.
--
-- 'username', 'credential_username' - Undocumented member.
newCredential ::
  Credential
newCredential =
  Credential'
    { password = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | Undocumented member.
credential_password :: Lens.Lens' Credential (Prelude.Maybe Prelude.Text)
credential_password = Lens.lens (\Credential' {password} -> password) (\s@Credential' {} a -> s {password = a} :: Credential) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
credential_username :: Lens.Lens' Credential (Prelude.Maybe Prelude.Text)
credential_username = Lens.lens (\Credential' {username} -> username) (\s@Credential' {} a -> s {username = a} :: Credential) Prelude.. Lens.mapping Data._Sensitive

instance Prelude.Hashable Credential where
  hashWithSalt _salt Credential' {..} =
    _salt
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` username

instance Prelude.NFData Credential where
  rnf Credential' {..} =
    Prelude.rnf password `Prelude.seq`
      Prelude.rnf username

instance Data.ToJSON Credential where
  toJSON Credential' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Password" Data..=) Prelude.<$> password,
            ("Username" Data..=) Prelude.<$> username
          ]
      )
