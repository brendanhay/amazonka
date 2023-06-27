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
-- Module      : Amazonka.DataSync.Types.Credentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.Credentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The credentials that provide DataSync Discovery read access to your
-- on-premises storage system\'s management interface.
--
-- DataSync Discovery stores these credentials in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>.
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-configure-storage.html Accessing your on-premises storage system>.
--
-- /See:/ 'newCredentials' smart constructor.
data Credentials = Credentials'
  { -- | Specifies the user name for your storage system\'s management interface.
    username :: Data.Sensitive Prelude.Text,
    -- | Specifies the password for your storage system\'s management interface.
    password :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Credentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'credentials_username' - Specifies the user name for your storage system\'s management interface.
--
-- 'password', 'credentials_password' - Specifies the password for your storage system\'s management interface.
newCredentials ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  Credentials
newCredentials pUsername_ pPassword_ =
  Credentials'
    { username =
        Data._Sensitive Lens.# pUsername_,
      password = Data._Sensitive Lens.# pPassword_
    }

-- | Specifies the user name for your storage system\'s management interface.
credentials_username :: Lens.Lens' Credentials Prelude.Text
credentials_username = Lens.lens (\Credentials' {username} -> username) (\s@Credentials' {} a -> s {username = a} :: Credentials) Prelude.. Data._Sensitive

-- | Specifies the password for your storage system\'s management interface.
credentials_password :: Lens.Lens' Credentials Prelude.Text
credentials_password = Lens.lens (\Credentials' {password} -> password) (\s@Credentials' {} a -> s {password = a} :: Credentials) Prelude.. Data._Sensitive

instance Prelude.Hashable Credentials where
  hashWithSalt _salt Credentials' {..} =
    _salt
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` password

instance Prelude.NFData Credentials where
  rnf Credentials' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance Data.ToJSON Credentials where
  toJSON Credentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Username" Data..= username),
            Prelude.Just ("Password" Data..= password)
          ]
      )
