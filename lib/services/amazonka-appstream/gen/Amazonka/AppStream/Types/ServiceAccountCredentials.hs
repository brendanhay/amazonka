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
-- Module      : Amazonka.AppStream.Types.ServiceAccountCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ServiceAccountCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the credentials for the service account used by the fleet or
-- image builder to connect to the directory.
--
-- /See:/ 'newServiceAccountCredentials' smart constructor.
data ServiceAccountCredentials = ServiceAccountCredentials'
  { -- | The user name of the account. This account must have the following
    -- privileges: create computer objects, join computers to the domain, and
    -- change\/reset the password on descendant computer objects for the
    -- organizational units specified.
    accountName :: Core.Sensitive Prelude.Text,
    -- | The password for the account.
    accountPassword :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceAccountCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountName', 'serviceAccountCredentials_accountName' - The user name of the account. This account must have the following
-- privileges: create computer objects, join computers to the domain, and
-- change\/reset the password on descendant computer objects for the
-- organizational units specified.
--
-- 'accountPassword', 'serviceAccountCredentials_accountPassword' - The password for the account.
newServiceAccountCredentials ::
  -- | 'accountName'
  Prelude.Text ->
  -- | 'accountPassword'
  Prelude.Text ->
  ServiceAccountCredentials
newServiceAccountCredentials
  pAccountName_
  pAccountPassword_ =
    ServiceAccountCredentials'
      { accountName =
          Core._Sensitive Lens.# pAccountName_,
        accountPassword =
          Core._Sensitive Lens.# pAccountPassword_
      }

-- | The user name of the account. This account must have the following
-- privileges: create computer objects, join computers to the domain, and
-- change\/reset the password on descendant computer objects for the
-- organizational units specified.
serviceAccountCredentials_accountName :: Lens.Lens' ServiceAccountCredentials Prelude.Text
serviceAccountCredentials_accountName = Lens.lens (\ServiceAccountCredentials' {accountName} -> accountName) (\s@ServiceAccountCredentials' {} a -> s {accountName = a} :: ServiceAccountCredentials) Prelude.. Core._Sensitive

-- | The password for the account.
serviceAccountCredentials_accountPassword :: Lens.Lens' ServiceAccountCredentials Prelude.Text
serviceAccountCredentials_accountPassword = Lens.lens (\ServiceAccountCredentials' {accountPassword} -> accountPassword) (\s@ServiceAccountCredentials' {} a -> s {accountPassword = a} :: ServiceAccountCredentials) Prelude.. Core._Sensitive

instance Core.FromJSON ServiceAccountCredentials where
  parseJSON =
    Core.withObject
      "ServiceAccountCredentials"
      ( \x ->
          ServiceAccountCredentials'
            Prelude.<$> (x Core..: "AccountName")
            Prelude.<*> (x Core..: "AccountPassword")
      )

instance Prelude.Hashable ServiceAccountCredentials where
  hashWithSalt _salt ServiceAccountCredentials' {..} =
    _salt `Prelude.hashWithSalt` accountName
      `Prelude.hashWithSalt` accountPassword

instance Prelude.NFData ServiceAccountCredentials where
  rnf ServiceAccountCredentials' {..} =
    Prelude.rnf accountName
      `Prelude.seq` Prelude.rnf accountPassword

instance Core.ToJSON ServiceAccountCredentials where
  toJSON ServiceAccountCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountName" Core..= accountName),
            Prelude.Just
              ("AccountPassword" Core..= accountPassword)
          ]
      )
