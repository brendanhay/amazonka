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
-- Module      : Amazonka.ManagedBlockChain.Types.MemberFabricConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.MemberFabricConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration properties for Hyperledger Fabric for a member in a
-- Managed Blockchain network using the Hyperledger Fabric framework.
--
-- /See:/ 'newMemberFabricConfiguration' smart constructor.
data MemberFabricConfiguration = MemberFabricConfiguration'
  { -- | The user name for the member\'s initial administrative user.
    adminUsername :: Prelude.Text,
    -- | The password for the member\'s initial administrative user. The
    -- @AdminPassword@ must be at least eight characters long and no more than
    -- 32 characters. It must contain at least one uppercase letter, one
    -- lowercase letter, and one digit. It cannot have a single quotation mark
    -- (‘), a double quotation marks (“), a forward slash(\/), a backward
    -- slash(\\), \@, or a space.
    adminPassword :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberFabricConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminUsername', 'memberFabricConfiguration_adminUsername' - The user name for the member\'s initial administrative user.
--
-- 'adminPassword', 'memberFabricConfiguration_adminPassword' - The password for the member\'s initial administrative user. The
-- @AdminPassword@ must be at least eight characters long and no more than
-- 32 characters. It must contain at least one uppercase letter, one
-- lowercase letter, and one digit. It cannot have a single quotation mark
-- (‘), a double quotation marks (“), a forward slash(\/), a backward
-- slash(\\), \@, or a space.
newMemberFabricConfiguration ::
  -- | 'adminUsername'
  Prelude.Text ->
  -- | 'adminPassword'
  Prelude.Text ->
  MemberFabricConfiguration
newMemberFabricConfiguration
  pAdminUsername_
  pAdminPassword_ =
    MemberFabricConfiguration'
      { adminUsername =
          pAdminUsername_,
        adminPassword =
          Data._Sensitive Lens.# pAdminPassword_
      }

-- | The user name for the member\'s initial administrative user.
memberFabricConfiguration_adminUsername :: Lens.Lens' MemberFabricConfiguration Prelude.Text
memberFabricConfiguration_adminUsername = Lens.lens (\MemberFabricConfiguration' {adminUsername} -> adminUsername) (\s@MemberFabricConfiguration' {} a -> s {adminUsername = a} :: MemberFabricConfiguration)

-- | The password for the member\'s initial administrative user. The
-- @AdminPassword@ must be at least eight characters long and no more than
-- 32 characters. It must contain at least one uppercase letter, one
-- lowercase letter, and one digit. It cannot have a single quotation mark
-- (‘), a double quotation marks (“), a forward slash(\/), a backward
-- slash(\\), \@, or a space.
memberFabricConfiguration_adminPassword :: Lens.Lens' MemberFabricConfiguration Prelude.Text
memberFabricConfiguration_adminPassword = Lens.lens (\MemberFabricConfiguration' {adminPassword} -> adminPassword) (\s@MemberFabricConfiguration' {} a -> s {adminPassword = a} :: MemberFabricConfiguration) Prelude.. Data._Sensitive

instance Prelude.Hashable MemberFabricConfiguration where
  hashWithSalt _salt MemberFabricConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` adminUsername
      `Prelude.hashWithSalt` adminPassword

instance Prelude.NFData MemberFabricConfiguration where
  rnf MemberFabricConfiguration' {..} =
    Prelude.rnf adminUsername
      `Prelude.seq` Prelude.rnf adminPassword

instance Data.ToJSON MemberFabricConfiguration where
  toJSON MemberFabricConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AdminUsername" Data..= adminUsername),
            Prelude.Just
              ("AdminPassword" Data..= adminPassword)
          ]
      )
