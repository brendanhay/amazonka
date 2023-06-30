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
-- Module      : Amazonka.SSO.Types.RoleInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSO.Types.RoleInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the role that is assigned to the user.
--
-- /See:/ 'newRoleInfo' smart constructor.
data RoleInfo = RoleInfo'
  { -- | The identifier of the AWS account assigned to the user.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the role that is assigned to the user.
    roleName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoleInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'roleInfo_accountId' - The identifier of the AWS account assigned to the user.
--
-- 'roleName', 'roleInfo_roleName' - The friendly name of the role that is assigned to the user.
newRoleInfo ::
  RoleInfo
newRoleInfo =
  RoleInfo'
    { accountId = Prelude.Nothing,
      roleName = Prelude.Nothing
    }

-- | The identifier of the AWS account assigned to the user.
roleInfo_accountId :: Lens.Lens' RoleInfo (Prelude.Maybe Prelude.Text)
roleInfo_accountId = Lens.lens (\RoleInfo' {accountId} -> accountId) (\s@RoleInfo' {} a -> s {accountId = a} :: RoleInfo)

-- | The friendly name of the role that is assigned to the user.
roleInfo_roleName :: Lens.Lens' RoleInfo (Prelude.Maybe Prelude.Text)
roleInfo_roleName = Lens.lens (\RoleInfo' {roleName} -> roleName) (\s@RoleInfo' {} a -> s {roleName = a} :: RoleInfo)

instance Data.FromJSON RoleInfo where
  parseJSON =
    Data.withObject
      "RoleInfo"
      ( \x ->
          RoleInfo'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "roleName")
      )

instance Prelude.Hashable RoleInfo where
  hashWithSalt _salt RoleInfo' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` roleName

instance Prelude.NFData RoleInfo where
  rnf RoleInfo' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf roleName
