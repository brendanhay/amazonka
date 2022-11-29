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
-- Module      : Amazonka.Signer.Types.Permission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.Permission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A cross-account permission for a signing profile.
--
-- /See:/ 'newPermission' smart constructor.
data Permission = Permission'
  { -- | The AWS principal that has been granted a cross-account permission.
    principal :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a cross-account permission statement.
    statementId :: Prelude.Maybe Prelude.Text,
    -- | The signing profile version that a permission applies to.
    profileVersion :: Prelude.Maybe Prelude.Text,
    -- | An AWS Signer action permitted as part of cross-account permissions.
    action :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Permission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'permission_principal' - The AWS principal that has been granted a cross-account permission.
--
-- 'statementId', 'permission_statementId' - A unique identifier for a cross-account permission statement.
--
-- 'profileVersion', 'permission_profileVersion' - The signing profile version that a permission applies to.
--
-- 'action', 'permission_action' - An AWS Signer action permitted as part of cross-account permissions.
newPermission ::
  Permission
newPermission =
  Permission'
    { principal = Prelude.Nothing,
      statementId = Prelude.Nothing,
      profileVersion = Prelude.Nothing,
      action = Prelude.Nothing
    }

-- | The AWS principal that has been granted a cross-account permission.
permission_principal :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_principal = Lens.lens (\Permission' {principal} -> principal) (\s@Permission' {} a -> s {principal = a} :: Permission)

-- | A unique identifier for a cross-account permission statement.
permission_statementId :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_statementId = Lens.lens (\Permission' {statementId} -> statementId) (\s@Permission' {} a -> s {statementId = a} :: Permission)

-- | The signing profile version that a permission applies to.
permission_profileVersion :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_profileVersion = Lens.lens (\Permission' {profileVersion} -> profileVersion) (\s@Permission' {} a -> s {profileVersion = a} :: Permission)

-- | An AWS Signer action permitted as part of cross-account permissions.
permission_action :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_action = Lens.lens (\Permission' {action} -> action) (\s@Permission' {} a -> s {action = a} :: Permission)

instance Core.FromJSON Permission where
  parseJSON =
    Core.withObject
      "Permission"
      ( \x ->
          Permission'
            Prelude.<$> (x Core..:? "principal")
            Prelude.<*> (x Core..:? "statementId")
            Prelude.<*> (x Core..:? "profileVersion")
            Prelude.<*> (x Core..:? "action")
      )

instance Prelude.Hashable Permission where
  hashWithSalt _salt Permission' {..} =
    _salt `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` statementId
      `Prelude.hashWithSalt` profileVersion
      `Prelude.hashWithSalt` action

instance Prelude.NFData Permission where
  rnf Permission' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf statementId
      `Prelude.seq` Prelude.rnf profileVersion
      `Prelude.seq` Prelude.rnf action
