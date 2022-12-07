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
-- Module      : Amazonka.WorkMail.Types.ImpersonationRole
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.ImpersonationRole where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.ImpersonationRoleType

-- | An impersonation role for the given WorkMail organization.
--
-- /See:/ 'newImpersonationRole' smart constructor.
data ImpersonationRole = ImpersonationRole'
  { -- | The impersonation role name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The impersonation role type.
    type' :: Prelude.Maybe ImpersonationRoleType,
    -- | The date when the impersonation role was created.
    dateCreated :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the impersonation role.
    impersonationRoleId :: Prelude.Maybe Prelude.Text,
    -- | The date when the impersonation role was last modified.
    dateModified :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImpersonationRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'impersonationRole_name' - The impersonation role name.
--
-- 'type'', 'impersonationRole_type' - The impersonation role type.
--
-- 'dateCreated', 'impersonationRole_dateCreated' - The date when the impersonation role was created.
--
-- 'impersonationRoleId', 'impersonationRole_impersonationRoleId' - The identifier of the impersonation role.
--
-- 'dateModified', 'impersonationRole_dateModified' - The date when the impersonation role was last modified.
newImpersonationRole ::
  ImpersonationRole
newImpersonationRole =
  ImpersonationRole'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      impersonationRoleId = Prelude.Nothing,
      dateModified = Prelude.Nothing
    }

-- | The impersonation role name.
impersonationRole_name :: Lens.Lens' ImpersonationRole (Prelude.Maybe Prelude.Text)
impersonationRole_name = Lens.lens (\ImpersonationRole' {name} -> name) (\s@ImpersonationRole' {} a -> s {name = a} :: ImpersonationRole)

-- | The impersonation role type.
impersonationRole_type :: Lens.Lens' ImpersonationRole (Prelude.Maybe ImpersonationRoleType)
impersonationRole_type = Lens.lens (\ImpersonationRole' {type'} -> type') (\s@ImpersonationRole' {} a -> s {type' = a} :: ImpersonationRole)

-- | The date when the impersonation role was created.
impersonationRole_dateCreated :: Lens.Lens' ImpersonationRole (Prelude.Maybe Prelude.UTCTime)
impersonationRole_dateCreated = Lens.lens (\ImpersonationRole' {dateCreated} -> dateCreated) (\s@ImpersonationRole' {} a -> s {dateCreated = a} :: ImpersonationRole) Prelude.. Lens.mapping Data._Time

-- | The identifier of the impersonation role.
impersonationRole_impersonationRoleId :: Lens.Lens' ImpersonationRole (Prelude.Maybe Prelude.Text)
impersonationRole_impersonationRoleId = Lens.lens (\ImpersonationRole' {impersonationRoleId} -> impersonationRoleId) (\s@ImpersonationRole' {} a -> s {impersonationRoleId = a} :: ImpersonationRole)

-- | The date when the impersonation role was last modified.
impersonationRole_dateModified :: Lens.Lens' ImpersonationRole (Prelude.Maybe Prelude.UTCTime)
impersonationRole_dateModified = Lens.lens (\ImpersonationRole' {dateModified} -> dateModified) (\s@ImpersonationRole' {} a -> s {dateModified = a} :: ImpersonationRole) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ImpersonationRole where
  parseJSON =
    Data.withObject
      "ImpersonationRole"
      ( \x ->
          ImpersonationRole'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "DateCreated")
            Prelude.<*> (x Data..:? "ImpersonationRoleId")
            Prelude.<*> (x Data..:? "DateModified")
      )

instance Prelude.Hashable ImpersonationRole where
  hashWithSalt _salt ImpersonationRole' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` impersonationRoleId
      `Prelude.hashWithSalt` dateModified

instance Prelude.NFData ImpersonationRole where
  rnf ImpersonationRole' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf impersonationRoleId
      `Prelude.seq` Prelude.rnf dateModified
