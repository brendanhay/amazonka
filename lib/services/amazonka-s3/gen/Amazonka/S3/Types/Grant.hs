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
-- Module      : Amazonka.S3.Types.Grant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Grant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Grantee
import Amazonka.S3.Types.Permission

-- | Container for grant information.
--
-- /See:/ 'newGrant' smart constructor.
data Grant = Grant'
  { -- | The person being granted permissions.
    grantee :: Prelude.Maybe Grantee,
    -- | Specifies the permission given to the grantee.
    permission :: Prelude.Maybe Permission
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Grant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantee', 'grant_grantee' - The person being granted permissions.
--
-- 'permission', 'grant_permission' - Specifies the permission given to the grantee.
newGrant ::
  Grant
newGrant =
  Grant'
    { grantee = Prelude.Nothing,
      permission = Prelude.Nothing
    }

-- | The person being granted permissions.
grant_grantee :: Lens.Lens' Grant (Prelude.Maybe Grantee)
grant_grantee = Lens.lens (\Grant' {grantee} -> grantee) (\s@Grant' {} a -> s {grantee = a} :: Grant)

-- | Specifies the permission given to the grantee.
grant_permission :: Lens.Lens' Grant (Prelude.Maybe Permission)
grant_permission = Lens.lens (\Grant' {permission} -> permission) (\s@Grant' {} a -> s {permission = a} :: Grant)

instance Data.FromXML Grant where
  parseXML x =
    Grant'
      Prelude.<$> (x Data..@? "Grantee")
      Prelude.<*> (x Data..@? "Permission")

instance Prelude.Hashable Grant where
  hashWithSalt _salt Grant' {..} =
    _salt
      `Prelude.hashWithSalt` grantee
      `Prelude.hashWithSalt` permission

instance Prelude.NFData Grant where
  rnf Grant' {..} =
    Prelude.rnf grantee
      `Prelude.seq` Prelude.rnf permission

instance Data.ToXML Grant where
  toXML Grant' {..} =
    Prelude.mconcat
      [ "Grantee" Data.@= grantee,
        "Permission" Data.@= permission
      ]
