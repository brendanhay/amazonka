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
-- Module      : Network.AWS.Glacier.Types.Grant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Grant where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types.Grantee
import Network.AWS.Glacier.Types.Permission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a grant.
--
-- /See:/ 'newGrant' smart constructor.
data Grant = Grant'
  { -- | The grantee.
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
-- 'grantee', 'grant_grantee' - The grantee.
--
-- 'permission', 'grant_permission' - Specifies the permission given to the grantee.
newGrant ::
  Grant
newGrant =
  Grant'
    { grantee = Prelude.Nothing,
      permission = Prelude.Nothing
    }

-- | The grantee.
grant_grantee :: Lens.Lens' Grant (Prelude.Maybe Grantee)
grant_grantee = Lens.lens (\Grant' {grantee} -> grantee) (\s@Grant' {} a -> s {grantee = a} :: Grant)

-- | Specifies the permission given to the grantee.
grant_permission :: Lens.Lens' Grant (Prelude.Maybe Permission)
grant_permission = Lens.lens (\Grant' {permission} -> permission) (\s@Grant' {} a -> s {permission = a} :: Grant)

instance Core.FromJSON Grant where
  parseJSON =
    Core.withObject
      "Grant"
      ( \x ->
          Grant'
            Prelude.<$> (x Core..:? "Grantee")
            Prelude.<*> (x Core..:? "Permission")
      )

instance Prelude.Hashable Grant

instance Prelude.NFData Grant

instance Core.ToJSON Grant where
  toJSON Grant' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Grantee" Core..=) Prelude.<$> grantee,
            ("Permission" Core..=) Prelude.<$> permission
          ]
      )
