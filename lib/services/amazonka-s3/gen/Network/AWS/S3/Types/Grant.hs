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
-- Module      : Network.AWS.S3.Types.Grant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Grant where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Grantee
import Network.AWS.S3.Types.Permission

-- | Container for grant information.
--
-- /See:/ 'newGrant' smart constructor.
data Grant = Grant'
  { -- | Specifies the permission given to the grantee.
    permission :: Prelude.Maybe Permission,
    -- | The person being granted permissions.
    grantee :: Prelude.Maybe Grantee
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
-- 'permission', 'grant_permission' - Specifies the permission given to the grantee.
--
-- 'grantee', 'grant_grantee' - The person being granted permissions.
newGrant ::
  Grant
newGrant =
  Grant'
    { permission = Prelude.Nothing,
      grantee = Prelude.Nothing
    }

-- | Specifies the permission given to the grantee.
grant_permission :: Lens.Lens' Grant (Prelude.Maybe Permission)
grant_permission = Lens.lens (\Grant' {permission} -> permission) (\s@Grant' {} a -> s {permission = a} :: Grant)

-- | The person being granted permissions.
grant_grantee :: Lens.Lens' Grant (Prelude.Maybe Grantee)
grant_grantee = Lens.lens (\Grant' {grantee} -> grantee) (\s@Grant' {} a -> s {grantee = a} :: Grant)

instance Core.FromXML Grant where
  parseXML x =
    Grant'
      Prelude.<$> (x Core..@? "Permission")
      Prelude.<*> (x Core..@? "Grantee")

instance Prelude.Hashable Grant

instance Prelude.NFData Grant

instance Core.ToXML Grant where
  toXML Grant' {..} =
    Prelude.mconcat
      [ "Permission" Core.@= permission,
        "Grantee" Core.@= grantee
      ]
