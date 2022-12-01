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
-- Module      : Amazonka.IAM.Types.AttachedPermissionsBoundary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.AttachedPermissionsBoundary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types.PermissionsBoundaryAttachmentType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an attached permissions boundary.
--
-- An attached permissions boundary is a managed policy that has been
-- attached to a user or role to set the permissions boundary.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
--
-- /See:/ 'newAttachedPermissionsBoundary' smart constructor.
data AttachedPermissionsBoundary = AttachedPermissionsBoundary'
  { -- | The permissions boundary usage type that indicates what type of IAM
    -- resource is used as the permissions boundary for an entity. This data
    -- type can only have a value of @Policy@.
    permissionsBoundaryType :: Prelude.Maybe PermissionsBoundaryAttachmentType,
    -- | The ARN of the policy used to set the permissions boundary for the user
    -- or role.
    permissionsBoundaryArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachedPermissionsBoundary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionsBoundaryType', 'attachedPermissionsBoundary_permissionsBoundaryType' - The permissions boundary usage type that indicates what type of IAM
-- resource is used as the permissions boundary for an entity. This data
-- type can only have a value of @Policy@.
--
-- 'permissionsBoundaryArn', 'attachedPermissionsBoundary_permissionsBoundaryArn' - The ARN of the policy used to set the permissions boundary for the user
-- or role.
newAttachedPermissionsBoundary ::
  AttachedPermissionsBoundary
newAttachedPermissionsBoundary =
  AttachedPermissionsBoundary'
    { permissionsBoundaryType =
        Prelude.Nothing,
      permissionsBoundaryArn = Prelude.Nothing
    }

-- | The permissions boundary usage type that indicates what type of IAM
-- resource is used as the permissions boundary for an entity. This data
-- type can only have a value of @Policy@.
attachedPermissionsBoundary_permissionsBoundaryType :: Lens.Lens' AttachedPermissionsBoundary (Prelude.Maybe PermissionsBoundaryAttachmentType)
attachedPermissionsBoundary_permissionsBoundaryType = Lens.lens (\AttachedPermissionsBoundary' {permissionsBoundaryType} -> permissionsBoundaryType) (\s@AttachedPermissionsBoundary' {} a -> s {permissionsBoundaryType = a} :: AttachedPermissionsBoundary)

-- | The ARN of the policy used to set the permissions boundary for the user
-- or role.
attachedPermissionsBoundary_permissionsBoundaryArn :: Lens.Lens' AttachedPermissionsBoundary (Prelude.Maybe Prelude.Text)
attachedPermissionsBoundary_permissionsBoundaryArn = Lens.lens (\AttachedPermissionsBoundary' {permissionsBoundaryArn} -> permissionsBoundaryArn) (\s@AttachedPermissionsBoundary' {} a -> s {permissionsBoundaryArn = a} :: AttachedPermissionsBoundary)

instance Core.FromXML AttachedPermissionsBoundary where
  parseXML x =
    AttachedPermissionsBoundary'
      Prelude.<$> (x Core..@? "PermissionsBoundaryType")
      Prelude.<*> (x Core..@? "PermissionsBoundaryArn")

instance Prelude.Hashable AttachedPermissionsBoundary where
  hashWithSalt _salt AttachedPermissionsBoundary' {..} =
    _salt
      `Prelude.hashWithSalt` permissionsBoundaryType
      `Prelude.hashWithSalt` permissionsBoundaryArn

instance Prelude.NFData AttachedPermissionsBoundary where
  rnf AttachedPermissionsBoundary' {..} =
    Prelude.rnf permissionsBoundaryType
      `Prelude.seq` Prelude.rnf permissionsBoundaryArn
