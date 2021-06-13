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
-- Module      : Network.AWS.CertificateManagerPCA.Types.Permission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.Permission where

import Network.AWS.CertificateManagerPCA.Types.ActionType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Permissions designate which private CA actions can be performed by an
-- AWS service or entity. In order for ACM to automatically renew private
-- certificates, you must give the ACM service principal all available
-- permissions (@IssueCertificate@, @GetCertificate@, and
-- @ListPermissions@). Permissions can be assigned with the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreatePermission.html CreatePermission>
-- action, removed with the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePermission.html DeletePermission>
-- action, and listed with the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListPermissions.html ListPermissions>
-- action.
--
-- /See:/ 'newPermission' smart constructor.
data Permission = Permission'
  { -- | The Amazon Resource Number (ARN) of the private CA from which the
    -- permission was issued.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the permission was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The private CA actions that can be performed by the designated AWS
    -- service.
    actions :: Prelude.Maybe (Prelude.NonEmpty ActionType),
    -- | The AWS service or entity that holds the permission. At this time, the
    -- only valid principal is @acm.amazonaws.com@.
    principal :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account that assigned the permission.
    sourceAccount :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy that is associated with the permission.
    policy :: Prelude.Maybe Prelude.Text
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
-- 'certificateAuthorityArn', 'permission_certificateAuthorityArn' - The Amazon Resource Number (ARN) of the private CA from which the
-- permission was issued.
--
-- 'createdAt', 'permission_createdAt' - The time at which the permission was created.
--
-- 'actions', 'permission_actions' - The private CA actions that can be performed by the designated AWS
-- service.
--
-- 'principal', 'permission_principal' - The AWS service or entity that holds the permission. At this time, the
-- only valid principal is @acm.amazonaws.com@.
--
-- 'sourceAccount', 'permission_sourceAccount' - The ID of the account that assigned the permission.
--
-- 'policy', 'permission_policy' - The name of the policy that is associated with the permission.
newPermission ::
  Permission
newPermission =
  Permission'
    { certificateAuthorityArn =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      actions = Prelude.Nothing,
      principal = Prelude.Nothing,
      sourceAccount = Prelude.Nothing,
      policy = Prelude.Nothing
    }

-- | The Amazon Resource Number (ARN) of the private CA from which the
-- permission was issued.
permission_certificateAuthorityArn :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_certificateAuthorityArn = Lens.lens (\Permission' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@Permission' {} a -> s {certificateAuthorityArn = a} :: Permission)

-- | The time at which the permission was created.
permission_createdAt :: Lens.Lens' Permission (Prelude.Maybe Prelude.UTCTime)
permission_createdAt = Lens.lens (\Permission' {createdAt} -> createdAt) (\s@Permission' {} a -> s {createdAt = a} :: Permission) Prelude.. Lens.mapping Core._Time

-- | The private CA actions that can be performed by the designated AWS
-- service.
permission_actions :: Lens.Lens' Permission (Prelude.Maybe (Prelude.NonEmpty ActionType))
permission_actions = Lens.lens (\Permission' {actions} -> actions) (\s@Permission' {} a -> s {actions = a} :: Permission) Prelude.. Lens.mapping Lens._Coerce

-- | The AWS service or entity that holds the permission. At this time, the
-- only valid principal is @acm.amazonaws.com@.
permission_principal :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_principal = Lens.lens (\Permission' {principal} -> principal) (\s@Permission' {} a -> s {principal = a} :: Permission)

-- | The ID of the account that assigned the permission.
permission_sourceAccount :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_sourceAccount = Lens.lens (\Permission' {sourceAccount} -> sourceAccount) (\s@Permission' {} a -> s {sourceAccount = a} :: Permission)

-- | The name of the policy that is associated with the permission.
permission_policy :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_policy = Lens.lens (\Permission' {policy} -> policy) (\s@Permission' {} a -> s {policy = a} :: Permission)

instance Core.FromJSON Permission where
  parseJSON =
    Core.withObject
      "Permission"
      ( \x ->
          Permission'
            Prelude.<$> (x Core..:? "CertificateAuthorityArn")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Actions")
            Prelude.<*> (x Core..:? "Principal")
            Prelude.<*> (x Core..:? "SourceAccount")
            Prelude.<*> (x Core..:? "Policy")
      )

instance Prelude.Hashable Permission

instance Prelude.NFData Permission
