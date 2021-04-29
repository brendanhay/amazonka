{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkMail.Types.Permission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Permission where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkMail.Types.MemberType
import Network.AWS.WorkMail.Types.PermissionType

-- | Permission granted to a user, group, or resource to access a certain
-- aspect of another user, group, or resource mailbox.
--
-- /See:/ 'newPermission' smart constructor.
data Permission = Permission'
  { -- | The identifier of the user, group, or resource to which the permissions
    -- are granted.
    granteeId :: Prelude.Text,
    -- | The type of user, group, or resource referred to in GranteeId.
    granteeType :: MemberType,
    -- | The permissions granted to the grantee. SEND_AS allows the grantee to
    -- send email as the owner of the mailbox (the grantee is not mentioned on
    -- these emails). SEND_ON_BEHALF allows the grantee to send email on behalf
    -- of the owner of the mailbox (the grantee is not mentioned as the
    -- physical sender of these emails). FULL_ACCESS allows the grantee full
    -- access to the mailbox, irrespective of other folder-level permissions
    -- set on the mailbox.
    permissionValues :: [PermissionType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Permission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'granteeId', 'permission_granteeId' - The identifier of the user, group, or resource to which the permissions
-- are granted.
--
-- 'granteeType', 'permission_granteeType' - The type of user, group, or resource referred to in GranteeId.
--
-- 'permissionValues', 'permission_permissionValues' - The permissions granted to the grantee. SEND_AS allows the grantee to
-- send email as the owner of the mailbox (the grantee is not mentioned on
-- these emails). SEND_ON_BEHALF allows the grantee to send email on behalf
-- of the owner of the mailbox (the grantee is not mentioned as the
-- physical sender of these emails). FULL_ACCESS allows the grantee full
-- access to the mailbox, irrespective of other folder-level permissions
-- set on the mailbox.
newPermission ::
  -- | 'granteeId'
  Prelude.Text ->
  -- | 'granteeType'
  MemberType ->
  Permission
newPermission pGranteeId_ pGranteeType_ =
  Permission'
    { granteeId = pGranteeId_,
      granteeType = pGranteeType_,
      permissionValues = Prelude.mempty
    }

-- | The identifier of the user, group, or resource to which the permissions
-- are granted.
permission_granteeId :: Lens.Lens' Permission Prelude.Text
permission_granteeId = Lens.lens (\Permission' {granteeId} -> granteeId) (\s@Permission' {} a -> s {granteeId = a} :: Permission)

-- | The type of user, group, or resource referred to in GranteeId.
permission_granteeType :: Lens.Lens' Permission MemberType
permission_granteeType = Lens.lens (\Permission' {granteeType} -> granteeType) (\s@Permission' {} a -> s {granteeType = a} :: Permission)

-- | The permissions granted to the grantee. SEND_AS allows the grantee to
-- send email as the owner of the mailbox (the grantee is not mentioned on
-- these emails). SEND_ON_BEHALF allows the grantee to send email on behalf
-- of the owner of the mailbox (the grantee is not mentioned as the
-- physical sender of these emails). FULL_ACCESS allows the grantee full
-- access to the mailbox, irrespective of other folder-level permissions
-- set on the mailbox.
permission_permissionValues :: Lens.Lens' Permission [PermissionType]
permission_permissionValues = Lens.lens (\Permission' {permissionValues} -> permissionValues) (\s@Permission' {} a -> s {permissionValues = a} :: Permission) Prelude.. Prelude._Coerce

instance Prelude.FromJSON Permission where
  parseJSON =
    Prelude.withObject
      "Permission"
      ( \x ->
          Permission'
            Prelude.<$> (x Prelude..: "GranteeId")
            Prelude.<*> (x Prelude..: "GranteeType")
            Prelude.<*> ( x Prelude..:? "PermissionValues"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Permission

instance Prelude.NFData Permission
