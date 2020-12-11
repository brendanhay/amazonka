-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Permission
  ( Permission (..),

    -- * Smart constructor
    mkPermission,

    -- * Lenses
    pGranteeId,
    pGranteeType,
    pPermissionValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkMail.Types.MemberType
import Network.AWS.WorkMail.Types.PermissionType

-- | Permission granted to a user, group, or resource to access a certain aspect of another user, group, or resource mailbox.
--
-- /See:/ 'mkPermission' smart constructor.
data Permission = Permission'
  { granteeId :: Lude.Text,
    granteeType :: MemberType,
    permissionValues :: [PermissionType]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Permission' with the minimum fields required to make a request.
--
-- * 'granteeId' - The identifier of the user, group, or resource to which the permissions are granted.
-- * 'granteeType' - The type of user, group, or resource referred to in GranteeId.
-- * 'permissionValues' - The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
mkPermission ::
  -- | 'granteeId'
  Lude.Text ->
  -- | 'granteeType'
  MemberType ->
  Permission
mkPermission pGranteeId_ pGranteeType_ =
  Permission'
    { granteeId = pGranteeId_,
      granteeType = pGranteeType_,
      permissionValues = Lude.mempty
    }

-- | The identifier of the user, group, or resource to which the permissions are granted.
--
-- /Note:/ Consider using 'granteeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGranteeId :: Lens.Lens' Permission Lude.Text
pGranteeId = Lens.lens (granteeId :: Permission -> Lude.Text) (\s a -> s {granteeId = a} :: Permission)
{-# DEPRECATED pGranteeId "Use generic-lens or generic-optics with 'granteeId' instead." #-}

-- | The type of user, group, or resource referred to in GranteeId.
--
-- /Note:/ Consider using 'granteeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGranteeType :: Lens.Lens' Permission MemberType
pGranteeType = Lens.lens (granteeType :: Permission -> MemberType) (\s a -> s {granteeType = a} :: Permission)
{-# DEPRECATED pGranteeType "Use generic-lens or generic-optics with 'granteeType' instead." #-}

-- | The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
--
-- /Note:/ Consider using 'permissionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPermissionValues :: Lens.Lens' Permission [PermissionType]
pPermissionValues = Lens.lens (permissionValues :: Permission -> [PermissionType]) (\s a -> s {permissionValues = a} :: Permission)
{-# DEPRECATED pPermissionValues "Use generic-lens or generic-optics with 'permissionValues' instead." #-}

instance Lude.FromJSON Permission where
  parseJSON =
    Lude.withObject
      "Permission"
      ( \x ->
          Permission'
            Lude.<$> (x Lude..: "GranteeId")
            Lude.<*> (x Lude..: "GranteeType")
            Lude.<*> (x Lude..:? "PermissionValues" Lude..!= Lude.mempty)
      )
