{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.Permission
  ( Permission (..)
  -- * Smart constructor
  , mkPermission
  -- * Lenses
  , pGranteeId
  , pGranteeType
  , pPermissionValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkMail.Types.MemberType as Types
import qualified Network.AWS.WorkMail.Types.PermissionType as Types
import qualified Network.AWS.WorkMail.Types.WorkMailIdentifier as Types

-- | Permission granted to a user, group, or resource to access a certain aspect of another user, group, or resource mailbox.
--
-- /See:/ 'mkPermission' smart constructor.
data Permission = Permission'
  { granteeId :: Types.WorkMailIdentifier
    -- ^ The identifier of the user, group, or resource to which the permissions are granted.
  , granteeType :: Types.MemberType
    -- ^ The type of user, group, or resource referred to in GranteeId.
  , permissionValues :: [Types.PermissionType]
    -- ^ The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Permission' value with any optional fields omitted.
mkPermission
    :: Types.WorkMailIdentifier -- ^ 'granteeId'
    -> Types.MemberType -- ^ 'granteeType'
    -> Permission
mkPermission granteeId granteeType
  = Permission'{granteeId, granteeType,
                permissionValues = Core.mempty}

-- | The identifier of the user, group, or resource to which the permissions are granted.
--
-- /Note:/ Consider using 'granteeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGranteeId :: Lens.Lens' Permission Types.WorkMailIdentifier
pGranteeId = Lens.field @"granteeId"
{-# INLINEABLE pGranteeId #-}
{-# DEPRECATED granteeId "Use generic-lens or generic-optics with 'granteeId' instead"  #-}

-- | The type of user, group, or resource referred to in GranteeId.
--
-- /Note:/ Consider using 'granteeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGranteeType :: Lens.Lens' Permission Types.MemberType
pGranteeType = Lens.field @"granteeType"
{-# INLINEABLE pGranteeType #-}
{-# DEPRECATED granteeType "Use generic-lens or generic-optics with 'granteeType' instead"  #-}

-- | The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
--
-- /Note:/ Consider using 'permissionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPermissionValues :: Lens.Lens' Permission [Types.PermissionType]
pPermissionValues = Lens.field @"permissionValues"
{-# INLINEABLE pPermissionValues #-}
{-# DEPRECATED permissionValues "Use generic-lens or generic-optics with 'permissionValues' instead"  #-}

instance Core.FromJSON Permission where
        parseJSON
          = Core.withObject "Permission" Core.$
              \ x ->
                Permission' Core.<$>
                  (x Core..: "GranteeId") Core.<*> x Core..: "GranteeType" Core.<*>
                    x Core..:? "PermissionValues" Core..!= Core.mempty
