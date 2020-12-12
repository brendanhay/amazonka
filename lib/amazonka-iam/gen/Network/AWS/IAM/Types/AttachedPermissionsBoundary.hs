{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AttachedPermissionsBoundary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AttachedPermissionsBoundary
  ( AttachedPermissionsBoundary (..),

    -- * Smart constructor
    mkAttachedPermissionsBoundary,

    -- * Lenses
    apbPermissionsBoundaryType,
    apbPermissionsBoundaryARN,
  )
where

import Network.AWS.IAM.Types.PermissionsBoundaryAttachmentType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an attached permissions boundary.
--
-- An attached permissions boundary is a managed policy that has been attached to a user or role to set the permissions boundary.
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /See:/ 'mkAttachedPermissionsBoundary' smart constructor.
data AttachedPermissionsBoundary = AttachedPermissionsBoundary'
  { permissionsBoundaryType ::
      Lude.Maybe
        PermissionsBoundaryAttachmentType,
    permissionsBoundaryARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachedPermissionsBoundary' with the minimum fields required to make a request.
--
-- * 'permissionsBoundaryARN' - The ARN of the policy used to set the permissions boundary for the user or role.
-- * 'permissionsBoundaryType' - The permissions boundary usage type that indicates what type of IAM resource is used as the permissions boundary for an entity. This data type can only have a value of @Policy@ .
mkAttachedPermissionsBoundary ::
  AttachedPermissionsBoundary
mkAttachedPermissionsBoundary =
  AttachedPermissionsBoundary'
    { permissionsBoundaryType =
        Lude.Nothing,
      permissionsBoundaryARN = Lude.Nothing
    }

-- | The permissions boundary usage type that indicates what type of IAM resource is used as the permissions boundary for an entity. This data type can only have a value of @Policy@ .
--
-- /Note:/ Consider using 'permissionsBoundaryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apbPermissionsBoundaryType :: Lens.Lens' AttachedPermissionsBoundary (Lude.Maybe PermissionsBoundaryAttachmentType)
apbPermissionsBoundaryType = Lens.lens (permissionsBoundaryType :: AttachedPermissionsBoundary -> Lude.Maybe PermissionsBoundaryAttachmentType) (\s a -> s {permissionsBoundaryType = a} :: AttachedPermissionsBoundary)
{-# DEPRECATED apbPermissionsBoundaryType "Use generic-lens or generic-optics with 'permissionsBoundaryType' instead." #-}

-- | The ARN of the policy used to set the permissions boundary for the user or role.
--
-- /Note:/ Consider using 'permissionsBoundaryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apbPermissionsBoundaryARN :: Lens.Lens' AttachedPermissionsBoundary (Lude.Maybe Lude.Text)
apbPermissionsBoundaryARN = Lens.lens (permissionsBoundaryARN :: AttachedPermissionsBoundary -> Lude.Maybe Lude.Text) (\s a -> s {permissionsBoundaryARN = a} :: AttachedPermissionsBoundary)
{-# DEPRECATED apbPermissionsBoundaryARN "Use generic-lens or generic-optics with 'permissionsBoundaryARN' instead." #-}

instance Lude.FromXML AttachedPermissionsBoundary where
  parseXML x =
    AttachedPermissionsBoundary'
      Lude.<$> (x Lude..@? "PermissionsBoundaryType")
      Lude.<*> (x Lude..@? "PermissionsBoundaryArn")
