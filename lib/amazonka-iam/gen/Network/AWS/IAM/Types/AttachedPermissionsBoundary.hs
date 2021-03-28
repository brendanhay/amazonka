{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AttachedPermissionsBoundary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.AttachedPermissionsBoundary
  ( AttachedPermissionsBoundary (..)
  -- * Smart constructor
  , mkAttachedPermissionsBoundary
  -- * Lenses
  , apbPermissionsBoundaryArn
  , apbPermissionsBoundaryType
  ) where

import qualified Network.AWS.IAM.Types.PermissionsBoundaryArn as Types
import qualified Network.AWS.IAM.Types.PermissionsBoundaryAttachmentType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an attached permissions boundary.
--
-- An attached permissions boundary is a managed policy that has been attached to a user or role to set the permissions boundary.
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /See:/ 'mkAttachedPermissionsBoundary' smart constructor.
data AttachedPermissionsBoundary = AttachedPermissionsBoundary'
  { permissionsBoundaryArn :: Core.Maybe Types.PermissionsBoundaryArn
    -- ^ The ARN of the policy used to set the permissions boundary for the user or role.
  , permissionsBoundaryType :: Core.Maybe Types.PermissionsBoundaryAttachmentType
    -- ^ The permissions boundary usage type that indicates what type of IAM resource is used as the permissions boundary for an entity. This data type can only have a value of @Policy@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachedPermissionsBoundary' value with any optional fields omitted.
mkAttachedPermissionsBoundary
    :: AttachedPermissionsBoundary
mkAttachedPermissionsBoundary
  = AttachedPermissionsBoundary'{permissionsBoundaryArn =
                                   Core.Nothing,
                                 permissionsBoundaryType = Core.Nothing}

-- | The ARN of the policy used to set the permissions boundary for the user or role.
--
-- /Note:/ Consider using 'permissionsBoundaryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apbPermissionsBoundaryArn :: Lens.Lens' AttachedPermissionsBoundary (Core.Maybe Types.PermissionsBoundaryArn)
apbPermissionsBoundaryArn = Lens.field @"permissionsBoundaryArn"
{-# INLINEABLE apbPermissionsBoundaryArn #-}
{-# DEPRECATED permissionsBoundaryArn "Use generic-lens or generic-optics with 'permissionsBoundaryArn' instead"  #-}

-- | The permissions boundary usage type that indicates what type of IAM resource is used as the permissions boundary for an entity. This data type can only have a value of @Policy@ .
--
-- /Note:/ Consider using 'permissionsBoundaryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apbPermissionsBoundaryType :: Lens.Lens' AttachedPermissionsBoundary (Core.Maybe Types.PermissionsBoundaryAttachmentType)
apbPermissionsBoundaryType = Lens.field @"permissionsBoundaryType"
{-# INLINEABLE apbPermissionsBoundaryType #-}
{-# DEPRECATED permissionsBoundaryType "Use generic-lens or generic-optics with 'permissionsBoundaryType' instead"  #-}

instance Core.FromXML AttachedPermissionsBoundary where
        parseXML x
          = AttachedPermissionsBoundary' Core.<$>
              (x Core..@? "PermissionsBoundaryArn") Core.<*>
                x Core..@? "PermissionsBoundaryType"
