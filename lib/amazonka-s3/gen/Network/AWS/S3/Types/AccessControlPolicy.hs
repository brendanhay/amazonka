{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AccessControlPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AccessControlPolicy
  ( AccessControlPolicy (..),

    -- * Smart constructor
    mkAccessControlPolicy,

    -- * Lenses
    acpGrants,
    acpOwner,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Grant as Types
import qualified Network.AWS.S3.Types.Owner as Types

-- | Contains the elements that set the ACL permissions for an object per grantee.
--
-- /See:/ 'mkAccessControlPolicy' smart constructor.
data AccessControlPolicy = AccessControlPolicy'
  { -- | A list of grants.
    grants :: Core.Maybe [Types.Grant],
    -- | Container for the bucket owner's display name and ID.
    owner :: Core.Maybe Types.Owner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccessControlPolicy' value with any optional fields omitted.
mkAccessControlPolicy ::
  AccessControlPolicy
mkAccessControlPolicy =
  AccessControlPolicy' {grants = Core.Nothing, owner = Core.Nothing}

-- | A list of grants.
--
-- /Note:/ Consider using 'grants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpGrants :: Lens.Lens' AccessControlPolicy (Core.Maybe [Types.Grant])
acpGrants = Lens.field @"grants"
{-# DEPRECATED acpGrants "Use generic-lens or generic-optics with 'grants' instead." #-}

-- | Container for the bucket owner's display name and ID.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpOwner :: Lens.Lens' AccessControlPolicy (Core.Maybe Types.Owner)
acpOwner = Lens.field @"owner"
{-# DEPRECATED acpOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

instance Core.ToXML AccessControlPolicy where
  toXML AccessControlPolicy {..} =
    Core.toXMLNode
      "AccessControlList"
      (Core.toXMLList "Grant" Core.<$> grants)
      Core.<> Core.toXMLNode "Owner" Core.<$> owner
