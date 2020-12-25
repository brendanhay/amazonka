{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SecurityGroupMembership
  ( SecurityGroupMembership (..),

    -- * Smart constructor
    mkSecurityGroupMembership,

    -- * Lenses
    sgmSecurityGroupId,
    sgmStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a single cache security group and its status.
--
-- /See:/ 'mkSecurityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
  { -- | The identifier of the cache security group.
    securityGroupId :: Core.Maybe Types.String,
    -- | The status of the cache security group membership. The status changes whenever a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityGroupMembership' value with any optional fields omitted.
mkSecurityGroupMembership ::
  SecurityGroupMembership
mkSecurityGroupMembership =
  SecurityGroupMembership'
    { securityGroupId = Core.Nothing,
      status = Core.Nothing
    }

-- | The identifier of the cache security group.
--
-- /Note:/ Consider using 'securityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgmSecurityGroupId :: Lens.Lens' SecurityGroupMembership (Core.Maybe Types.String)
sgmSecurityGroupId = Lens.field @"securityGroupId"
{-# DEPRECATED sgmSecurityGroupId "Use generic-lens or generic-optics with 'securityGroupId' instead." #-}

-- | The status of the cache security group membership. The status changes whenever a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgmStatus :: Lens.Lens' SecurityGroupMembership (Core.Maybe Types.String)
sgmStatus = Lens.field @"status"
{-# DEPRECATED sgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML SecurityGroupMembership where
  parseXML x =
    SecurityGroupMembership'
      Core.<$> (x Core..@? "SecurityGroupId") Core.<*> (x Core..@? "Status")
