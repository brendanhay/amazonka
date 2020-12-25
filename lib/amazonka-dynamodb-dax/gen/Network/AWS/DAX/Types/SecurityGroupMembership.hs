{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SecurityGroupMembership
  ( SecurityGroupMembership (..),

    -- * Smart constructor
    mkSecurityGroupMembership,

    -- * Lenses
    sgmSecurityGroupIdentifier,
    sgmStatus,
  )
where

import qualified Network.AWS.DAX.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An individual VPC security group and its status.
--
-- /See:/ 'mkSecurityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
  { -- | The unique ID for this security group.
    securityGroupIdentifier :: Core.Maybe Types.String,
    -- | The status of this security group.
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityGroupMembership' value with any optional fields omitted.
mkSecurityGroupMembership ::
  SecurityGroupMembership
mkSecurityGroupMembership =
  SecurityGroupMembership'
    { securityGroupIdentifier = Core.Nothing,
      status = Core.Nothing
    }

-- | The unique ID for this security group.
--
-- /Note:/ Consider using 'securityGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgmSecurityGroupIdentifier :: Lens.Lens' SecurityGroupMembership (Core.Maybe Types.String)
sgmSecurityGroupIdentifier = Lens.field @"securityGroupIdentifier"
{-# DEPRECATED sgmSecurityGroupIdentifier "Use generic-lens or generic-optics with 'securityGroupIdentifier' instead." #-}

-- | The status of this security group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgmStatus :: Lens.Lens' SecurityGroupMembership (Core.Maybe Types.String)
sgmStatus = Lens.field @"status"
{-# DEPRECATED sgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON SecurityGroupMembership where
  parseJSON =
    Core.withObject "SecurityGroupMembership" Core.$
      \x ->
        SecurityGroupMembership'
          Core.<$> (x Core..:? "SecurityGroupIdentifier")
          Core.<*> (x Core..:? "Status")
