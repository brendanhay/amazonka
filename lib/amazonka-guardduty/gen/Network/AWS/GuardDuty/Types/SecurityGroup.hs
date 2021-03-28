{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.SecurityGroup
  ( SecurityGroup (..)
  -- * Smart constructor
  , mkSecurityGroup
  -- * Lenses
  , sgGroupId
  , sgGroupName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the security groups associated with the EC2 instance.
--
-- /See:/ 'mkSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { groupId :: Core.Maybe Core.Text
    -- ^ The security group ID of the EC2 instance.
  , groupName :: Core.Maybe Core.Text
    -- ^ The security group name of the EC2 instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityGroup' value with any optional fields omitted.
mkSecurityGroup
    :: SecurityGroup
mkSecurityGroup
  = SecurityGroup'{groupId = Core.Nothing, groupName = Core.Nothing}

-- | The security group ID of the EC2 instance.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupId :: Lens.Lens' SecurityGroup (Core.Maybe Core.Text)
sgGroupId = Lens.field @"groupId"
{-# INLINEABLE sgGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The security group name of the EC2 instance.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupName :: Lens.Lens' SecurityGroup (Core.Maybe Core.Text)
sgGroupName = Lens.field @"groupName"
{-# INLINEABLE sgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.FromJSON SecurityGroup where
        parseJSON
          = Core.withObject "SecurityGroup" Core.$
              \ x ->
                SecurityGroup' Core.<$>
                  (x Core..:? "groupId") Core.<*> x Core..:? "groupName"
