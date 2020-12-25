{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SecurityGroupIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupIdentifier
  ( SecurityGroupIdentifier (..),

    -- * Smart constructor
    mkSecurityGroupIdentifier,

    -- * Lenses
    sgiGroupId,
    sgiGroupName,
  )
where

import qualified Network.AWS.EC2.Types.GroupId as Types
import qualified Network.AWS.EC2.Types.GroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a security group.
--
-- /See:/ 'mkSecurityGroupIdentifier' smart constructor.
data SecurityGroupIdentifier = SecurityGroupIdentifier'
  { -- | The ID of the security group.
    groupId :: Core.Maybe Types.GroupId,
    -- | The name of the security group.
    groupName :: Core.Maybe Types.GroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityGroupIdentifier' value with any optional fields omitted.
mkSecurityGroupIdentifier ::
  SecurityGroupIdentifier
mkSecurityGroupIdentifier =
  SecurityGroupIdentifier'
    { groupId = Core.Nothing,
      groupName = Core.Nothing
    }

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgiGroupId :: Lens.Lens' SecurityGroupIdentifier (Core.Maybe Types.GroupId)
sgiGroupId = Lens.field @"groupId"
{-# DEPRECATED sgiGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgiGroupName :: Lens.Lens' SecurityGroupIdentifier (Core.Maybe Types.GroupName)
sgiGroupName = Lens.field @"groupName"
{-# DEPRECATED sgiGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Core.FromXML SecurityGroupIdentifier where
  parseXML x =
    SecurityGroupIdentifier'
      Core.<$> (x Core..@? "groupId") Core.<*> (x Core..@? "groupName")
