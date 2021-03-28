{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.SecurityGroup
  ( SecurityGroup (..)
  -- * Smart constructor
  , mkSecurityGroup
  -- * Lenses
  , sgGroupId
  , sgGroupName
  ) where

import qualified Network.AWS.Inspector.Types.Text as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a security group associated with a network interface. This data type is used as one of the elements of the 'NetworkInterface' data type.
--
-- /See:/ 'mkSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { groupId :: Core.Maybe Types.Text
    -- ^ The ID of the security group.
  , groupName :: Core.Maybe Types.Text
    -- ^ The name of the security group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityGroup' value with any optional fields omitted.
mkSecurityGroup
    :: SecurityGroup
mkSecurityGroup
  = SecurityGroup'{groupId = Core.Nothing, groupName = Core.Nothing}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupId :: Lens.Lens' SecurityGroup (Core.Maybe Types.Text)
sgGroupId = Lens.field @"groupId"
{-# INLINEABLE sgGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupName :: Lens.Lens' SecurityGroup (Core.Maybe Types.Text)
sgGroupName = Lens.field @"groupName"
{-# INLINEABLE sgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.FromJSON SecurityGroup where
        parseJSON
          = Core.withObject "SecurityGroup" Core.$
              \ x ->
                SecurityGroup' Core.<$>
                  (x Core..:? "groupId") Core.<*> x Core..:? "groupName"
