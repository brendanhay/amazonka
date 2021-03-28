{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GroupIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.GroupIdentifier
  ( GroupIdentifier (..)
  -- * Smart constructor
  , mkGroupIdentifier
  -- * Lenses
  , giGroupId
  , giGroupName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a security group.
--
-- /See:/ 'mkGroupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { groupId :: Core.Maybe Core.Text
    -- ^ The ID of the security group.
  , groupName :: Core.Maybe Core.Text
    -- ^ The name of the security group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupIdentifier' value with any optional fields omitted.
mkGroupIdentifier
    :: GroupIdentifier
mkGroupIdentifier
  = GroupIdentifier'{groupId = Core.Nothing,
                     groupName = Core.Nothing}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGroupId :: Lens.Lens' GroupIdentifier (Core.Maybe Core.Text)
giGroupId = Lens.field @"groupId"
{-# INLINEABLE giGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGroupName :: Lens.Lens' GroupIdentifier (Core.Maybe Core.Text)
giGroupName = Lens.field @"groupName"
{-# INLINEABLE giGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.ToQuery GroupIdentifier where
        toQuery GroupIdentifier{..}
          = Core.maybe Core.mempty (Core.toQueryPair "GroupId") groupId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GroupName") groupName

instance Core.FromXML GroupIdentifier where
        parseXML x
          = GroupIdentifier' Core.<$>
              (x Core..@? "groupId") Core.<*> x Core..@? "groupName"
