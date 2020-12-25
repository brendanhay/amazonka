{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.SourceSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.SourceSecurityGroup
  ( SourceSecurityGroup (..),

    -- * Smart constructor
    mkSourceSecurityGroup,

    -- * Lenses
    ssgGroupName,
    ssgOwnerAlias,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.GroupName as Types
import qualified Network.AWS.ELB.Types.OwnerAlias as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a source security group.
--
-- /See:/ 'mkSourceSecurityGroup' smart constructor.
data SourceSecurityGroup = SourceSecurityGroup'
  { -- | The name of the security group.
    groupName :: Core.Maybe Types.GroupName,
    -- | The owner of the security group.
    ownerAlias :: Core.Maybe Types.OwnerAlias
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceSecurityGroup' value with any optional fields omitted.
mkSourceSecurityGroup ::
  SourceSecurityGroup
mkSourceSecurityGroup =
  SourceSecurityGroup'
    { groupName = Core.Nothing,
      ownerAlias = Core.Nothing
    }

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgGroupName :: Lens.Lens' SourceSecurityGroup (Core.Maybe Types.GroupName)
ssgGroupName = Lens.field @"groupName"
{-# DEPRECATED ssgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The owner of the security group.
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgOwnerAlias :: Lens.Lens' SourceSecurityGroup (Core.Maybe Types.OwnerAlias)
ssgOwnerAlias = Lens.field @"ownerAlias"
{-# DEPRECATED ssgOwnerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead." #-}

instance Core.FromXML SourceSecurityGroup where
  parseXML x =
    SourceSecurityGroup'
      Core.<$> (x Core..@? "GroupName") Core.<*> (x Core..@? "OwnerAlias")
