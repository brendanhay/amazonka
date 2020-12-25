{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionGroupMembership
  ( OptionGroupMembership (..),

    -- * Smart constructor
    mkOptionGroupMembership,

    -- * Lenses
    ogmOptionGroupName,
    ogmStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | Provides information on the option groups the DB instance is a member of.
--
-- /See:/ 'mkOptionGroupMembership' smart constructor.
data OptionGroupMembership = OptionGroupMembership'
  { -- | The name of the option group that the instance belongs to.
    optionGroupName :: Core.Maybe Types.String,
    -- | The status of the DB instance's option group membership. Valid values are: @in-sync@ , @pending-apply@ , @pending-removal@ , @pending-maintenance-apply@ , @pending-maintenance-removal@ , @applying@ , @removing@ , and @failed@ .
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OptionGroupMembership' value with any optional fields omitted.
mkOptionGroupMembership ::
  OptionGroupMembership
mkOptionGroupMembership =
  OptionGroupMembership'
    { optionGroupName = Core.Nothing,
      status = Core.Nothing
    }

-- | The name of the option group that the instance belongs to.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogmOptionGroupName :: Lens.Lens' OptionGroupMembership (Core.Maybe Types.String)
ogmOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED ogmOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The status of the DB instance's option group membership. Valid values are: @in-sync@ , @pending-apply@ , @pending-removal@ , @pending-maintenance-apply@ , @pending-maintenance-removal@ , @applying@ , @removing@ , and @failed@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogmStatus :: Lens.Lens' OptionGroupMembership (Core.Maybe Types.String)
ogmStatus = Lens.field @"status"
{-# DEPRECATED ogmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML OptionGroupMembership where
  parseXML x =
    OptionGroupMembership'
      Core.<$> (x Core..@? "OptionGroupName") Core.<*> (x Core..@? "Status")
