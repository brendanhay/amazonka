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
    ogmStatus,
    ogmOptionGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information on the option groups the DB instance is a member of.
--
-- /See:/ 'mkOptionGroupMembership' smart constructor.
data OptionGroupMembership = OptionGroupMembership'
  { status ::
      Lude.Maybe Lude.Text,
    optionGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptionGroupMembership' with the minimum fields required to make a request.
--
-- * 'optionGroupName' - The name of the option group that the instance belongs to.
-- * 'status' - The status of the DB instance's option group membership. Valid values are: @in-sync@ , @pending-apply@ , @pending-removal@ , @pending-maintenance-apply@ , @pending-maintenance-removal@ , @applying@ , @removing@ , and @failed@ .
mkOptionGroupMembership ::
  OptionGroupMembership
mkOptionGroupMembership =
  OptionGroupMembership'
    { status = Lude.Nothing,
      optionGroupName = Lude.Nothing
    }

-- | The status of the DB instance's option group membership. Valid values are: @in-sync@ , @pending-apply@ , @pending-removal@ , @pending-maintenance-apply@ , @pending-maintenance-removal@ , @applying@ , @removing@ , and @failed@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogmStatus :: Lens.Lens' OptionGroupMembership (Lude.Maybe Lude.Text)
ogmStatus = Lens.lens (status :: OptionGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: OptionGroupMembership)
{-# DEPRECATED ogmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the option group that the instance belongs to.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogmOptionGroupName :: Lens.Lens' OptionGroupMembership (Lude.Maybe Lude.Text)
ogmOptionGroupName = Lens.lens (optionGroupName :: OptionGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: OptionGroupMembership)
{-# DEPRECATED ogmOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

instance Lude.FromXML OptionGroupMembership where
  parseXML x =
    OptionGroupMembership'
      Lude.<$> (x Lude..@? "Status") Lude.<*> (x Lude..@? "OptionGroupName")
