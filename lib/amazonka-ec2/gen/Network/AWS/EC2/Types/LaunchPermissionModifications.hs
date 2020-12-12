{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchPermissionModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchPermissionModifications
  ( LaunchPermissionModifications (..),

    -- * Smart constructor
    mkLaunchPermissionModifications,

    -- * Lenses
    lRemove,
    lAdd,
  )
where

import Network.AWS.EC2.Types.LaunchPermission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch permission modification.
--
-- /See:/ 'mkLaunchPermissionModifications' smart constructor.
data LaunchPermissionModifications = LaunchPermissionModifications'
  { remove ::
      Lude.Maybe [LaunchPermission],
    add ::
      Lude.Maybe [LaunchPermission]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchPermissionModifications' with the minimum fields required to make a request.
--
-- * 'add' - The AWS account ID to add to the list of launch permissions for the AMI.
-- * 'remove' - The AWS account ID to remove from the list of launch permissions for the AMI.
mkLaunchPermissionModifications ::
  LaunchPermissionModifications
mkLaunchPermissionModifications =
  LaunchPermissionModifications'
    { remove = Lude.Nothing,
      add = Lude.Nothing
    }

-- | The AWS account ID to remove from the list of launch permissions for the AMI.
--
-- /Note:/ Consider using 'remove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lRemove :: Lens.Lens' LaunchPermissionModifications (Lude.Maybe [LaunchPermission])
lRemove = Lens.lens (remove :: LaunchPermissionModifications -> Lude.Maybe [LaunchPermission]) (\s a -> s {remove = a} :: LaunchPermissionModifications)
{-# DEPRECATED lRemove "Use generic-lens or generic-optics with 'remove' instead." #-}

-- | The AWS account ID to add to the list of launch permissions for the AMI.
--
-- /Note:/ Consider using 'add' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAdd :: Lens.Lens' LaunchPermissionModifications (Lude.Maybe [LaunchPermission])
lAdd = Lens.lens (add :: LaunchPermissionModifications -> Lude.Maybe [LaunchPermission]) (\s a -> s {add = a} :: LaunchPermissionModifications)
{-# DEPRECATED lAdd "Use generic-lens or generic-optics with 'add' instead." #-}

instance Lude.ToQuery LaunchPermissionModifications where
  toQuery LaunchPermissionModifications' {..} =
    Lude.mconcat
      [ Lude.toQuery (Lude.toQueryList "Remove" Lude.<$> remove),
        Lude.toQuery (Lude.toQueryList "Add" Lude.<$> add)
      ]
