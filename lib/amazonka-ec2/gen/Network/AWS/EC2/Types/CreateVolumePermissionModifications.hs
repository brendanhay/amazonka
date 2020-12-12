{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateVolumePermissionModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateVolumePermissionModifications
  ( CreateVolumePermissionModifications (..),

    -- * Smart constructor
    mkCreateVolumePermissionModifications,

    -- * Lenses
    cvpmRemove,
    cvpmAdd,
  )
where

import Network.AWS.EC2.Types.CreateVolumePermission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes modifications to the list of create volume permissions for a volume.
--
-- /See:/ 'mkCreateVolumePermissionModifications' smart constructor.
data CreateVolumePermissionModifications = CreateVolumePermissionModifications'
  { remove ::
      Lude.Maybe
        [CreateVolumePermission],
    add ::
      Lude.Maybe
        [CreateVolumePermission]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVolumePermissionModifications' with the minimum fields required to make a request.
--
-- * 'add' - Adds the specified AWS account ID or group to the list.
-- * 'remove' - Removes the specified AWS account ID or group from the list.
mkCreateVolumePermissionModifications ::
  CreateVolumePermissionModifications
mkCreateVolumePermissionModifications =
  CreateVolumePermissionModifications'
    { remove = Lude.Nothing,
      add = Lude.Nothing
    }

-- | Removes the specified AWS account ID or group from the list.
--
-- /Note:/ Consider using 'remove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpmRemove :: Lens.Lens' CreateVolumePermissionModifications (Lude.Maybe [CreateVolumePermission])
cvpmRemove = Lens.lens (remove :: CreateVolumePermissionModifications -> Lude.Maybe [CreateVolumePermission]) (\s a -> s {remove = a} :: CreateVolumePermissionModifications)
{-# DEPRECATED cvpmRemove "Use generic-lens or generic-optics with 'remove' instead." #-}

-- | Adds the specified AWS account ID or group to the list.
--
-- /Note:/ Consider using 'add' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpmAdd :: Lens.Lens' CreateVolumePermissionModifications (Lude.Maybe [CreateVolumePermission])
cvpmAdd = Lens.lens (add :: CreateVolumePermissionModifications -> Lude.Maybe [CreateVolumePermission]) (\s a -> s {add = a} :: CreateVolumePermissionModifications)
{-# DEPRECATED cvpmAdd "Use generic-lens or generic-optics with 'add' instead." #-}

instance Lude.ToQuery CreateVolumePermissionModifications where
  toQuery CreateVolumePermissionModifications' {..} =
    Lude.mconcat
      [ Lude.toQuery (Lude.toQueryList "Remove" Lude.<$> remove),
        Lude.toQuery (Lude.toQueryList "Add" Lude.<$> add)
      ]
