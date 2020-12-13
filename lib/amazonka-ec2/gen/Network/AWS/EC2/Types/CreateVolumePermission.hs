{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateVolumePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateVolumePermission
  ( CreateVolumePermission (..),

    -- * Smart constructor
    mkCreateVolumePermission,

    -- * Lenses
    cvpGroup,
    cvpUserId,
  )
where

import Network.AWS.EC2.Types.PermissionGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the user or group to be added or removed from the list of create volume permissions for a volume.
--
-- /See:/ 'mkCreateVolumePermission' smart constructor.
data CreateVolumePermission = CreateVolumePermission'
  { -- | The group to be added or removed. The possible value is @all@ .
    group :: Lude.Maybe PermissionGroup,
    -- | The AWS account ID to be added or removed.
    userId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVolumePermission' with the minimum fields required to make a request.
--
-- * 'group' - The group to be added or removed. The possible value is @all@ .
-- * 'userId' - The AWS account ID to be added or removed.
mkCreateVolumePermission ::
  CreateVolumePermission
mkCreateVolumePermission =
  CreateVolumePermission'
    { group = Lude.Nothing,
      userId = Lude.Nothing
    }

-- | The group to be added or removed. The possible value is @all@ .
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpGroup :: Lens.Lens' CreateVolumePermission (Lude.Maybe PermissionGroup)
cvpGroup = Lens.lens (group :: CreateVolumePermission -> Lude.Maybe PermissionGroup) (\s a -> s {group = a} :: CreateVolumePermission)
{-# DEPRECATED cvpGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The AWS account ID to be added or removed.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpUserId :: Lens.Lens' CreateVolumePermission (Lude.Maybe Lude.Text)
cvpUserId = Lens.lens (userId :: CreateVolumePermission -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: CreateVolumePermission)
{-# DEPRECATED cvpUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.FromXML CreateVolumePermission where
  parseXML x =
    CreateVolumePermission'
      Lude.<$> (x Lude..@? "group") Lude.<*> (x Lude..@? "userId")

instance Lude.ToQuery CreateVolumePermission where
  toQuery CreateVolumePermission' {..} =
    Lude.mconcat ["Group" Lude.=: group, "UserId" Lude.=: userId]
