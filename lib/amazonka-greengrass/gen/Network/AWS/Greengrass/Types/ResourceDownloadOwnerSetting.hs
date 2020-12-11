-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
  ( ResourceDownloadOwnerSetting (..),

    -- * Smart constructor
    mkResourceDownloadOwnerSetting,

    -- * Lenses
    rdosGroupOwner,
    rdosGroupPermission,
  )
where

import Network.AWS.Greengrass.Types.Permission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The owner setting for downloaded machine learning resources.
--
-- /See:/ 'mkResourceDownloadOwnerSetting' smart constructor.
data ResourceDownloadOwnerSetting = ResourceDownloadOwnerSetting'
  { groupOwner ::
      Lude.Text,
    groupPermission :: Permission
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceDownloadOwnerSetting' with the minimum fields required to make a request.
--
-- * 'groupOwner' - The group owner of the resource. This is the name of an existing Linux OS group on the system or a GID. The group's permissions are added to the Lambda process.
-- * 'groupPermission' - The permissions that the group owner has to the resource. Valid values are ''rw'' (read/write) or ''ro'' (read-only).
mkResourceDownloadOwnerSetting ::
  -- | 'groupOwner'
  Lude.Text ->
  -- | 'groupPermission'
  Permission ->
  ResourceDownloadOwnerSetting
mkResourceDownloadOwnerSetting pGroupOwner_ pGroupPermission_ =
  ResourceDownloadOwnerSetting'
    { groupOwner = pGroupOwner_,
      groupPermission = pGroupPermission_
    }

-- | The group owner of the resource. This is the name of an existing Linux OS group on the system or a GID. The group's permissions are added to the Lambda process.
--
-- /Note:/ Consider using 'groupOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdosGroupOwner :: Lens.Lens' ResourceDownloadOwnerSetting Lude.Text
rdosGroupOwner = Lens.lens (groupOwner :: ResourceDownloadOwnerSetting -> Lude.Text) (\s a -> s {groupOwner = a} :: ResourceDownloadOwnerSetting)
{-# DEPRECATED rdosGroupOwner "Use generic-lens or generic-optics with 'groupOwner' instead." #-}

-- | The permissions that the group owner has to the resource. Valid values are ''rw'' (read/write) or ''ro'' (read-only).
--
-- /Note:/ Consider using 'groupPermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdosGroupPermission :: Lens.Lens' ResourceDownloadOwnerSetting Permission
rdosGroupPermission = Lens.lens (groupPermission :: ResourceDownloadOwnerSetting -> Permission) (\s a -> s {groupPermission = a} :: ResourceDownloadOwnerSetting)
{-# DEPRECATED rdosGroupPermission "Use generic-lens or generic-optics with 'groupPermission' instead." #-}

instance Lude.FromJSON ResourceDownloadOwnerSetting where
  parseJSON =
    Lude.withObject
      "ResourceDownloadOwnerSetting"
      ( \x ->
          ResourceDownloadOwnerSetting'
            Lude.<$> (x Lude..: "GroupOwner") Lude.<*> (x Lude..: "GroupPermission")
      )

instance Lude.ToJSON ResourceDownloadOwnerSetting where
  toJSON ResourceDownloadOwnerSetting' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GroupOwner" Lude..= groupOwner),
            Lude.Just ("GroupPermission" Lude..= groupPermission)
          ]
      )
