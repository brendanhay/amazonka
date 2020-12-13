{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupOwnerSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupOwnerSetting
  ( GroupOwnerSetting (..),

    -- * Smart constructor
    mkGroupOwnerSetting,

    -- * Lenses
    gosAutoAddGroupOwner,
    gosGroupOwner,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Group owner related settings for local resources.
--
-- /See:/ 'mkGroupOwnerSetting' smart constructor.
data GroupOwnerSetting = GroupOwnerSetting'
  { -- | If true, AWS IoT Greengrass automatically adds the specified Linux OS group owner of the resource to the Lambda process privileges. Thus the Lambda process will have the file access permissions of the added Linux group.
    autoAddGroupOwner :: Lude.Maybe Lude.Bool,
    -- | The name of the Linux OS group whose privileges will be added to the Lambda process. This field is optional.
    groupOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupOwnerSetting' with the minimum fields required to make a request.
--
-- * 'autoAddGroupOwner' - If true, AWS IoT Greengrass automatically adds the specified Linux OS group owner of the resource to the Lambda process privileges. Thus the Lambda process will have the file access permissions of the added Linux group.
-- * 'groupOwner' - The name of the Linux OS group whose privileges will be added to the Lambda process. This field is optional.
mkGroupOwnerSetting ::
  GroupOwnerSetting
mkGroupOwnerSetting =
  GroupOwnerSetting'
    { autoAddGroupOwner = Lude.Nothing,
      groupOwner = Lude.Nothing
    }

-- | If true, AWS IoT Greengrass automatically adds the specified Linux OS group owner of the resource to the Lambda process privileges. Thus the Lambda process will have the file access permissions of the added Linux group.
--
-- /Note:/ Consider using 'autoAddGroupOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosAutoAddGroupOwner :: Lens.Lens' GroupOwnerSetting (Lude.Maybe Lude.Bool)
gosAutoAddGroupOwner = Lens.lens (autoAddGroupOwner :: GroupOwnerSetting -> Lude.Maybe Lude.Bool) (\s a -> s {autoAddGroupOwner = a} :: GroupOwnerSetting)
{-# DEPRECATED gosAutoAddGroupOwner "Use generic-lens or generic-optics with 'autoAddGroupOwner' instead." #-}

-- | The name of the Linux OS group whose privileges will be added to the Lambda process. This field is optional.
--
-- /Note:/ Consider using 'groupOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosGroupOwner :: Lens.Lens' GroupOwnerSetting (Lude.Maybe Lude.Text)
gosGroupOwner = Lens.lens (groupOwner :: GroupOwnerSetting -> Lude.Maybe Lude.Text) (\s a -> s {groupOwner = a} :: GroupOwnerSetting)
{-# DEPRECATED gosGroupOwner "Use generic-lens or generic-optics with 'groupOwner' instead." #-}

instance Lude.FromJSON GroupOwnerSetting where
  parseJSON =
    Lude.withObject
      "GroupOwnerSetting"
      ( \x ->
          GroupOwnerSetting'
            Lude.<$> (x Lude..:? "AutoAddGroupOwner")
            Lude.<*> (x Lude..:? "GroupOwner")
      )

instance Lude.ToJSON GroupOwnerSetting where
  toJSON GroupOwnerSetting' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AutoAddGroupOwner" Lude..=) Lude.<$> autoAddGroupOwner,
            ("GroupOwner" Lude..=) Lude.<$> groupOwner
          ]
      )
