{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LocalDeviceResourceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LocalDeviceResourceData
  ( LocalDeviceResourceData (..),

    -- * Smart constructor
    mkLocalDeviceResourceData,

    -- * Lenses
    ldrdGroupOwnerSetting,
    ldrdSourcePath,
  )
where

import Network.AWS.Greengrass.Types.GroupOwnerSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Attributes that define a local device resource.
--
-- /See:/ 'mkLocalDeviceResourceData' smart constructor.
data LocalDeviceResourceData = LocalDeviceResourceData'
  { -- | Group/owner related settings for local resources.
    groupOwnerSetting :: Lude.Maybe GroupOwnerSetting,
    -- | The local absolute path of the device resource. The source path for a device resource can refer only to a character device or block device under ''/dev''.
    sourcePath :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LocalDeviceResourceData' with the minimum fields required to make a request.
--
-- * 'groupOwnerSetting' - Group/owner related settings for local resources.
-- * 'sourcePath' - The local absolute path of the device resource. The source path for a device resource can refer only to a character device or block device under ''/dev''.
mkLocalDeviceResourceData ::
  LocalDeviceResourceData
mkLocalDeviceResourceData =
  LocalDeviceResourceData'
    { groupOwnerSetting = Lude.Nothing,
      sourcePath = Lude.Nothing
    }

-- | Group/owner related settings for local resources.
--
-- /Note:/ Consider using 'groupOwnerSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrdGroupOwnerSetting :: Lens.Lens' LocalDeviceResourceData (Lude.Maybe GroupOwnerSetting)
ldrdGroupOwnerSetting = Lens.lens (groupOwnerSetting :: LocalDeviceResourceData -> Lude.Maybe GroupOwnerSetting) (\s a -> s {groupOwnerSetting = a} :: LocalDeviceResourceData)
{-# DEPRECATED ldrdGroupOwnerSetting "Use generic-lens or generic-optics with 'groupOwnerSetting' instead." #-}

-- | The local absolute path of the device resource. The source path for a device resource can refer only to a character device or block device under ''/dev''.
--
-- /Note:/ Consider using 'sourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrdSourcePath :: Lens.Lens' LocalDeviceResourceData (Lude.Maybe Lude.Text)
ldrdSourcePath = Lens.lens (sourcePath :: LocalDeviceResourceData -> Lude.Maybe Lude.Text) (\s a -> s {sourcePath = a} :: LocalDeviceResourceData)
{-# DEPRECATED ldrdSourcePath "Use generic-lens or generic-optics with 'sourcePath' instead." #-}

instance Lude.FromJSON LocalDeviceResourceData where
  parseJSON =
    Lude.withObject
      "LocalDeviceResourceData"
      ( \x ->
          LocalDeviceResourceData'
            Lude.<$> (x Lude..:? "GroupOwnerSetting")
            Lude.<*> (x Lude..:? "SourcePath")
      )

instance Lude.ToJSON LocalDeviceResourceData where
  toJSON LocalDeviceResourceData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GroupOwnerSetting" Lude..=) Lude.<$> groupOwnerSetting,
            ("SourcePath" Lude..=) Lude.<$> sourcePath
          ]
      )
