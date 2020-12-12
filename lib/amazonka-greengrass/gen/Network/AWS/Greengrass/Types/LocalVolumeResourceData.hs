{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LocalVolumeResourceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LocalVolumeResourceData
  ( LocalVolumeResourceData (..),

    -- * Smart constructor
    mkLocalVolumeResourceData,

    -- * Lenses
    lvrdGroupOwnerSetting,
    lvrdDestinationPath,
    lvrdSourcePath,
  )
where

import Network.AWS.Greengrass.Types.GroupOwnerSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Attributes that define a local volume resource.
--
-- /See:/ 'mkLocalVolumeResourceData' smart constructor.
data LocalVolumeResourceData = LocalVolumeResourceData'
  { groupOwnerSetting ::
      Lude.Maybe GroupOwnerSetting,
    destinationPath :: Lude.Maybe Lude.Text,
    sourcePath :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LocalVolumeResourceData' with the minimum fields required to make a request.
--
-- * 'destinationPath' - The absolute local path of the resource inside the Lambda environment.
-- * 'groupOwnerSetting' - Allows you to configure additional group privileges for the Lambda process. This field is optional.
-- * 'sourcePath' - The local absolute path of the volume resource on the host. The source path for a volume resource type cannot start with ''/sys''.
mkLocalVolumeResourceData ::
  LocalVolumeResourceData
mkLocalVolumeResourceData =
  LocalVolumeResourceData'
    { groupOwnerSetting = Lude.Nothing,
      destinationPath = Lude.Nothing,
      sourcePath = Lude.Nothing
    }

-- | Allows you to configure additional group privileges for the Lambda process. This field is optional.
--
-- /Note:/ Consider using 'groupOwnerSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrdGroupOwnerSetting :: Lens.Lens' LocalVolumeResourceData (Lude.Maybe GroupOwnerSetting)
lvrdGroupOwnerSetting = Lens.lens (groupOwnerSetting :: LocalVolumeResourceData -> Lude.Maybe GroupOwnerSetting) (\s a -> s {groupOwnerSetting = a} :: LocalVolumeResourceData)
{-# DEPRECATED lvrdGroupOwnerSetting "Use generic-lens or generic-optics with 'groupOwnerSetting' instead." #-}

-- | The absolute local path of the resource inside the Lambda environment.
--
-- /Note:/ Consider using 'destinationPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrdDestinationPath :: Lens.Lens' LocalVolumeResourceData (Lude.Maybe Lude.Text)
lvrdDestinationPath = Lens.lens (destinationPath :: LocalVolumeResourceData -> Lude.Maybe Lude.Text) (\s a -> s {destinationPath = a} :: LocalVolumeResourceData)
{-# DEPRECATED lvrdDestinationPath "Use generic-lens or generic-optics with 'destinationPath' instead." #-}

-- | The local absolute path of the volume resource on the host. The source path for a volume resource type cannot start with ''/sys''.
--
-- /Note:/ Consider using 'sourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrdSourcePath :: Lens.Lens' LocalVolumeResourceData (Lude.Maybe Lude.Text)
lvrdSourcePath = Lens.lens (sourcePath :: LocalVolumeResourceData -> Lude.Maybe Lude.Text) (\s a -> s {sourcePath = a} :: LocalVolumeResourceData)
{-# DEPRECATED lvrdSourcePath "Use generic-lens or generic-optics with 'sourcePath' instead." #-}

instance Lude.FromJSON LocalVolumeResourceData where
  parseJSON =
    Lude.withObject
      "LocalVolumeResourceData"
      ( \x ->
          LocalVolumeResourceData'
            Lude.<$> (x Lude..:? "GroupOwnerSetting")
            Lude.<*> (x Lude..:? "DestinationPath")
            Lude.<*> (x Lude..:? "SourcePath")
      )

instance Lude.ToJSON LocalVolumeResourceData where
  toJSON LocalVolumeResourceData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GroupOwnerSetting" Lude..=) Lude.<$> groupOwnerSetting,
            ("DestinationPath" Lude..=) Lude.<$> destinationPath,
            ("SourcePath" Lude..=) Lude.<$> sourcePath
          ]
      )
