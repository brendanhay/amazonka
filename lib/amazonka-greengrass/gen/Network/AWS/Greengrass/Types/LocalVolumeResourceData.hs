{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LocalVolumeResourceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.LocalVolumeResourceData
  ( LocalVolumeResourceData (..)
  -- * Smart constructor
  , mkLocalVolumeResourceData
  -- * Lenses
  , lvrdDestinationPath
  , lvrdGroupOwnerSetting
  , lvrdSourcePath
  ) where

import qualified Network.AWS.Greengrass.Types.GroupOwnerSetting as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Attributes that define a local volume resource.
--
-- /See:/ 'mkLocalVolumeResourceData' smart constructor.
data LocalVolumeResourceData = LocalVolumeResourceData'
  { destinationPath :: Core.Maybe Core.Text
    -- ^ The absolute local path of the resource inside the Lambda environment.
  , groupOwnerSetting :: Core.Maybe Types.GroupOwnerSetting
    -- ^ Allows you to configure additional group privileges for the Lambda process. This field is optional.
  , sourcePath :: Core.Maybe Core.Text
    -- ^ The local absolute path of the volume resource on the host. The source path for a volume resource type cannot start with ''/sys''.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalVolumeResourceData' value with any optional fields omitted.
mkLocalVolumeResourceData
    :: LocalVolumeResourceData
mkLocalVolumeResourceData
  = LocalVolumeResourceData'{destinationPath = Core.Nothing,
                             groupOwnerSetting = Core.Nothing, sourcePath = Core.Nothing}

-- | The absolute local path of the resource inside the Lambda environment.
--
-- /Note:/ Consider using 'destinationPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrdDestinationPath :: Lens.Lens' LocalVolumeResourceData (Core.Maybe Core.Text)
lvrdDestinationPath = Lens.field @"destinationPath"
{-# INLINEABLE lvrdDestinationPath #-}
{-# DEPRECATED destinationPath "Use generic-lens or generic-optics with 'destinationPath' instead"  #-}

-- | Allows you to configure additional group privileges for the Lambda process. This field is optional.
--
-- /Note:/ Consider using 'groupOwnerSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrdGroupOwnerSetting :: Lens.Lens' LocalVolumeResourceData (Core.Maybe Types.GroupOwnerSetting)
lvrdGroupOwnerSetting = Lens.field @"groupOwnerSetting"
{-# INLINEABLE lvrdGroupOwnerSetting #-}
{-# DEPRECATED groupOwnerSetting "Use generic-lens or generic-optics with 'groupOwnerSetting' instead"  #-}

-- | The local absolute path of the volume resource on the host. The source path for a volume resource type cannot start with ''/sys''.
--
-- /Note:/ Consider using 'sourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrdSourcePath :: Lens.Lens' LocalVolumeResourceData (Core.Maybe Core.Text)
lvrdSourcePath = Lens.field @"sourcePath"
{-# INLINEABLE lvrdSourcePath #-}
{-# DEPRECATED sourcePath "Use generic-lens or generic-optics with 'sourcePath' instead"  #-}

instance Core.FromJSON LocalVolumeResourceData where
        toJSON LocalVolumeResourceData{..}
          = Core.object
              (Core.catMaybes
                 [("DestinationPath" Core..=) Core.<$> destinationPath,
                  ("GroupOwnerSetting" Core..=) Core.<$> groupOwnerSetting,
                  ("SourcePath" Core..=) Core.<$> sourcePath])

instance Core.FromJSON LocalVolumeResourceData where
        parseJSON
          = Core.withObject "LocalVolumeResourceData" Core.$
              \ x ->
                LocalVolumeResourceData' Core.<$>
                  (x Core..:? "DestinationPath") Core.<*>
                    x Core..:? "GroupOwnerSetting"
                    Core.<*> x Core..:? "SourcePath"
