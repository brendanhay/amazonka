{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LocalDeviceResourceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.LocalDeviceResourceData
  ( LocalDeviceResourceData (..)
  -- * Smart constructor
  , mkLocalDeviceResourceData
  -- * Lenses
  , ldrdGroupOwnerSetting
  , ldrdSourcePath
  ) where

import qualified Network.AWS.Greengrass.Types.GroupOwnerSetting as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Attributes that define a local device resource.
--
-- /See:/ 'mkLocalDeviceResourceData' smart constructor.
data LocalDeviceResourceData = LocalDeviceResourceData'
  { groupOwnerSetting :: Core.Maybe Types.GroupOwnerSetting
    -- ^ Group/owner related settings for local resources.
  , sourcePath :: Core.Maybe Core.Text
    -- ^ The local absolute path of the device resource. The source path for a device resource can refer only to a character device or block device under ''/dev''.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalDeviceResourceData' value with any optional fields omitted.
mkLocalDeviceResourceData
    :: LocalDeviceResourceData
mkLocalDeviceResourceData
  = LocalDeviceResourceData'{groupOwnerSetting = Core.Nothing,
                             sourcePath = Core.Nothing}

-- | Group/owner related settings for local resources.
--
-- /Note:/ Consider using 'groupOwnerSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrdGroupOwnerSetting :: Lens.Lens' LocalDeviceResourceData (Core.Maybe Types.GroupOwnerSetting)
ldrdGroupOwnerSetting = Lens.field @"groupOwnerSetting"
{-# INLINEABLE ldrdGroupOwnerSetting #-}
{-# DEPRECATED groupOwnerSetting "Use generic-lens or generic-optics with 'groupOwnerSetting' instead"  #-}

-- | The local absolute path of the device resource. The source path for a device resource can refer only to a character device or block device under ''/dev''.
--
-- /Note:/ Consider using 'sourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrdSourcePath :: Lens.Lens' LocalDeviceResourceData (Core.Maybe Core.Text)
ldrdSourcePath = Lens.field @"sourcePath"
{-# INLINEABLE ldrdSourcePath #-}
{-# DEPRECATED sourcePath "Use generic-lens or generic-optics with 'sourcePath' instead"  #-}

instance Core.FromJSON LocalDeviceResourceData where
        toJSON LocalDeviceResourceData{..}
          = Core.object
              (Core.catMaybes
                 [("GroupOwnerSetting" Core..=) Core.<$> groupOwnerSetting,
                  ("SourcePath" Core..=) Core.<$> sourcePath])

instance Core.FromJSON LocalDeviceResourceData where
        parseJSON
          = Core.withObject "LocalDeviceResourceData" Core.$
              \ x ->
                LocalDeviceResourceData' Core.<$>
                  (x Core..:? "GroupOwnerSetting") Core.<*> x Core..:? "SourcePath"
