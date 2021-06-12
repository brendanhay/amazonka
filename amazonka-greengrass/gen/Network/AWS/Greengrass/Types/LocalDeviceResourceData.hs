{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LocalDeviceResourceData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LocalDeviceResourceData where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.GroupOwnerSetting
import qualified Network.AWS.Lens as Lens

-- | Attributes that define a local device resource.
--
-- /See:/ 'newLocalDeviceResourceData' smart constructor.
data LocalDeviceResourceData = LocalDeviceResourceData'
  { -- | The local absolute path of the device resource. The source path for a
    -- device resource can refer only to a character device or block device
    -- under \'\'\/dev\'\'.
    sourcePath :: Core.Maybe Core.Text,
    -- | Group\/owner related settings for local resources.
    groupOwnerSetting :: Core.Maybe GroupOwnerSetting
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LocalDeviceResourceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePath', 'localDeviceResourceData_sourcePath' - The local absolute path of the device resource. The source path for a
-- device resource can refer only to a character device or block device
-- under \'\'\/dev\'\'.
--
-- 'groupOwnerSetting', 'localDeviceResourceData_groupOwnerSetting' - Group\/owner related settings for local resources.
newLocalDeviceResourceData ::
  LocalDeviceResourceData
newLocalDeviceResourceData =
  LocalDeviceResourceData'
    { sourcePath = Core.Nothing,
      groupOwnerSetting = Core.Nothing
    }

-- | The local absolute path of the device resource. The source path for a
-- device resource can refer only to a character device or block device
-- under \'\'\/dev\'\'.
localDeviceResourceData_sourcePath :: Lens.Lens' LocalDeviceResourceData (Core.Maybe Core.Text)
localDeviceResourceData_sourcePath = Lens.lens (\LocalDeviceResourceData' {sourcePath} -> sourcePath) (\s@LocalDeviceResourceData' {} a -> s {sourcePath = a} :: LocalDeviceResourceData)

-- | Group\/owner related settings for local resources.
localDeviceResourceData_groupOwnerSetting :: Lens.Lens' LocalDeviceResourceData (Core.Maybe GroupOwnerSetting)
localDeviceResourceData_groupOwnerSetting = Lens.lens (\LocalDeviceResourceData' {groupOwnerSetting} -> groupOwnerSetting) (\s@LocalDeviceResourceData' {} a -> s {groupOwnerSetting = a} :: LocalDeviceResourceData)

instance Core.FromJSON LocalDeviceResourceData where
  parseJSON =
    Core.withObject
      "LocalDeviceResourceData"
      ( \x ->
          LocalDeviceResourceData'
            Core.<$> (x Core..:? "SourcePath")
            Core.<*> (x Core..:? "GroupOwnerSetting")
      )

instance Core.Hashable LocalDeviceResourceData

instance Core.NFData LocalDeviceResourceData

instance Core.ToJSON LocalDeviceResourceData where
  toJSON LocalDeviceResourceData' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SourcePath" Core..=) Core.<$> sourcePath,
            ("GroupOwnerSetting" Core..=)
              Core.<$> groupOwnerSetting
          ]
      )
