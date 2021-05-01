{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Greengrass.Types.GroupOwnerSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Attributes that define a local device resource.
--
-- /See:/ 'newLocalDeviceResourceData' smart constructor.
data LocalDeviceResourceData = LocalDeviceResourceData'
  { -- | The local absolute path of the device resource. The source path for a
    -- device resource can refer only to a character device or block device
    -- under \'\'\/dev\'\'.
    sourcePath :: Prelude.Maybe Prelude.Text,
    -- | Group\/owner related settings for local resources.
    groupOwnerSetting :: Prelude.Maybe GroupOwnerSetting
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { sourcePath =
        Prelude.Nothing,
      groupOwnerSetting = Prelude.Nothing
    }

-- | The local absolute path of the device resource. The source path for a
-- device resource can refer only to a character device or block device
-- under \'\'\/dev\'\'.
localDeviceResourceData_sourcePath :: Lens.Lens' LocalDeviceResourceData (Prelude.Maybe Prelude.Text)
localDeviceResourceData_sourcePath = Lens.lens (\LocalDeviceResourceData' {sourcePath} -> sourcePath) (\s@LocalDeviceResourceData' {} a -> s {sourcePath = a} :: LocalDeviceResourceData)

-- | Group\/owner related settings for local resources.
localDeviceResourceData_groupOwnerSetting :: Lens.Lens' LocalDeviceResourceData (Prelude.Maybe GroupOwnerSetting)
localDeviceResourceData_groupOwnerSetting = Lens.lens (\LocalDeviceResourceData' {groupOwnerSetting} -> groupOwnerSetting) (\s@LocalDeviceResourceData' {} a -> s {groupOwnerSetting = a} :: LocalDeviceResourceData)

instance Prelude.FromJSON LocalDeviceResourceData where
  parseJSON =
    Prelude.withObject
      "LocalDeviceResourceData"
      ( \x ->
          LocalDeviceResourceData'
            Prelude.<$> (x Prelude..:? "SourcePath")
            Prelude.<*> (x Prelude..:? "GroupOwnerSetting")
      )

instance Prelude.Hashable LocalDeviceResourceData

instance Prelude.NFData LocalDeviceResourceData

instance Prelude.ToJSON LocalDeviceResourceData where
  toJSON LocalDeviceResourceData' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SourcePath" Prelude..=) Prelude.<$> sourcePath,
            ("GroupOwnerSetting" Prelude..=)
              Prelude.<$> groupOwnerSetting
          ]
      )
