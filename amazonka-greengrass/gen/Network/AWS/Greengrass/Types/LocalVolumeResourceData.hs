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
-- Module      : Network.AWS.Greengrass.Types.LocalVolumeResourceData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LocalVolumeResourceData where

import Network.AWS.Greengrass.Types.GroupOwnerSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Attributes that define a local volume resource.
--
-- /See:/ 'newLocalVolumeResourceData' smart constructor.
data LocalVolumeResourceData = LocalVolumeResourceData'
  { -- | The absolute local path of the resource inside the Lambda environment.
    destinationPath :: Prelude.Maybe Prelude.Text,
    -- | The local absolute path of the volume resource on the host. The source
    -- path for a volume resource type cannot start with \'\'\/sys\'\'.
    sourcePath :: Prelude.Maybe Prelude.Text,
    -- | Allows you to configure additional group privileges for the Lambda
    -- process. This field is optional.
    groupOwnerSetting :: Prelude.Maybe GroupOwnerSetting
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LocalVolumeResourceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPath', 'localVolumeResourceData_destinationPath' - The absolute local path of the resource inside the Lambda environment.
--
-- 'sourcePath', 'localVolumeResourceData_sourcePath' - The local absolute path of the volume resource on the host. The source
-- path for a volume resource type cannot start with \'\'\/sys\'\'.
--
-- 'groupOwnerSetting', 'localVolumeResourceData_groupOwnerSetting' - Allows you to configure additional group privileges for the Lambda
-- process. This field is optional.
newLocalVolumeResourceData ::
  LocalVolumeResourceData
newLocalVolumeResourceData =
  LocalVolumeResourceData'
    { destinationPath =
        Prelude.Nothing,
      sourcePath = Prelude.Nothing,
      groupOwnerSetting = Prelude.Nothing
    }

-- | The absolute local path of the resource inside the Lambda environment.
localVolumeResourceData_destinationPath :: Lens.Lens' LocalVolumeResourceData (Prelude.Maybe Prelude.Text)
localVolumeResourceData_destinationPath = Lens.lens (\LocalVolumeResourceData' {destinationPath} -> destinationPath) (\s@LocalVolumeResourceData' {} a -> s {destinationPath = a} :: LocalVolumeResourceData)

-- | The local absolute path of the volume resource on the host. The source
-- path for a volume resource type cannot start with \'\'\/sys\'\'.
localVolumeResourceData_sourcePath :: Lens.Lens' LocalVolumeResourceData (Prelude.Maybe Prelude.Text)
localVolumeResourceData_sourcePath = Lens.lens (\LocalVolumeResourceData' {sourcePath} -> sourcePath) (\s@LocalVolumeResourceData' {} a -> s {sourcePath = a} :: LocalVolumeResourceData)

-- | Allows you to configure additional group privileges for the Lambda
-- process. This field is optional.
localVolumeResourceData_groupOwnerSetting :: Lens.Lens' LocalVolumeResourceData (Prelude.Maybe GroupOwnerSetting)
localVolumeResourceData_groupOwnerSetting = Lens.lens (\LocalVolumeResourceData' {groupOwnerSetting} -> groupOwnerSetting) (\s@LocalVolumeResourceData' {} a -> s {groupOwnerSetting = a} :: LocalVolumeResourceData)

instance Prelude.FromJSON LocalVolumeResourceData where
  parseJSON =
    Prelude.withObject
      "LocalVolumeResourceData"
      ( \x ->
          LocalVolumeResourceData'
            Prelude.<$> (x Prelude..:? "DestinationPath")
            Prelude.<*> (x Prelude..:? "SourcePath")
            Prelude.<*> (x Prelude..:? "GroupOwnerSetting")
      )

instance Prelude.Hashable LocalVolumeResourceData

instance Prelude.NFData LocalVolumeResourceData

instance Prelude.ToJSON LocalVolumeResourceData where
  toJSON LocalVolumeResourceData' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DestinationPath" Prelude..=)
              Prelude.<$> destinationPath,
            ("SourcePath" Prelude..=) Prelude.<$> sourcePath,
            ("GroupOwnerSetting" Prelude..=)
              Prelude.<$> groupOwnerSetting
          ]
      )
