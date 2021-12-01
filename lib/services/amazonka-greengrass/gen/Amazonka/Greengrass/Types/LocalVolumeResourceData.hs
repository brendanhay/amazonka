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
-- Module      : Amazonka.Greengrass.Types.LocalVolumeResourceData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.LocalVolumeResourceData where

import qualified Amazonka.Core as Core
import Amazonka.Greengrass.Types.GroupOwnerSetting
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Attributes that define a local volume resource.
--
-- /See:/ 'newLocalVolumeResourceData' smart constructor.
data LocalVolumeResourceData = LocalVolumeResourceData'
  { -- | Allows you to configure additional group privileges for the Lambda
    -- process. This field is optional.
    groupOwnerSetting :: Prelude.Maybe GroupOwnerSetting,
    -- | The absolute local path of the resource inside the Lambda environment.
    destinationPath :: Prelude.Maybe Prelude.Text,
    -- | The local absolute path of the volume resource on the host. The source
    -- path for a volume resource type cannot start with \'\'\/sys\'\'.
    sourcePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalVolumeResourceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupOwnerSetting', 'localVolumeResourceData_groupOwnerSetting' - Allows you to configure additional group privileges for the Lambda
-- process. This field is optional.
--
-- 'destinationPath', 'localVolumeResourceData_destinationPath' - The absolute local path of the resource inside the Lambda environment.
--
-- 'sourcePath', 'localVolumeResourceData_sourcePath' - The local absolute path of the volume resource on the host. The source
-- path for a volume resource type cannot start with \'\'\/sys\'\'.
newLocalVolumeResourceData ::
  LocalVolumeResourceData
newLocalVolumeResourceData =
  LocalVolumeResourceData'
    { groupOwnerSetting =
        Prelude.Nothing,
      destinationPath = Prelude.Nothing,
      sourcePath = Prelude.Nothing
    }

-- | Allows you to configure additional group privileges for the Lambda
-- process. This field is optional.
localVolumeResourceData_groupOwnerSetting :: Lens.Lens' LocalVolumeResourceData (Prelude.Maybe GroupOwnerSetting)
localVolumeResourceData_groupOwnerSetting = Lens.lens (\LocalVolumeResourceData' {groupOwnerSetting} -> groupOwnerSetting) (\s@LocalVolumeResourceData' {} a -> s {groupOwnerSetting = a} :: LocalVolumeResourceData)

-- | The absolute local path of the resource inside the Lambda environment.
localVolumeResourceData_destinationPath :: Lens.Lens' LocalVolumeResourceData (Prelude.Maybe Prelude.Text)
localVolumeResourceData_destinationPath = Lens.lens (\LocalVolumeResourceData' {destinationPath} -> destinationPath) (\s@LocalVolumeResourceData' {} a -> s {destinationPath = a} :: LocalVolumeResourceData)

-- | The local absolute path of the volume resource on the host. The source
-- path for a volume resource type cannot start with \'\'\/sys\'\'.
localVolumeResourceData_sourcePath :: Lens.Lens' LocalVolumeResourceData (Prelude.Maybe Prelude.Text)
localVolumeResourceData_sourcePath = Lens.lens (\LocalVolumeResourceData' {sourcePath} -> sourcePath) (\s@LocalVolumeResourceData' {} a -> s {sourcePath = a} :: LocalVolumeResourceData)

instance Core.FromJSON LocalVolumeResourceData where
  parseJSON =
    Core.withObject
      "LocalVolumeResourceData"
      ( \x ->
          LocalVolumeResourceData'
            Prelude.<$> (x Core..:? "GroupOwnerSetting")
            Prelude.<*> (x Core..:? "DestinationPath")
            Prelude.<*> (x Core..:? "SourcePath")
      )

instance Prelude.Hashable LocalVolumeResourceData where
  hashWithSalt salt' LocalVolumeResourceData' {..} =
    salt' `Prelude.hashWithSalt` sourcePath
      `Prelude.hashWithSalt` destinationPath
      `Prelude.hashWithSalt` groupOwnerSetting

instance Prelude.NFData LocalVolumeResourceData where
  rnf LocalVolumeResourceData' {..} =
    Prelude.rnf groupOwnerSetting
      `Prelude.seq` Prelude.rnf sourcePath
      `Prelude.seq` Prelude.rnf destinationPath

instance Core.ToJSON LocalVolumeResourceData where
  toJSON LocalVolumeResourceData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GroupOwnerSetting" Core..=)
              Prelude.<$> groupOwnerSetting,
            ("DestinationPath" Core..=)
              Prelude.<$> destinationPath,
            ("SourcePath" Core..=) Prelude.<$> sourcePath
          ]
      )
