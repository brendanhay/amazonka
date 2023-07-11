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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.LocalVolumeResourceData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.GroupOwnerSetting
import qualified Amazonka.Prelude as Prelude

-- | Attributes that define a local volume resource.
--
-- /See:/ 'newLocalVolumeResourceData' smart constructor.
data LocalVolumeResourceData = LocalVolumeResourceData'
  { -- | The absolute local path of the resource inside the Lambda environment.
    destinationPath :: Prelude.Maybe Prelude.Text,
    -- | Allows you to configure additional group privileges for the Lambda
    -- process. This field is optional.
    groupOwnerSetting :: Prelude.Maybe GroupOwnerSetting,
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
-- 'destinationPath', 'localVolumeResourceData_destinationPath' - The absolute local path of the resource inside the Lambda environment.
--
-- 'groupOwnerSetting', 'localVolumeResourceData_groupOwnerSetting' - Allows you to configure additional group privileges for the Lambda
-- process. This field is optional.
--
-- 'sourcePath', 'localVolumeResourceData_sourcePath' - The local absolute path of the volume resource on the host. The source
-- path for a volume resource type cannot start with \'\'\/sys\'\'.
newLocalVolumeResourceData ::
  LocalVolumeResourceData
newLocalVolumeResourceData =
  LocalVolumeResourceData'
    { destinationPath =
        Prelude.Nothing,
      groupOwnerSetting = Prelude.Nothing,
      sourcePath = Prelude.Nothing
    }

-- | The absolute local path of the resource inside the Lambda environment.
localVolumeResourceData_destinationPath :: Lens.Lens' LocalVolumeResourceData (Prelude.Maybe Prelude.Text)
localVolumeResourceData_destinationPath = Lens.lens (\LocalVolumeResourceData' {destinationPath} -> destinationPath) (\s@LocalVolumeResourceData' {} a -> s {destinationPath = a} :: LocalVolumeResourceData)

-- | Allows you to configure additional group privileges for the Lambda
-- process. This field is optional.
localVolumeResourceData_groupOwnerSetting :: Lens.Lens' LocalVolumeResourceData (Prelude.Maybe GroupOwnerSetting)
localVolumeResourceData_groupOwnerSetting = Lens.lens (\LocalVolumeResourceData' {groupOwnerSetting} -> groupOwnerSetting) (\s@LocalVolumeResourceData' {} a -> s {groupOwnerSetting = a} :: LocalVolumeResourceData)

-- | The local absolute path of the volume resource on the host. The source
-- path for a volume resource type cannot start with \'\'\/sys\'\'.
localVolumeResourceData_sourcePath :: Lens.Lens' LocalVolumeResourceData (Prelude.Maybe Prelude.Text)
localVolumeResourceData_sourcePath = Lens.lens (\LocalVolumeResourceData' {sourcePath} -> sourcePath) (\s@LocalVolumeResourceData' {} a -> s {sourcePath = a} :: LocalVolumeResourceData)

instance Data.FromJSON LocalVolumeResourceData where
  parseJSON =
    Data.withObject
      "LocalVolumeResourceData"
      ( \x ->
          LocalVolumeResourceData'
            Prelude.<$> (x Data..:? "DestinationPath")
            Prelude.<*> (x Data..:? "GroupOwnerSetting")
            Prelude.<*> (x Data..:? "SourcePath")
      )

instance Prelude.Hashable LocalVolumeResourceData where
  hashWithSalt _salt LocalVolumeResourceData' {..} =
    _salt
      `Prelude.hashWithSalt` destinationPath
      `Prelude.hashWithSalt` groupOwnerSetting
      `Prelude.hashWithSalt` sourcePath

instance Prelude.NFData LocalVolumeResourceData where
  rnf LocalVolumeResourceData' {..} =
    Prelude.rnf destinationPath
      `Prelude.seq` Prelude.rnf groupOwnerSetting
      `Prelude.seq` Prelude.rnf sourcePath

instance Data.ToJSON LocalVolumeResourceData where
  toJSON LocalVolumeResourceData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationPath" Data..=)
              Prelude.<$> destinationPath,
            ("GroupOwnerSetting" Data..=)
              Prelude.<$> groupOwnerSetting,
            ("SourcePath" Data..=) Prelude.<$> sourcePath
          ]
      )
