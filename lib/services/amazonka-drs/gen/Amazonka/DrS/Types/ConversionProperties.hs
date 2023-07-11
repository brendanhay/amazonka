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
-- Module      : Amazonka.DrS.Types.ConversionProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ConversionProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Properties of a conversion job
--
-- /See:/ 'newConversionProperties' smart constructor.
data ConversionProperties = ConversionProperties'
  { -- | The timestamp of when the snapshot being converted was taken
    dataTimestamp :: Prelude.Maybe Prelude.Text,
    -- | Whether the volume being converted uses UEFI or not
    forceUefi :: Prelude.Maybe Prelude.Bool,
    -- | The root volume name of a conversion job
    rootVolumeName :: Prelude.Maybe Prelude.Text,
    -- | A mapping between the volumes being converted and the converted snapshot
    -- ids
    volumeToConversionMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A mapping between the volumes and their sizes
    volumeToVolumeSize :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConversionProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTimestamp', 'conversionProperties_dataTimestamp' - The timestamp of when the snapshot being converted was taken
--
-- 'forceUefi', 'conversionProperties_forceUefi' - Whether the volume being converted uses UEFI or not
--
-- 'rootVolumeName', 'conversionProperties_rootVolumeName' - The root volume name of a conversion job
--
-- 'volumeToConversionMap', 'conversionProperties_volumeToConversionMap' - A mapping between the volumes being converted and the converted snapshot
-- ids
--
-- 'volumeToVolumeSize', 'conversionProperties_volumeToVolumeSize' - A mapping between the volumes and their sizes
newConversionProperties ::
  ConversionProperties
newConversionProperties =
  ConversionProperties'
    { dataTimestamp =
        Prelude.Nothing,
      forceUefi = Prelude.Nothing,
      rootVolumeName = Prelude.Nothing,
      volumeToConversionMap = Prelude.Nothing,
      volumeToVolumeSize = Prelude.Nothing
    }

-- | The timestamp of when the snapshot being converted was taken
conversionProperties_dataTimestamp :: Lens.Lens' ConversionProperties (Prelude.Maybe Prelude.Text)
conversionProperties_dataTimestamp = Lens.lens (\ConversionProperties' {dataTimestamp} -> dataTimestamp) (\s@ConversionProperties' {} a -> s {dataTimestamp = a} :: ConversionProperties)

-- | Whether the volume being converted uses UEFI or not
conversionProperties_forceUefi :: Lens.Lens' ConversionProperties (Prelude.Maybe Prelude.Bool)
conversionProperties_forceUefi = Lens.lens (\ConversionProperties' {forceUefi} -> forceUefi) (\s@ConversionProperties' {} a -> s {forceUefi = a} :: ConversionProperties)

-- | The root volume name of a conversion job
conversionProperties_rootVolumeName :: Lens.Lens' ConversionProperties (Prelude.Maybe Prelude.Text)
conversionProperties_rootVolumeName = Lens.lens (\ConversionProperties' {rootVolumeName} -> rootVolumeName) (\s@ConversionProperties' {} a -> s {rootVolumeName = a} :: ConversionProperties)

-- | A mapping between the volumes being converted and the converted snapshot
-- ids
conversionProperties_volumeToConversionMap :: Lens.Lens' ConversionProperties (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
conversionProperties_volumeToConversionMap = Lens.lens (\ConversionProperties' {volumeToConversionMap} -> volumeToConversionMap) (\s@ConversionProperties' {} a -> s {volumeToConversionMap = a} :: ConversionProperties) Prelude.. Lens.mapping Lens.coerced

-- | A mapping between the volumes and their sizes
conversionProperties_volumeToVolumeSize :: Lens.Lens' ConversionProperties (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural))
conversionProperties_volumeToVolumeSize = Lens.lens (\ConversionProperties' {volumeToVolumeSize} -> volumeToVolumeSize) (\s@ConversionProperties' {} a -> s {volumeToVolumeSize = a} :: ConversionProperties) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ConversionProperties where
  parseJSON =
    Data.withObject
      "ConversionProperties"
      ( \x ->
          ConversionProperties'
            Prelude.<$> (x Data..:? "dataTimestamp")
            Prelude.<*> (x Data..:? "forceUefi")
            Prelude.<*> (x Data..:? "rootVolumeName")
            Prelude.<*> ( x
                            Data..:? "volumeToConversionMap"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "volumeToVolumeSize"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ConversionProperties where
  hashWithSalt _salt ConversionProperties' {..} =
    _salt
      `Prelude.hashWithSalt` dataTimestamp
      `Prelude.hashWithSalt` forceUefi
      `Prelude.hashWithSalt` rootVolumeName
      `Prelude.hashWithSalt` volumeToConversionMap
      `Prelude.hashWithSalt` volumeToVolumeSize

instance Prelude.NFData ConversionProperties where
  rnf ConversionProperties' {..} =
    Prelude.rnf dataTimestamp
      `Prelude.seq` Prelude.rnf forceUefi
      `Prelude.seq` Prelude.rnf rootVolumeName
      `Prelude.seq` Prelude.rnf volumeToConversionMap
      `Prelude.seq` Prelude.rnf volumeToVolumeSize
