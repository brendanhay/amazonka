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
-- Module      : Amazonka.IoTEvents.Types.DetectorModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.DetectorModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.DetectorModelConfiguration
import Amazonka.IoTEvents.Types.DetectorModelDefinition
import qualified Amazonka.Prelude as Prelude

-- | Information about the detector model.
--
-- /See:/ 'newDetectorModel' smart constructor.
data DetectorModel = DetectorModel'
  { -- | Information that defines how a detector operates.
    detectorModelDefinition :: Prelude.Maybe DetectorModelDefinition,
    -- | Information about how the detector is configured.
    detectorModelConfiguration :: Prelude.Maybe DetectorModelConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorModelDefinition', 'detectorModel_detectorModelDefinition' - Information that defines how a detector operates.
--
-- 'detectorModelConfiguration', 'detectorModel_detectorModelConfiguration' - Information about how the detector is configured.
newDetectorModel ::
  DetectorModel
newDetectorModel =
  DetectorModel'
    { detectorModelDefinition =
        Prelude.Nothing,
      detectorModelConfiguration = Prelude.Nothing
    }

-- | Information that defines how a detector operates.
detectorModel_detectorModelDefinition :: Lens.Lens' DetectorModel (Prelude.Maybe DetectorModelDefinition)
detectorModel_detectorModelDefinition = Lens.lens (\DetectorModel' {detectorModelDefinition} -> detectorModelDefinition) (\s@DetectorModel' {} a -> s {detectorModelDefinition = a} :: DetectorModel)

-- | Information about how the detector is configured.
detectorModel_detectorModelConfiguration :: Lens.Lens' DetectorModel (Prelude.Maybe DetectorModelConfiguration)
detectorModel_detectorModelConfiguration = Lens.lens (\DetectorModel' {detectorModelConfiguration} -> detectorModelConfiguration) (\s@DetectorModel' {} a -> s {detectorModelConfiguration = a} :: DetectorModel)

instance Data.FromJSON DetectorModel where
  parseJSON =
    Data.withObject
      "DetectorModel"
      ( \x ->
          DetectorModel'
            Prelude.<$> (x Data..:? "detectorModelDefinition")
            Prelude.<*> (x Data..:? "detectorModelConfiguration")
      )

instance Prelude.Hashable DetectorModel where
  hashWithSalt _salt DetectorModel' {..} =
    _salt
      `Prelude.hashWithSalt` detectorModelDefinition
      `Prelude.hashWithSalt` detectorModelConfiguration

instance Prelude.NFData DetectorModel where
  rnf DetectorModel' {..} =
    Prelude.rnf detectorModelDefinition
      `Prelude.seq` Prelude.rnf detectorModelConfiguration
