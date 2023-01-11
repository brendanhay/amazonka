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
-- Module      : Amazonka.IoTEvents.Types.DetectorDebugOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.DetectorDebugOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The detector model and the specific detectors (instances) for which the
-- logging level is given.
--
-- /See:/ 'newDetectorDebugOption' smart constructor.
data DetectorDebugOption = DetectorDebugOption'
  { -- | The value of the input attribute key used to create the detector (the
    -- instance of the detector model).
    keyValue :: Prelude.Maybe Prelude.Text,
    -- | The name of the detector model.
    detectorModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorDebugOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyValue', 'detectorDebugOption_keyValue' - The value of the input attribute key used to create the detector (the
-- instance of the detector model).
--
-- 'detectorModelName', 'detectorDebugOption_detectorModelName' - The name of the detector model.
newDetectorDebugOption ::
  -- | 'detectorModelName'
  Prelude.Text ->
  DetectorDebugOption
newDetectorDebugOption pDetectorModelName_ =
  DetectorDebugOption'
    { keyValue = Prelude.Nothing,
      detectorModelName = pDetectorModelName_
    }

-- | The value of the input attribute key used to create the detector (the
-- instance of the detector model).
detectorDebugOption_keyValue :: Lens.Lens' DetectorDebugOption (Prelude.Maybe Prelude.Text)
detectorDebugOption_keyValue = Lens.lens (\DetectorDebugOption' {keyValue} -> keyValue) (\s@DetectorDebugOption' {} a -> s {keyValue = a} :: DetectorDebugOption)

-- | The name of the detector model.
detectorDebugOption_detectorModelName :: Lens.Lens' DetectorDebugOption Prelude.Text
detectorDebugOption_detectorModelName = Lens.lens (\DetectorDebugOption' {detectorModelName} -> detectorModelName) (\s@DetectorDebugOption' {} a -> s {detectorModelName = a} :: DetectorDebugOption)

instance Data.FromJSON DetectorDebugOption where
  parseJSON =
    Data.withObject
      "DetectorDebugOption"
      ( \x ->
          DetectorDebugOption'
            Prelude.<$> (x Data..:? "keyValue")
            Prelude.<*> (x Data..: "detectorModelName")
      )

instance Prelude.Hashable DetectorDebugOption where
  hashWithSalt _salt DetectorDebugOption' {..} =
    _salt `Prelude.hashWithSalt` keyValue
      `Prelude.hashWithSalt` detectorModelName

instance Prelude.NFData DetectorDebugOption where
  rnf DetectorDebugOption' {..} =
    Prelude.rnf keyValue
      `Prelude.seq` Prelude.rnf detectorModelName

instance Data.ToJSON DetectorDebugOption where
  toJSON DetectorDebugOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keyValue" Data..=) Prelude.<$> keyValue,
            Prelude.Just
              ("detectorModelName" Data..= detectorModelName)
          ]
      )
