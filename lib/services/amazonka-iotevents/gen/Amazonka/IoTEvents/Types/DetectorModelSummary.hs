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
-- Module      : Amazonka.IoTEvents.Types.DetectorModelSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.DetectorModelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the detector model.
--
-- /See:/ 'newDetectorModelSummary' smart constructor.
data DetectorModelSummary = DetectorModelSummary'
  { -- | The time the detector model was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A brief description of the detector model.
    detectorModelDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the detector model.
    detectorModelName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorModelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'detectorModelSummary_creationTime' - The time the detector model was created.
--
-- 'detectorModelDescription', 'detectorModelSummary_detectorModelDescription' - A brief description of the detector model.
--
-- 'detectorModelName', 'detectorModelSummary_detectorModelName' - The name of the detector model.
newDetectorModelSummary ::
  DetectorModelSummary
newDetectorModelSummary =
  DetectorModelSummary'
    { creationTime =
        Prelude.Nothing,
      detectorModelDescription = Prelude.Nothing,
      detectorModelName = Prelude.Nothing
    }

-- | The time the detector model was created.
detectorModelSummary_creationTime :: Lens.Lens' DetectorModelSummary (Prelude.Maybe Prelude.UTCTime)
detectorModelSummary_creationTime = Lens.lens (\DetectorModelSummary' {creationTime} -> creationTime) (\s@DetectorModelSummary' {} a -> s {creationTime = a} :: DetectorModelSummary) Prelude.. Lens.mapping Data._Time

-- | A brief description of the detector model.
detectorModelSummary_detectorModelDescription :: Lens.Lens' DetectorModelSummary (Prelude.Maybe Prelude.Text)
detectorModelSummary_detectorModelDescription = Lens.lens (\DetectorModelSummary' {detectorModelDescription} -> detectorModelDescription) (\s@DetectorModelSummary' {} a -> s {detectorModelDescription = a} :: DetectorModelSummary)

-- | The name of the detector model.
detectorModelSummary_detectorModelName :: Lens.Lens' DetectorModelSummary (Prelude.Maybe Prelude.Text)
detectorModelSummary_detectorModelName = Lens.lens (\DetectorModelSummary' {detectorModelName} -> detectorModelName) (\s@DetectorModelSummary' {} a -> s {detectorModelName = a} :: DetectorModelSummary)

instance Data.FromJSON DetectorModelSummary where
  parseJSON =
    Data.withObject
      "DetectorModelSummary"
      ( \x ->
          DetectorModelSummary'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "detectorModelDescription")
            Prelude.<*> (x Data..:? "detectorModelName")
      )

instance Prelude.Hashable DetectorModelSummary where
  hashWithSalt _salt DetectorModelSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` detectorModelDescription
      `Prelude.hashWithSalt` detectorModelName

instance Prelude.NFData DetectorModelSummary where
  rnf DetectorModelSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf detectorModelDescription
      `Prelude.seq` Prelude.rnf detectorModelName
