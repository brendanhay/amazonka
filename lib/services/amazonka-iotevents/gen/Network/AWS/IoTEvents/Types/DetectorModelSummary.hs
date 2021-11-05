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
-- Module      : Network.AWS.IoTEvents.Types.DetectorModelSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEvents.Types.DetectorModelSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the detector model.
--
-- /See:/ 'newDetectorModelSummary' smart constructor.
data DetectorModelSummary = DetectorModelSummary'
  { -- | The time the detector model was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the detector model.
    detectorModelName :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the detector model.
    detectorModelDescription :: Prelude.Maybe Prelude.Text
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
-- 'detectorModelName', 'detectorModelSummary_detectorModelName' - The name of the detector model.
--
-- 'detectorModelDescription', 'detectorModelSummary_detectorModelDescription' - A brief description of the detector model.
newDetectorModelSummary ::
  DetectorModelSummary
newDetectorModelSummary =
  DetectorModelSummary'
    { creationTime =
        Prelude.Nothing,
      detectorModelName = Prelude.Nothing,
      detectorModelDescription = Prelude.Nothing
    }

-- | The time the detector model was created.
detectorModelSummary_creationTime :: Lens.Lens' DetectorModelSummary (Prelude.Maybe Prelude.UTCTime)
detectorModelSummary_creationTime = Lens.lens (\DetectorModelSummary' {creationTime} -> creationTime) (\s@DetectorModelSummary' {} a -> s {creationTime = a} :: DetectorModelSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the detector model.
detectorModelSummary_detectorModelName :: Lens.Lens' DetectorModelSummary (Prelude.Maybe Prelude.Text)
detectorModelSummary_detectorModelName = Lens.lens (\DetectorModelSummary' {detectorModelName} -> detectorModelName) (\s@DetectorModelSummary' {} a -> s {detectorModelName = a} :: DetectorModelSummary)

-- | A brief description of the detector model.
detectorModelSummary_detectorModelDescription :: Lens.Lens' DetectorModelSummary (Prelude.Maybe Prelude.Text)
detectorModelSummary_detectorModelDescription = Lens.lens (\DetectorModelSummary' {detectorModelDescription} -> detectorModelDescription) (\s@DetectorModelSummary' {} a -> s {detectorModelDescription = a} :: DetectorModelSummary)

instance Core.FromJSON DetectorModelSummary where
  parseJSON =
    Core.withObject
      "DetectorModelSummary"
      ( \x ->
          DetectorModelSummary'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "detectorModelName")
            Prelude.<*> (x Core..:? "detectorModelDescription")
      )

instance Prelude.Hashable DetectorModelSummary

instance Prelude.NFData DetectorModelSummary
