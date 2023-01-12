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
-- Module      : Amazonka.FraudDetector.Types.Detector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.Detector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The detector.
--
-- /See:/ 'newDetector' smart constructor.
data Detector = Detector'
  { -- | The detector ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the detector was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The detector description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The detector ID.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The name of the event type.
    eventTypeName :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the detector was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Detector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'detector_arn' - The detector ARN.
--
-- 'createdTime', 'detector_createdTime' - Timestamp of when the detector was created.
--
-- 'description', 'detector_description' - The detector description.
--
-- 'detectorId', 'detector_detectorId' - The detector ID.
--
-- 'eventTypeName', 'detector_eventTypeName' - The name of the event type.
--
-- 'lastUpdatedTime', 'detector_lastUpdatedTime' - Timestamp of when the detector was last updated.
newDetector ::
  Detector
newDetector =
  Detector'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      eventTypeName = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing
    }

-- | The detector ARN.
detector_arn :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_arn = Lens.lens (\Detector' {arn} -> arn) (\s@Detector' {} a -> s {arn = a} :: Detector)

-- | Timestamp of when the detector was created.
detector_createdTime :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_createdTime = Lens.lens (\Detector' {createdTime} -> createdTime) (\s@Detector' {} a -> s {createdTime = a} :: Detector)

-- | The detector description.
detector_description :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_description = Lens.lens (\Detector' {description} -> description) (\s@Detector' {} a -> s {description = a} :: Detector)

-- | The detector ID.
detector_detectorId :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_detectorId = Lens.lens (\Detector' {detectorId} -> detectorId) (\s@Detector' {} a -> s {detectorId = a} :: Detector)

-- | The name of the event type.
detector_eventTypeName :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_eventTypeName = Lens.lens (\Detector' {eventTypeName} -> eventTypeName) (\s@Detector' {} a -> s {eventTypeName = a} :: Detector)

-- | Timestamp of when the detector was last updated.
detector_lastUpdatedTime :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_lastUpdatedTime = Lens.lens (\Detector' {lastUpdatedTime} -> lastUpdatedTime) (\s@Detector' {} a -> s {lastUpdatedTime = a} :: Detector)

instance Data.FromJSON Detector where
  parseJSON =
    Data.withObject
      "Detector"
      ( \x ->
          Detector'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "detectorId")
            Prelude.<*> (x Data..:? "eventTypeName")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
      )

instance Prelude.Hashable Detector where
  hashWithSalt _salt Detector' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` lastUpdatedTime

instance Prelude.NFData Detector where
  rnf Detector' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf eventTypeName
      `Prelude.seq` Prelude.rnf lastUpdatedTime
