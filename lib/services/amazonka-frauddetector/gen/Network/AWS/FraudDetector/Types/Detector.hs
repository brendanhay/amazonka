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
-- Module      : Network.AWS.FraudDetector.Types.Detector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FraudDetector.Types.Detector where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The detector.
--
-- /See:/ 'newDetector' smart constructor.
data Detector = Detector'
  { -- | Timestamp of when the detector was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The detector ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the detector was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the event type.
    eventTypeName :: Prelude.Maybe Prelude.Text,
    -- | The detector ID.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The detector description.
    description :: Prelude.Maybe Prelude.Text
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
-- 'lastUpdatedTime', 'detector_lastUpdatedTime' - Timestamp of when the detector was last updated.
--
-- 'arn', 'detector_arn' - The detector ARN.
--
-- 'createdTime', 'detector_createdTime' - Timestamp of when the detector was created.
--
-- 'eventTypeName', 'detector_eventTypeName' - The name of the event type.
--
-- 'detectorId', 'detector_detectorId' - The detector ID.
--
-- 'description', 'detector_description' - The detector description.
newDetector ::
  Detector
newDetector =
  Detector'
    { lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      eventTypeName = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Timestamp of when the detector was last updated.
detector_lastUpdatedTime :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_lastUpdatedTime = Lens.lens (\Detector' {lastUpdatedTime} -> lastUpdatedTime) (\s@Detector' {} a -> s {lastUpdatedTime = a} :: Detector)

-- | The detector ARN.
detector_arn :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_arn = Lens.lens (\Detector' {arn} -> arn) (\s@Detector' {} a -> s {arn = a} :: Detector)

-- | Timestamp of when the detector was created.
detector_createdTime :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_createdTime = Lens.lens (\Detector' {createdTime} -> createdTime) (\s@Detector' {} a -> s {createdTime = a} :: Detector)

-- | The name of the event type.
detector_eventTypeName :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_eventTypeName = Lens.lens (\Detector' {eventTypeName} -> eventTypeName) (\s@Detector' {} a -> s {eventTypeName = a} :: Detector)

-- | The detector ID.
detector_detectorId :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_detectorId = Lens.lens (\Detector' {detectorId} -> detectorId) (\s@Detector' {} a -> s {detectorId = a} :: Detector)

-- | The detector description.
detector_description :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_description = Lens.lens (\Detector' {description} -> description) (\s@Detector' {} a -> s {description = a} :: Detector)

instance Core.FromJSON Detector where
  parseJSON =
    Core.withObject
      "Detector"
      ( \x ->
          Detector'
            Prelude.<$> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdTime")
            Prelude.<*> (x Core..:? "eventTypeName")
            Prelude.<*> (x Core..:? "detectorId")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable Detector

instance Prelude.NFData Detector
