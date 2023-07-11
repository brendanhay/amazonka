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
-- Module      : Amazonka.Textract.Types.AnalyzeIDDetections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.AnalyzeIDDetections where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.NormalizedValue

-- | Used to contain the information detected by an AnalyzeID operation.
--
-- /See:/ 'newAnalyzeIDDetections' smart constructor.
data AnalyzeIDDetections = AnalyzeIDDetections'
  { -- | The confidence score of the detected text.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Only returned for dates, returns the type of value detected and the date
    -- written in a more machine readable way.
    normalizedValue :: Prelude.Maybe NormalizedValue,
    -- | Text of either the normalized field or value associated with it.
    text :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyzeIDDetections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'analyzeIDDetections_confidence' - The confidence score of the detected text.
--
-- 'normalizedValue', 'analyzeIDDetections_normalizedValue' - Only returned for dates, returns the type of value detected and the date
-- written in a more machine readable way.
--
-- 'text', 'analyzeIDDetections_text' - Text of either the normalized field or value associated with it.
newAnalyzeIDDetections ::
  -- | 'text'
  Prelude.Text ->
  AnalyzeIDDetections
newAnalyzeIDDetections pText_ =
  AnalyzeIDDetections'
    { confidence = Prelude.Nothing,
      normalizedValue = Prelude.Nothing,
      text = pText_
    }

-- | The confidence score of the detected text.
analyzeIDDetections_confidence :: Lens.Lens' AnalyzeIDDetections (Prelude.Maybe Prelude.Double)
analyzeIDDetections_confidence = Lens.lens (\AnalyzeIDDetections' {confidence} -> confidence) (\s@AnalyzeIDDetections' {} a -> s {confidence = a} :: AnalyzeIDDetections)

-- | Only returned for dates, returns the type of value detected and the date
-- written in a more machine readable way.
analyzeIDDetections_normalizedValue :: Lens.Lens' AnalyzeIDDetections (Prelude.Maybe NormalizedValue)
analyzeIDDetections_normalizedValue = Lens.lens (\AnalyzeIDDetections' {normalizedValue} -> normalizedValue) (\s@AnalyzeIDDetections' {} a -> s {normalizedValue = a} :: AnalyzeIDDetections)

-- | Text of either the normalized field or value associated with it.
analyzeIDDetections_text :: Lens.Lens' AnalyzeIDDetections Prelude.Text
analyzeIDDetections_text = Lens.lens (\AnalyzeIDDetections' {text} -> text) (\s@AnalyzeIDDetections' {} a -> s {text = a} :: AnalyzeIDDetections)

instance Data.FromJSON AnalyzeIDDetections where
  parseJSON =
    Data.withObject
      "AnalyzeIDDetections"
      ( \x ->
          AnalyzeIDDetections'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "NormalizedValue")
            Prelude.<*> (x Data..: "Text")
      )

instance Prelude.Hashable AnalyzeIDDetections where
  hashWithSalt _salt AnalyzeIDDetections' {..} =
    _salt
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` normalizedValue
      `Prelude.hashWithSalt` text

instance Prelude.NFData AnalyzeIDDetections where
  rnf AnalyzeIDDetections' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf normalizedValue
      `Prelude.seq` Prelude.rnf text
