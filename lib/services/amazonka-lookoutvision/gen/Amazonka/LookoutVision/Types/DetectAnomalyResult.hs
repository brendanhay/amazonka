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
-- Module      : Amazonka.LookoutVision.Types.DetectAnomalyResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.DetectAnomalyResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutVision.Types.ImageSource
import qualified Amazonka.Prelude as Prelude

-- | The prediction results from a call to DetectAnomalies.
--
-- /See:/ 'newDetectAnomalyResult' smart constructor.
data DetectAnomalyResult = DetectAnomalyResult'
  { -- | True if the image contains an anomaly, otherwise false.
    isAnomalous :: Prelude.Maybe Prelude.Bool,
    -- | The confidence that Amazon Lookout for Vision has in the accuracy of the
    -- prediction.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The source of the image that was analyzed. @direct@ means that the
    -- images was supplied from the local computer. No other values are
    -- supported.
    source :: Prelude.Maybe ImageSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectAnomalyResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isAnomalous', 'detectAnomalyResult_isAnomalous' - True if the image contains an anomaly, otherwise false.
--
-- 'confidence', 'detectAnomalyResult_confidence' - The confidence that Amazon Lookout for Vision has in the accuracy of the
-- prediction.
--
-- 'source', 'detectAnomalyResult_source' - The source of the image that was analyzed. @direct@ means that the
-- images was supplied from the local computer. No other values are
-- supported.
newDetectAnomalyResult ::
  DetectAnomalyResult
newDetectAnomalyResult =
  DetectAnomalyResult'
    { isAnomalous = Prelude.Nothing,
      confidence = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | True if the image contains an anomaly, otherwise false.
detectAnomalyResult_isAnomalous :: Lens.Lens' DetectAnomalyResult (Prelude.Maybe Prelude.Bool)
detectAnomalyResult_isAnomalous = Lens.lens (\DetectAnomalyResult' {isAnomalous} -> isAnomalous) (\s@DetectAnomalyResult' {} a -> s {isAnomalous = a} :: DetectAnomalyResult)

-- | The confidence that Amazon Lookout for Vision has in the accuracy of the
-- prediction.
detectAnomalyResult_confidence :: Lens.Lens' DetectAnomalyResult (Prelude.Maybe Prelude.Double)
detectAnomalyResult_confidence = Lens.lens (\DetectAnomalyResult' {confidence} -> confidence) (\s@DetectAnomalyResult' {} a -> s {confidence = a} :: DetectAnomalyResult)

-- | The source of the image that was analyzed. @direct@ means that the
-- images was supplied from the local computer. No other values are
-- supported.
detectAnomalyResult_source :: Lens.Lens' DetectAnomalyResult (Prelude.Maybe ImageSource)
detectAnomalyResult_source = Lens.lens (\DetectAnomalyResult' {source} -> source) (\s@DetectAnomalyResult' {} a -> s {source = a} :: DetectAnomalyResult)

instance Core.FromJSON DetectAnomalyResult where
  parseJSON =
    Core.withObject
      "DetectAnomalyResult"
      ( \x ->
          DetectAnomalyResult'
            Prelude.<$> (x Core..:? "IsAnomalous")
            Prelude.<*> (x Core..:? "Confidence")
            Prelude.<*> (x Core..:? "Source")
      )

instance Prelude.Hashable DetectAnomalyResult

instance Prelude.NFData DetectAnomalyResult
