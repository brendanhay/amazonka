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
-- Module      : Amazonka.Textract.Types.Prediction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.Prediction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information regarding predicted values returned by Amazon
-- Textract operations, including the predicted value and the confidence in
-- the predicted value.
--
-- /See:/ 'newPrediction' smart constructor.
data Prediction = Prediction'
  { -- | Amazon Textract\'s confidence in its predicted value.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The predicted value of a detected object.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Prediction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'prediction_confidence' - Amazon Textract\'s confidence in its predicted value.
--
-- 'value', 'prediction_value' - The predicted value of a detected object.
newPrediction ::
  Prediction
newPrediction =
  Prediction'
    { confidence = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Amazon Textract\'s confidence in its predicted value.
prediction_confidence :: Lens.Lens' Prediction (Prelude.Maybe Prelude.Double)
prediction_confidence = Lens.lens (\Prediction' {confidence} -> confidence) (\s@Prediction' {} a -> s {confidence = a} :: Prediction)

-- | The predicted value of a detected object.
prediction_value :: Lens.Lens' Prediction (Prelude.Maybe Prelude.Text)
prediction_value = Lens.lens (\Prediction' {value} -> value) (\s@Prediction' {} a -> s {value = a} :: Prediction)

instance Data.FromJSON Prediction where
  parseJSON =
    Data.withObject
      "Prediction"
      ( \x ->
          Prediction'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable Prediction where
  hashWithSalt _salt Prediction' {..} =
    _salt `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` value

instance Prelude.NFData Prediction where
  rnf Prediction' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf value
