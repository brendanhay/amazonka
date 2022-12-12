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
-- Module      : Amazonka.LookoutMetrics.Types.DetectedField
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DetectedField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.AttributeValue
import Amazonka.LookoutMetrics.Types.Confidence
import qualified Amazonka.Prelude as Prelude

-- | An inferred field.
--
-- /See:/ 'newDetectedField' smart constructor.
data DetectedField = DetectedField'
  { -- | The field\'s confidence.
    confidence :: Prelude.Maybe Confidence,
    -- | The field\'s message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The field\'s value.
    value :: Prelude.Maybe AttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectedField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'detectedField_confidence' - The field\'s confidence.
--
-- 'message', 'detectedField_message' - The field\'s message.
--
-- 'value', 'detectedField_value' - The field\'s value.
newDetectedField ::
  DetectedField
newDetectedField =
  DetectedField'
    { confidence = Prelude.Nothing,
      message = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The field\'s confidence.
detectedField_confidence :: Lens.Lens' DetectedField (Prelude.Maybe Confidence)
detectedField_confidence = Lens.lens (\DetectedField' {confidence} -> confidence) (\s@DetectedField' {} a -> s {confidence = a} :: DetectedField)

-- | The field\'s message.
detectedField_message :: Lens.Lens' DetectedField (Prelude.Maybe Prelude.Text)
detectedField_message = Lens.lens (\DetectedField' {message} -> message) (\s@DetectedField' {} a -> s {message = a} :: DetectedField)

-- | The field\'s value.
detectedField_value :: Lens.Lens' DetectedField (Prelude.Maybe AttributeValue)
detectedField_value = Lens.lens (\DetectedField' {value} -> value) (\s@DetectedField' {} a -> s {value = a} :: DetectedField)

instance Data.FromJSON DetectedField where
  parseJSON =
    Data.withObject
      "DetectedField"
      ( \x ->
          DetectedField'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable DetectedField where
  hashWithSalt _salt DetectedField' {..} =
    _salt `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` value

instance Prelude.NFData DetectedField where
  rnf DetectedField' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf value
