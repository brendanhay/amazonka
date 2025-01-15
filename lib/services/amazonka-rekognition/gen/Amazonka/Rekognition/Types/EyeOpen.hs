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
-- Module      : Amazonka.Rekognition.Types.EyeOpen
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.EyeOpen where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether or not the eyes on the face are open, and the
-- confidence level in the determination.
--
-- /See:/ 'newEyeOpen' smart constructor.
data EyeOpen = EyeOpen'
  { -- | Level of confidence in the determination.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Boolean value that indicates whether the eyes on the face are open.
    value :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EyeOpen' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'eyeOpen_confidence' - Level of confidence in the determination.
--
-- 'value', 'eyeOpen_value' - Boolean value that indicates whether the eyes on the face are open.
newEyeOpen ::
  EyeOpen
newEyeOpen =
  EyeOpen'
    { confidence = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Level of confidence in the determination.
eyeOpen_confidence :: Lens.Lens' EyeOpen (Prelude.Maybe Prelude.Double)
eyeOpen_confidence = Lens.lens (\EyeOpen' {confidence} -> confidence) (\s@EyeOpen' {} a -> s {confidence = a} :: EyeOpen)

-- | Boolean value that indicates whether the eyes on the face are open.
eyeOpen_value :: Lens.Lens' EyeOpen (Prelude.Maybe Prelude.Bool)
eyeOpen_value = Lens.lens (\EyeOpen' {value} -> value) (\s@EyeOpen' {} a -> s {value = a} :: EyeOpen)

instance Data.FromJSON EyeOpen where
  parseJSON =
    Data.withObject
      "EyeOpen"
      ( \x ->
          EyeOpen'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable EyeOpen where
  hashWithSalt _salt EyeOpen' {..} =
    _salt
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` value

instance Prelude.NFData EyeOpen where
  rnf EyeOpen' {..} =
    Prelude.rnf confidence `Prelude.seq`
      Prelude.rnf value
