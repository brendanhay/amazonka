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
-- Module      : Amazonka.Rekognition.Types.Eyeglasses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Eyeglasses where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether or not the face is wearing eye glasses, and the
-- confidence level in the determination.
--
-- /See:/ 'newEyeglasses' smart constructor.
data Eyeglasses = Eyeglasses'
  { -- | Boolean value that indicates whether the face is wearing eye glasses or
    -- not.
    value :: Prelude.Maybe Prelude.Bool,
    -- | Level of confidence in the determination.
    confidence :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Eyeglasses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'eyeglasses_value' - Boolean value that indicates whether the face is wearing eye glasses or
-- not.
--
-- 'confidence', 'eyeglasses_confidence' - Level of confidence in the determination.
newEyeglasses ::
  Eyeglasses
newEyeglasses =
  Eyeglasses'
    { value = Prelude.Nothing,
      confidence = Prelude.Nothing
    }

-- | Boolean value that indicates whether the face is wearing eye glasses or
-- not.
eyeglasses_value :: Lens.Lens' Eyeglasses (Prelude.Maybe Prelude.Bool)
eyeglasses_value = Lens.lens (\Eyeglasses' {value} -> value) (\s@Eyeglasses' {} a -> s {value = a} :: Eyeglasses)

-- | Level of confidence in the determination.
eyeglasses_confidence :: Lens.Lens' Eyeglasses (Prelude.Maybe Prelude.Double)
eyeglasses_confidence = Lens.lens (\Eyeglasses' {confidence} -> confidence) (\s@Eyeglasses' {} a -> s {confidence = a} :: Eyeglasses)

instance Core.FromJSON Eyeglasses where
  parseJSON =
    Core.withObject
      "Eyeglasses"
      ( \x ->
          Eyeglasses'
            Prelude.<$> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Confidence")
      )

instance Prelude.Hashable Eyeglasses

instance Prelude.NFData Eyeglasses
