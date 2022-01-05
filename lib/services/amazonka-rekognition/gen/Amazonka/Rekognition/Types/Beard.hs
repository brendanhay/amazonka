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
-- Module      : Amazonka.Rekognition.Types.Beard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Beard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether or not the face has a beard, and the confidence level
-- in the determination.
--
-- /See:/ 'newBeard' smart constructor.
data Beard = Beard'
  { -- | Boolean value that indicates whether the face has beard or not.
    value :: Prelude.Maybe Prelude.Bool,
    -- | Level of confidence in the determination.
    confidence :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Beard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'beard_value' - Boolean value that indicates whether the face has beard or not.
--
-- 'confidence', 'beard_confidence' - Level of confidence in the determination.
newBeard ::
  Beard
newBeard =
  Beard'
    { value = Prelude.Nothing,
      confidence = Prelude.Nothing
    }

-- | Boolean value that indicates whether the face has beard or not.
beard_value :: Lens.Lens' Beard (Prelude.Maybe Prelude.Bool)
beard_value = Lens.lens (\Beard' {value} -> value) (\s@Beard' {} a -> s {value = a} :: Beard)

-- | Level of confidence in the determination.
beard_confidence :: Lens.Lens' Beard (Prelude.Maybe Prelude.Double)
beard_confidence = Lens.lens (\Beard' {confidence} -> confidence) (\s@Beard' {} a -> s {confidence = a} :: Beard)

instance Core.FromJSON Beard where
  parseJSON =
    Core.withObject
      "Beard"
      ( \x ->
          Beard'
            Prelude.<$> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Confidence")
      )

instance Prelude.Hashable Beard where
  hashWithSalt _salt Beard' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` confidence

instance Prelude.NFData Beard where
  rnf Beard' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf confidence
