{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Rekognition.Types.Eyeglasses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Eyeglasses where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether or not the face is wearing eye glasses, and the
-- confidence level in the determination.
--
-- /See:/ 'newEyeglasses' smart constructor.
data Eyeglasses = Eyeglasses'
  { -- | Level of confidence in the determination.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Boolean value that indicates whether the face is wearing eye glasses or
    -- not.
    value :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Eyeglasses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'eyeglasses_confidence' - Level of confidence in the determination.
--
-- 'value', 'eyeglasses_value' - Boolean value that indicates whether the face is wearing eye glasses or
-- not.
newEyeglasses ::
  Eyeglasses
newEyeglasses =
  Eyeglasses'
    { confidence = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Level of confidence in the determination.
eyeglasses_confidence :: Lens.Lens' Eyeglasses (Prelude.Maybe Prelude.Double)
eyeglasses_confidence = Lens.lens (\Eyeglasses' {confidence} -> confidence) (\s@Eyeglasses' {} a -> s {confidence = a} :: Eyeglasses)

-- | Boolean value that indicates whether the face is wearing eye glasses or
-- not.
eyeglasses_value :: Lens.Lens' Eyeglasses (Prelude.Maybe Prelude.Bool)
eyeglasses_value = Lens.lens (\Eyeglasses' {value} -> value) (\s@Eyeglasses' {} a -> s {value = a} :: Eyeglasses)

instance Prelude.FromJSON Eyeglasses where
  parseJSON =
    Prelude.withObject
      "Eyeglasses"
      ( \x ->
          Eyeglasses'
            Prelude.<$> (x Prelude..:? "Confidence")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable Eyeglasses

instance Prelude.NFData Eyeglasses
