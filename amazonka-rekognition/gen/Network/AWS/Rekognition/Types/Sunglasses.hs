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
-- Module      : Network.AWS.Rekognition.Types.Sunglasses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Sunglasses where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether or not the face is wearing sunglasses, and the
-- confidence level in the determination.
--
-- /See:/ 'newSunglasses' smart constructor.
data Sunglasses = Sunglasses'
  { -- | Level of confidence in the determination.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Boolean value that indicates whether the face is wearing sunglasses or
    -- not.
    value :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Sunglasses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'sunglasses_confidence' - Level of confidence in the determination.
--
-- 'value', 'sunglasses_value' - Boolean value that indicates whether the face is wearing sunglasses or
-- not.
newSunglasses ::
  Sunglasses
newSunglasses =
  Sunglasses'
    { confidence = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Level of confidence in the determination.
sunglasses_confidence :: Lens.Lens' Sunglasses (Prelude.Maybe Prelude.Double)
sunglasses_confidence = Lens.lens (\Sunglasses' {confidence} -> confidence) (\s@Sunglasses' {} a -> s {confidence = a} :: Sunglasses)

-- | Boolean value that indicates whether the face is wearing sunglasses or
-- not.
sunglasses_value :: Lens.Lens' Sunglasses (Prelude.Maybe Prelude.Bool)
sunglasses_value = Lens.lens (\Sunglasses' {value} -> value) (\s@Sunglasses' {} a -> s {value = a} :: Sunglasses)

instance Prelude.FromJSON Sunglasses where
  parseJSON =
    Prelude.withObject
      "Sunglasses"
      ( \x ->
          Sunglasses'
            Prelude.<$> (x Prelude..:? "Confidence")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable Sunglasses

instance Prelude.NFData Sunglasses
