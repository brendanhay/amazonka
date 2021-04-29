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
-- Module      : Network.AWS.Rekognition.Types.Beard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Beard where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether or not the face has a beard, and the confidence level
-- in the determination.
--
-- /See:/ 'newBeard' smart constructor.
data Beard = Beard'
  { -- | Level of confidence in the determination.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Boolean value that indicates whether the face has beard or not.
    value :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Beard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'beard_confidence' - Level of confidence in the determination.
--
-- 'value', 'beard_value' - Boolean value that indicates whether the face has beard or not.
newBeard ::
  Beard
newBeard =
  Beard'
    { confidence = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Level of confidence in the determination.
beard_confidence :: Lens.Lens' Beard (Prelude.Maybe Prelude.Double)
beard_confidence = Lens.lens (\Beard' {confidence} -> confidence) (\s@Beard' {} a -> s {confidence = a} :: Beard)

-- | Boolean value that indicates whether the face has beard or not.
beard_value :: Lens.Lens' Beard (Prelude.Maybe Prelude.Bool)
beard_value = Lens.lens (\Beard' {value} -> value) (\s@Beard' {} a -> s {value = a} :: Beard)

instance Prelude.FromJSON Beard where
  parseJSON =
    Prelude.withObject
      "Beard"
      ( \x ->
          Beard'
            Prelude.<$> (x Prelude..:? "Confidence")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable Beard

instance Prelude.NFData Beard
