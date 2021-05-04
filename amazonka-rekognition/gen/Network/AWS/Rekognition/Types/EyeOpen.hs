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
-- Module      : Network.AWS.Rekognition.Types.EyeOpen
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EyeOpen where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON EyeOpen where
  parseJSON =
    Prelude.withObject
      "EyeOpen"
      ( \x ->
          EyeOpen'
            Prelude.<$> (x Prelude..:? "Confidence")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable EyeOpen

instance Prelude.NFData EyeOpen
