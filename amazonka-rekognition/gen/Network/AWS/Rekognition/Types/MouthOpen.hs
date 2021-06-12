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
-- Module      : Network.AWS.Rekognition.Types.MouthOpen
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.MouthOpen where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Indicates whether or not the mouth on the face is open, and the
-- confidence level in the determination.
--
-- /See:/ 'newMouthOpen' smart constructor.
data MouthOpen = MouthOpen'
  { -- | Level of confidence in the determination.
    confidence :: Core.Maybe Core.Double,
    -- | Boolean value that indicates whether the mouth on the face is open or
    -- not.
    value :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MouthOpen' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'mouthOpen_confidence' - Level of confidence in the determination.
--
-- 'value', 'mouthOpen_value' - Boolean value that indicates whether the mouth on the face is open or
-- not.
newMouthOpen ::
  MouthOpen
newMouthOpen =
  MouthOpen'
    { confidence = Core.Nothing,
      value = Core.Nothing
    }

-- | Level of confidence in the determination.
mouthOpen_confidence :: Lens.Lens' MouthOpen (Core.Maybe Core.Double)
mouthOpen_confidence = Lens.lens (\MouthOpen' {confidence} -> confidence) (\s@MouthOpen' {} a -> s {confidence = a} :: MouthOpen)

-- | Boolean value that indicates whether the mouth on the face is open or
-- not.
mouthOpen_value :: Lens.Lens' MouthOpen (Core.Maybe Core.Bool)
mouthOpen_value = Lens.lens (\MouthOpen' {value} -> value) (\s@MouthOpen' {} a -> s {value = a} :: MouthOpen)

instance Core.FromJSON MouthOpen where
  parseJSON =
    Core.withObject
      "MouthOpen"
      ( \x ->
          MouthOpen'
            Core.<$> (x Core..:? "Confidence")
            Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable MouthOpen

instance Core.NFData MouthOpen
