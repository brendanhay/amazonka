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
-- Module      : Amazonka.Rekognition.Types.MouthOpen
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.MouthOpen where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether or not the mouth on the face is open, and the
-- confidence level in the determination.
--
-- /See:/ 'newMouthOpen' smart constructor.
data MouthOpen = MouthOpen'
  { -- | Level of confidence in the determination.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Boolean value that indicates whether the mouth on the face is open or
    -- not.
    value :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { confidence = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Level of confidence in the determination.
mouthOpen_confidence :: Lens.Lens' MouthOpen (Prelude.Maybe Prelude.Double)
mouthOpen_confidence = Lens.lens (\MouthOpen' {confidence} -> confidence) (\s@MouthOpen' {} a -> s {confidence = a} :: MouthOpen)

-- | Boolean value that indicates whether the mouth on the face is open or
-- not.
mouthOpen_value :: Lens.Lens' MouthOpen (Prelude.Maybe Prelude.Bool)
mouthOpen_value = Lens.lens (\MouthOpen' {value} -> value) (\s@MouthOpen' {} a -> s {value = a} :: MouthOpen)

instance Data.FromJSON MouthOpen where
  parseJSON =
    Data.withObject
      "MouthOpen"
      ( \x ->
          MouthOpen'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable MouthOpen where
  hashWithSalt _salt MouthOpen' {..} =
    _salt `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` value

instance Prelude.NFData MouthOpen where
  rnf MouthOpen' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf value
