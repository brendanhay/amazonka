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
-- Module      : Amazonka.Rekognition.Types.Smile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Smile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether or not the face is smiling, and the confidence level
-- in the determination.
--
-- /See:/ 'newSmile' smart constructor.
data Smile = Smile'
  { -- | Level of confidence in the determination.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Boolean value that indicates whether the face is smiling or not.
    value :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Smile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'smile_confidence' - Level of confidence in the determination.
--
-- 'value', 'smile_value' - Boolean value that indicates whether the face is smiling or not.
newSmile ::
  Smile
newSmile =
  Smile'
    { confidence = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Level of confidence in the determination.
smile_confidence :: Lens.Lens' Smile (Prelude.Maybe Prelude.Double)
smile_confidence = Lens.lens (\Smile' {confidence} -> confidence) (\s@Smile' {} a -> s {confidence = a} :: Smile)

-- | Boolean value that indicates whether the face is smiling or not.
smile_value :: Lens.Lens' Smile (Prelude.Maybe Prelude.Bool)
smile_value = Lens.lens (\Smile' {value} -> value) (\s@Smile' {} a -> s {value = a} :: Smile)

instance Data.FromJSON Smile where
  parseJSON =
    Data.withObject
      "Smile"
      ( \x ->
          Smile'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable Smile where
  hashWithSalt _salt Smile' {..} =
    _salt
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` value

instance Prelude.NFData Smile where
  rnf Smile' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf value
