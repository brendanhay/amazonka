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
-- Module      : Amazonka.Rekognition.Types.Mustache
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Mustache where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether or not the face has a mustache, and the confidence
-- level in the determination.
--
-- /See:/ 'newMustache' smart constructor.
data Mustache = Mustache'
  { -- | Level of confidence in the determination.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Boolean value that indicates whether the face has mustache or not.
    value :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Mustache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'mustache_confidence' - Level of confidence in the determination.
--
-- 'value', 'mustache_value' - Boolean value that indicates whether the face has mustache or not.
newMustache ::
  Mustache
newMustache =
  Mustache'
    { confidence = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Level of confidence in the determination.
mustache_confidence :: Lens.Lens' Mustache (Prelude.Maybe Prelude.Double)
mustache_confidence = Lens.lens (\Mustache' {confidence} -> confidence) (\s@Mustache' {} a -> s {confidence = a} :: Mustache)

-- | Boolean value that indicates whether the face has mustache or not.
mustache_value :: Lens.Lens' Mustache (Prelude.Maybe Prelude.Bool)
mustache_value = Lens.lens (\Mustache' {value} -> value) (\s@Mustache' {} a -> s {value = a} :: Mustache)

instance Data.FromJSON Mustache where
  parseJSON =
    Data.withObject
      "Mustache"
      ( \x ->
          Mustache'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable Mustache where
  hashWithSalt _salt Mustache' {..} =
    _salt
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` value

instance Prelude.NFData Mustache where
  rnf Mustache' {..} =
    Prelude.rnf confidence `Prelude.seq`
      Prelude.rnf value
