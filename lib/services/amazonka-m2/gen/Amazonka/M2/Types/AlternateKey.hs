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
-- Module      : Amazonka.M2.Types.AlternateKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.AlternateKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines an alternate key. This value is optional. A legacy data set
-- might not have any alternate key defined but if those alternate keys
-- definitions exist, provide them, as some applications will make use of
-- them.
--
-- /See:/ 'newAlternateKey' smart constructor.
data AlternateKey = AlternateKey'
  { -- | Indicates whether the alternate key values are supposed to be unique for
    -- the given data set.
    allowDuplicates :: Prelude.Maybe Prelude.Bool,
    -- | The name of the alternate key.
    name :: Prelude.Maybe Prelude.Text,
    -- | A strictly positive integer value representing the length of the
    -- alternate key.
    length :: Prelude.Int,
    -- | A positive integer value representing the offset to mark the start of
    -- the alternate key part in the record byte array.
    offset :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlternateKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowDuplicates', 'alternateKey_allowDuplicates' - Indicates whether the alternate key values are supposed to be unique for
-- the given data set.
--
-- 'name', 'alternateKey_name' - The name of the alternate key.
--
-- 'length', 'alternateKey_length' - A strictly positive integer value representing the length of the
-- alternate key.
--
-- 'offset', 'alternateKey_offset' - A positive integer value representing the offset to mark the start of
-- the alternate key part in the record byte array.
newAlternateKey ::
  -- | 'length'
  Prelude.Int ->
  -- | 'offset'
  Prelude.Int ->
  AlternateKey
newAlternateKey pLength_ pOffset_ =
  AlternateKey'
    { allowDuplicates = Prelude.Nothing,
      name = Prelude.Nothing,
      length = pLength_,
      offset = pOffset_
    }

-- | Indicates whether the alternate key values are supposed to be unique for
-- the given data set.
alternateKey_allowDuplicates :: Lens.Lens' AlternateKey (Prelude.Maybe Prelude.Bool)
alternateKey_allowDuplicates = Lens.lens (\AlternateKey' {allowDuplicates} -> allowDuplicates) (\s@AlternateKey' {} a -> s {allowDuplicates = a} :: AlternateKey)

-- | The name of the alternate key.
alternateKey_name :: Lens.Lens' AlternateKey (Prelude.Maybe Prelude.Text)
alternateKey_name = Lens.lens (\AlternateKey' {name} -> name) (\s@AlternateKey' {} a -> s {name = a} :: AlternateKey)

-- | A strictly positive integer value representing the length of the
-- alternate key.
alternateKey_length :: Lens.Lens' AlternateKey Prelude.Int
alternateKey_length = Lens.lens (\AlternateKey' {length} -> length) (\s@AlternateKey' {} a -> s {length = a} :: AlternateKey)

-- | A positive integer value representing the offset to mark the start of
-- the alternate key part in the record byte array.
alternateKey_offset :: Lens.Lens' AlternateKey Prelude.Int
alternateKey_offset = Lens.lens (\AlternateKey' {offset} -> offset) (\s@AlternateKey' {} a -> s {offset = a} :: AlternateKey)

instance Data.FromJSON AlternateKey where
  parseJSON =
    Data.withObject
      "AlternateKey"
      ( \x ->
          AlternateKey'
            Prelude.<$> (x Data..:? "allowDuplicates")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..: "length")
            Prelude.<*> (x Data..: "offset")
      )

instance Prelude.Hashable AlternateKey where
  hashWithSalt _salt AlternateKey' {..} =
    _salt `Prelude.hashWithSalt` allowDuplicates
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` length
      `Prelude.hashWithSalt` offset

instance Prelude.NFData AlternateKey where
  rnf AlternateKey' {..} =
    Prelude.rnf allowDuplicates
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf length
      `Prelude.seq` Prelude.rnf offset

instance Data.ToJSON AlternateKey where
  toJSON AlternateKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowDuplicates" Data..=)
              Prelude.<$> allowDuplicates,
            ("name" Data..=) Prelude.<$> name,
            Prelude.Just ("length" Data..= length),
            Prelude.Just ("offset" Data..= offset)
          ]
      )
