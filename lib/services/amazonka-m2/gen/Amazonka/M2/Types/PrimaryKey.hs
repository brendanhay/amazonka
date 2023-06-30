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
-- Module      : Amazonka.M2.Types.PrimaryKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.PrimaryKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The primary key for a KSDS data set.
--
-- /See:/ 'newPrimaryKey' smart constructor.
data PrimaryKey = PrimaryKey'
  { -- | A name for the Primary Key.
    name :: Prelude.Maybe Prelude.Text,
    -- | A strictly positive integer value representing the length of the primary
    -- key.
    length :: Prelude.Int,
    -- | A positive integer value representing the offset to mark the start of
    -- the primary key in the record byte array.
    offset :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrimaryKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'primaryKey_name' - A name for the Primary Key.
--
-- 'length', 'primaryKey_length' - A strictly positive integer value representing the length of the primary
-- key.
--
-- 'offset', 'primaryKey_offset' - A positive integer value representing the offset to mark the start of
-- the primary key in the record byte array.
newPrimaryKey ::
  -- | 'length'
  Prelude.Int ->
  -- | 'offset'
  Prelude.Int ->
  PrimaryKey
newPrimaryKey pLength_ pOffset_ =
  PrimaryKey'
    { name = Prelude.Nothing,
      length = pLength_,
      offset = pOffset_
    }

-- | A name for the Primary Key.
primaryKey_name :: Lens.Lens' PrimaryKey (Prelude.Maybe Prelude.Text)
primaryKey_name = Lens.lens (\PrimaryKey' {name} -> name) (\s@PrimaryKey' {} a -> s {name = a} :: PrimaryKey)

-- | A strictly positive integer value representing the length of the primary
-- key.
primaryKey_length :: Lens.Lens' PrimaryKey Prelude.Int
primaryKey_length = Lens.lens (\PrimaryKey' {length} -> length) (\s@PrimaryKey' {} a -> s {length = a} :: PrimaryKey)

-- | A positive integer value representing the offset to mark the start of
-- the primary key in the record byte array.
primaryKey_offset :: Lens.Lens' PrimaryKey Prelude.Int
primaryKey_offset = Lens.lens (\PrimaryKey' {offset} -> offset) (\s@PrimaryKey' {} a -> s {offset = a} :: PrimaryKey)

instance Data.FromJSON PrimaryKey where
  parseJSON =
    Data.withObject
      "PrimaryKey"
      ( \x ->
          PrimaryKey'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..: "length")
            Prelude.<*> (x Data..: "offset")
      )

instance Prelude.Hashable PrimaryKey where
  hashWithSalt _salt PrimaryKey' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` length
      `Prelude.hashWithSalt` offset

instance Prelude.NFData PrimaryKey where
  rnf PrimaryKey' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf length
      `Prelude.seq` Prelude.rnf offset

instance Data.ToJSON PrimaryKey where
  toJSON PrimaryKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            Prelude.Just ("length" Data..= length),
            Prelude.Just ("offset" Data..= offset)
          ]
      )
