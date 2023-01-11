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
-- Module      : Amazonka.Glue.Types.NullCheckBoxList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.NullCheckBoxList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents whether certain values are recognized as null values for
-- removal.
--
-- /See:/ 'newNullCheckBoxList' smart constructor.
data NullCheckBoxList = NullCheckBoxList'
  { -- | Specifies that an empty string is considered as a null value.
    isEmpty :: Prelude.Maybe Prelude.Bool,
    -- | Specifies that an integer value of -1 is considered as a null value.
    isNegOne :: Prelude.Maybe Prelude.Bool,
    -- | Specifies that a value spelling out the word \'null\' is considered as a
    -- null value.
    isNullString :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NullCheckBoxList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isEmpty', 'nullCheckBoxList_isEmpty' - Specifies that an empty string is considered as a null value.
--
-- 'isNegOne', 'nullCheckBoxList_isNegOne' - Specifies that an integer value of -1 is considered as a null value.
--
-- 'isNullString', 'nullCheckBoxList_isNullString' - Specifies that a value spelling out the word \'null\' is considered as a
-- null value.
newNullCheckBoxList ::
  NullCheckBoxList
newNullCheckBoxList =
  NullCheckBoxList'
    { isEmpty = Prelude.Nothing,
      isNegOne = Prelude.Nothing,
      isNullString = Prelude.Nothing
    }

-- | Specifies that an empty string is considered as a null value.
nullCheckBoxList_isEmpty :: Lens.Lens' NullCheckBoxList (Prelude.Maybe Prelude.Bool)
nullCheckBoxList_isEmpty = Lens.lens (\NullCheckBoxList' {isEmpty} -> isEmpty) (\s@NullCheckBoxList' {} a -> s {isEmpty = a} :: NullCheckBoxList)

-- | Specifies that an integer value of -1 is considered as a null value.
nullCheckBoxList_isNegOne :: Lens.Lens' NullCheckBoxList (Prelude.Maybe Prelude.Bool)
nullCheckBoxList_isNegOne = Lens.lens (\NullCheckBoxList' {isNegOne} -> isNegOne) (\s@NullCheckBoxList' {} a -> s {isNegOne = a} :: NullCheckBoxList)

-- | Specifies that a value spelling out the word \'null\' is considered as a
-- null value.
nullCheckBoxList_isNullString :: Lens.Lens' NullCheckBoxList (Prelude.Maybe Prelude.Bool)
nullCheckBoxList_isNullString = Lens.lens (\NullCheckBoxList' {isNullString} -> isNullString) (\s@NullCheckBoxList' {} a -> s {isNullString = a} :: NullCheckBoxList)

instance Data.FromJSON NullCheckBoxList where
  parseJSON =
    Data.withObject
      "NullCheckBoxList"
      ( \x ->
          NullCheckBoxList'
            Prelude.<$> (x Data..:? "IsEmpty")
            Prelude.<*> (x Data..:? "IsNegOne")
            Prelude.<*> (x Data..:? "IsNullString")
      )

instance Prelude.Hashable NullCheckBoxList where
  hashWithSalt _salt NullCheckBoxList' {..} =
    _salt `Prelude.hashWithSalt` isEmpty
      `Prelude.hashWithSalt` isNegOne
      `Prelude.hashWithSalt` isNullString

instance Prelude.NFData NullCheckBoxList where
  rnf NullCheckBoxList' {..} =
    Prelude.rnf isEmpty
      `Prelude.seq` Prelude.rnf isNegOne
      `Prelude.seq` Prelude.rnf isNullString

instance Data.ToJSON NullCheckBoxList where
  toJSON NullCheckBoxList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsEmpty" Data..=) Prelude.<$> isEmpty,
            ("IsNegOne" Data..=) Prelude.<$> isNegOne,
            ("IsNullString" Data..=) Prelude.<$> isNullString
          ]
      )
