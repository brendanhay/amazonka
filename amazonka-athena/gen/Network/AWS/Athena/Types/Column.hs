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
-- Module      : Network.AWS.Athena.Types.Column
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Column where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains metadata for a column in a table.
--
-- /See:/ 'newColumn' smart constructor.
data Column = Column'
  { -- | Optional information about the column.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The data type of the column.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The name of the column.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Column' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'column_comment' - Optional information about the column.
--
-- 'type'', 'column_type' - The data type of the column.
--
-- 'name', 'column_name' - The name of the column.
newColumn ::
  -- | 'name'
  Prelude.Text ->
  Column
newColumn pName_ =
  Column'
    { comment = Prelude.Nothing,
      type' = Prelude.Nothing,
      name = pName_
    }

-- | Optional information about the column.
column_comment :: Lens.Lens' Column (Prelude.Maybe Prelude.Text)
column_comment = Lens.lens (\Column' {comment} -> comment) (\s@Column' {} a -> s {comment = a} :: Column)

-- | The data type of the column.
column_type :: Lens.Lens' Column (Prelude.Maybe Prelude.Text)
column_type = Lens.lens (\Column' {type'} -> type') (\s@Column' {} a -> s {type' = a} :: Column)

-- | The name of the column.
column_name :: Lens.Lens' Column Prelude.Text
column_name = Lens.lens (\Column' {name} -> name) (\s@Column' {} a -> s {name = a} :: Column)

instance Prelude.FromJSON Column where
  parseJSON =
    Prelude.withObject
      "Column"
      ( \x ->
          Column'
            Prelude.<$> (x Prelude..:? "Comment")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable Column

instance Prelude.NFData Column
