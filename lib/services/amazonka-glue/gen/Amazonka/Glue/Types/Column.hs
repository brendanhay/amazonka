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
-- Module      : Amazonka.Glue.Types.Column
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Column where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A column in a @Table@.
--
-- /See:/ 'newColumn' smart constructor.
data Column = Column'
  { -- | A free-form text comment.
    comment :: Prelude.Maybe Prelude.Text,
    -- | These key-value pairs define properties associated with the column.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The data type of the @Column@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The name of the @Column@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Column' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'column_comment' - A free-form text comment.
--
-- 'parameters', 'column_parameters' - These key-value pairs define properties associated with the column.
--
-- 'type'', 'column_type' - The data type of the @Column@.
--
-- 'name', 'column_name' - The name of the @Column@.
newColumn ::
  -- | 'name'
  Prelude.Text ->
  Column
newColumn pName_ =
  Column'
    { comment = Prelude.Nothing,
      parameters = Prelude.Nothing,
      type' = Prelude.Nothing,
      name = pName_
    }

-- | A free-form text comment.
column_comment :: Lens.Lens' Column (Prelude.Maybe Prelude.Text)
column_comment = Lens.lens (\Column' {comment} -> comment) (\s@Column' {} a -> s {comment = a} :: Column)

-- | These key-value pairs define properties associated with the column.
column_parameters :: Lens.Lens' Column (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
column_parameters = Lens.lens (\Column' {parameters} -> parameters) (\s@Column' {} a -> s {parameters = a} :: Column) Prelude.. Lens.mapping Lens.coerced

-- | The data type of the @Column@.
column_type :: Lens.Lens' Column (Prelude.Maybe Prelude.Text)
column_type = Lens.lens (\Column' {type'} -> type') (\s@Column' {} a -> s {type' = a} :: Column)

-- | The name of the @Column@.
column_name :: Lens.Lens' Column Prelude.Text
column_name = Lens.lens (\Column' {name} -> name) (\s@Column' {} a -> s {name = a} :: Column)

instance Data.FromJSON Column where
  parseJSON =
    Data.withObject
      "Column"
      ( \x ->
          Column'
            Prelude.<$> (x Data..:? "Comment")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Column where
  hashWithSalt _salt Column' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name

instance Prelude.NFData Column where
  rnf Column' {..} =
    Prelude.rnf comment `Prelude.seq`
      Prelude.rnf parameters `Prelude.seq`
        Prelude.rnf type' `Prelude.seq`
          Prelude.rnf name

instance Data.ToJSON Column where
  toJSON Column' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Comment" Data..=) Prelude.<$> comment,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just ("Name" Data..= name)
          ]
      )
