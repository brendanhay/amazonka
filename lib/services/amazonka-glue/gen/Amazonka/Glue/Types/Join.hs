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
-- Module      : Amazonka.Glue.Types.Join
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Join where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.JoinColumn
import Amazonka.Glue.Types.JoinType
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that joins two datasets into one dataset using a
-- comparison phrase on the specified data property keys. You can use
-- inner, outer, left, right, left semi, and left anti joins.
--
-- /See:/ 'newJoin' smart constructor.
data Join = Join'
  { -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | Specifies the type of join to be performed on the datasets.
    joinType :: JoinType,
    -- | A list of the two columns to be joined.
    columns :: Prelude.NonEmpty JoinColumn
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Join' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'join_name' - The name of the transform node.
--
-- 'inputs', 'join_inputs' - The data inputs identified by their node names.
--
-- 'joinType', 'join_joinType' - Specifies the type of join to be performed on the datasets.
--
-- 'columns', 'join_columns' - A list of the two columns to be joined.
newJoin ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'joinType'
  JoinType ->
  -- | 'columns'
  Prelude.NonEmpty JoinColumn ->
  Join
newJoin pName_ pInputs_ pJoinType_ pColumns_ =
  Join'
    { name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      joinType = pJoinType_,
      columns = Lens.coerced Lens.# pColumns_
    }

-- | The name of the transform node.
join_name :: Lens.Lens' Join Prelude.Text
join_name = Lens.lens (\Join' {name} -> name) (\s@Join' {} a -> s {name = a} :: Join)

-- | The data inputs identified by their node names.
join_inputs :: Lens.Lens' Join (Prelude.NonEmpty Prelude.Text)
join_inputs = Lens.lens (\Join' {inputs} -> inputs) (\s@Join' {} a -> s {inputs = a} :: Join) Prelude.. Lens.coerced

-- | Specifies the type of join to be performed on the datasets.
join_joinType :: Lens.Lens' Join JoinType
join_joinType = Lens.lens (\Join' {joinType} -> joinType) (\s@Join' {} a -> s {joinType = a} :: Join)

-- | A list of the two columns to be joined.
join_columns :: Lens.Lens' Join (Prelude.NonEmpty JoinColumn)
join_columns = Lens.lens (\Join' {columns} -> columns) (\s@Join' {} a -> s {columns = a} :: Join) Prelude.. Lens.coerced

instance Data.FromJSON Join where
  parseJSON =
    Data.withObject
      "Join"
      ( \x ->
          Join'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "JoinType")
            Prelude.<*> (x Data..: "Columns")
      )

instance Prelude.Hashable Join where
  hashWithSalt _salt Join' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` joinType
      `Prelude.hashWithSalt` columns

instance Prelude.NFData Join where
  rnf Join' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf joinType
      `Prelude.seq` Prelude.rnf columns

instance Data.ToJSON Join where
  toJSON Join' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("JoinType" Data..= joinType),
            Prelude.Just ("Columns" Data..= columns)
          ]
      )
