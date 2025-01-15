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
-- Module      : Amazonka.Glue.Types.Merge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Merge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that merges a @DynamicFrame@ with a staging
-- @DynamicFrame@ based on the specified primary keys to identify records.
-- Duplicate records (records with the same primary keys) are not
-- de-duplicated.
--
-- /See:/ 'newMerge' smart constructor.
data Merge = Merge'
  { -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The source @DynamicFrame@ that will be merged with a staging
    -- @DynamicFrame@.
    source :: Prelude.Text,
    -- | The list of primary key fields to match records from the source and
    -- staging dynamic frames.
    primaryKeys :: [[Prelude.Text]]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Merge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'merge_name' - The name of the transform node.
--
-- 'inputs', 'merge_inputs' - The data inputs identified by their node names.
--
-- 'source', 'merge_source' - The source @DynamicFrame@ that will be merged with a staging
-- @DynamicFrame@.
--
-- 'primaryKeys', 'merge_primaryKeys' - The list of primary key fields to match records from the source and
-- staging dynamic frames.
newMerge ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'source'
  Prelude.Text ->
  Merge
newMerge pName_ pInputs_ pSource_ =
  Merge'
    { name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      source = pSource_,
      primaryKeys = Prelude.mempty
    }

-- | The name of the transform node.
merge_name :: Lens.Lens' Merge Prelude.Text
merge_name = Lens.lens (\Merge' {name} -> name) (\s@Merge' {} a -> s {name = a} :: Merge)

-- | The data inputs identified by their node names.
merge_inputs :: Lens.Lens' Merge (Prelude.NonEmpty Prelude.Text)
merge_inputs = Lens.lens (\Merge' {inputs} -> inputs) (\s@Merge' {} a -> s {inputs = a} :: Merge) Prelude.. Lens.coerced

-- | The source @DynamicFrame@ that will be merged with a staging
-- @DynamicFrame@.
merge_source :: Lens.Lens' Merge Prelude.Text
merge_source = Lens.lens (\Merge' {source} -> source) (\s@Merge' {} a -> s {source = a} :: Merge)

-- | The list of primary key fields to match records from the source and
-- staging dynamic frames.
merge_primaryKeys :: Lens.Lens' Merge [[Prelude.Text]]
merge_primaryKeys = Lens.lens (\Merge' {primaryKeys} -> primaryKeys) (\s@Merge' {} a -> s {primaryKeys = a} :: Merge) Prelude.. Lens.coerced

instance Data.FromJSON Merge where
  parseJSON =
    Data.withObject
      "Merge"
      ( \x ->
          Merge'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Source")
            Prelude.<*> (x Data..:? "PrimaryKeys" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Merge where
  hashWithSalt _salt Merge' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` primaryKeys

instance Prelude.NFData Merge where
  rnf Merge' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf inputs `Prelude.seq`
        Prelude.rnf source `Prelude.seq`
          Prelude.rnf primaryKeys

instance Data.ToJSON Merge where
  toJSON Merge' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Source" Data..= source),
            Prelude.Just ("PrimaryKeys" Data..= primaryKeys)
          ]
      )
