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
-- Module      : Amazonka.Glue.Types.Union
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Union where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.UnionType
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that combines the rows from two or more datasets
-- into a single result.
--
-- /See:/ 'newUnion' smart constructor.
data Union = Union'
  { -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The node ID inputs to the transform.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | Indicates the type of Union transform.
    --
    -- Specify @ALL@ to join all rows from data sources to the resulting
    -- DynamicFrame. The resulting union does not remove duplicate rows.
    --
    -- Specify @DISTINCT@ to remove duplicate rows in the resulting
    -- DynamicFrame.
    unionType :: UnionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Union' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'union_name' - The name of the transform node.
--
-- 'inputs', 'union_inputs' - The node ID inputs to the transform.
--
-- 'unionType', 'union_unionType' - Indicates the type of Union transform.
--
-- Specify @ALL@ to join all rows from data sources to the resulting
-- DynamicFrame. The resulting union does not remove duplicate rows.
--
-- Specify @DISTINCT@ to remove duplicate rows in the resulting
-- DynamicFrame.
newUnion ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'unionType'
  UnionType ->
  Union
newUnion pName_ pInputs_ pUnionType_ =
  Union'
    { name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      unionType = pUnionType_
    }

-- | The name of the transform node.
union_name :: Lens.Lens' Union Prelude.Text
union_name = Lens.lens (\Union' {name} -> name) (\s@Union' {} a -> s {name = a} :: Union)

-- | The node ID inputs to the transform.
union_inputs :: Lens.Lens' Union (Prelude.NonEmpty Prelude.Text)
union_inputs = Lens.lens (\Union' {inputs} -> inputs) (\s@Union' {} a -> s {inputs = a} :: Union) Prelude.. Lens.coerced

-- | Indicates the type of Union transform.
--
-- Specify @ALL@ to join all rows from data sources to the resulting
-- DynamicFrame. The resulting union does not remove duplicate rows.
--
-- Specify @DISTINCT@ to remove duplicate rows in the resulting
-- DynamicFrame.
union_unionType :: Lens.Lens' Union UnionType
union_unionType = Lens.lens (\Union' {unionType} -> unionType) (\s@Union' {} a -> s {unionType = a} :: Union)

instance Core.FromJSON Union where
  parseJSON =
    Core.withObject
      "Union"
      ( \x ->
          Union'
            Prelude.<$> (x Core..: "Name")
            Prelude.<*> (x Core..: "Inputs")
            Prelude.<*> (x Core..: "UnionType")
      )

instance Prelude.Hashable Union where
  hashWithSalt _salt Union' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` unionType

instance Prelude.NFData Union where
  rnf Union' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf unionType

instance Core.ToJSON Union where
  toJSON Union' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Inputs" Core..= inputs),
            Prelude.Just ("UnionType" Core..= unionType)
          ]
      )
