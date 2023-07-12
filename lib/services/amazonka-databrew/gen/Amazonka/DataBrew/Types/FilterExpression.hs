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
-- Module      : Amazonka.DataBrew.Types.FilterExpression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.FilterExpression where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a structure for defining parameter conditions. Supported
-- conditions are described here:
-- <https://docs.aws.amazon.com/databrew/latest/dg/datasets.multiple-files.html#conditions.for.dynamic.datasets Supported conditions for dynamic datasets>
-- in the /Glue DataBrew Developer Guide/.
--
-- /See:/ 'newFilterExpression' smart constructor.
data FilterExpression = FilterExpression'
  { -- | The expression which includes condition names followed by substitution
    -- variables, possibly grouped and combined with other conditions. For
    -- example, \"(starts_with :prefix1 or starts_with :prefix2) and (ends_with
    -- :suffix1 or ends_with :suffix2)\". Substitution variables should start
    -- with \':\' symbol.
    expression :: Prelude.Text,
    -- | The map of substitution variable names to their values used in this
    -- filter expression.
    valuesMap :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterExpression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'filterExpression_expression' - The expression which includes condition names followed by substitution
-- variables, possibly grouped and combined with other conditions. For
-- example, \"(starts_with :prefix1 or starts_with :prefix2) and (ends_with
-- :suffix1 or ends_with :suffix2)\". Substitution variables should start
-- with \':\' symbol.
--
-- 'valuesMap', 'filterExpression_valuesMap' - The map of substitution variable names to their values used in this
-- filter expression.
newFilterExpression ::
  -- | 'expression'
  Prelude.Text ->
  FilterExpression
newFilterExpression pExpression_ =
  FilterExpression'
    { expression = pExpression_,
      valuesMap = Prelude.mempty
    }

-- | The expression which includes condition names followed by substitution
-- variables, possibly grouped and combined with other conditions. For
-- example, \"(starts_with :prefix1 or starts_with :prefix2) and (ends_with
-- :suffix1 or ends_with :suffix2)\". Substitution variables should start
-- with \':\' symbol.
filterExpression_expression :: Lens.Lens' FilterExpression Prelude.Text
filterExpression_expression = Lens.lens (\FilterExpression' {expression} -> expression) (\s@FilterExpression' {} a -> s {expression = a} :: FilterExpression)

-- | The map of substitution variable names to their values used in this
-- filter expression.
filterExpression_valuesMap :: Lens.Lens' FilterExpression (Prelude.HashMap Prelude.Text Prelude.Text)
filterExpression_valuesMap = Lens.lens (\FilterExpression' {valuesMap} -> valuesMap) (\s@FilterExpression' {} a -> s {valuesMap = a} :: FilterExpression) Prelude.. Lens.coerced

instance Data.FromJSON FilterExpression where
  parseJSON =
    Data.withObject
      "FilterExpression"
      ( \x ->
          FilterExpression'
            Prelude.<$> (x Data..: "Expression")
            Prelude.<*> (x Data..:? "ValuesMap" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FilterExpression where
  hashWithSalt _salt FilterExpression' {..} =
    _salt
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` valuesMap

instance Prelude.NFData FilterExpression where
  rnf FilterExpression' {..} =
    Prelude.rnf expression
      `Prelude.seq` Prelude.rnf valuesMap

instance Data.ToJSON FilterExpression where
  toJSON FilterExpression' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Expression" Data..= expression),
            Prelude.Just ("ValuesMap" Data..= valuesMap)
          ]
      )
