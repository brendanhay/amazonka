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
-- Module      : Amazonka.DataExchange.Types.TableLFTagPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.TableLFTagPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.LFTag
import qualified Amazonka.Prelude as Prelude

-- | The LF-tag policy for a table resource.
--
-- /See:/ 'newTableLFTagPolicy' smart constructor.
data TableLFTagPolicy = TableLFTagPolicy'
  { -- | A list of LF-tag conditions that apply to table resources.
    expression :: [LFTag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableLFTagPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'tableLFTagPolicy_expression' - A list of LF-tag conditions that apply to table resources.
newTableLFTagPolicy ::
  TableLFTagPolicy
newTableLFTagPolicy =
  TableLFTagPolicy' {expression = Prelude.mempty}

-- | A list of LF-tag conditions that apply to table resources.
tableLFTagPolicy_expression :: Lens.Lens' TableLFTagPolicy [LFTag]
tableLFTagPolicy_expression = Lens.lens (\TableLFTagPolicy' {expression} -> expression) (\s@TableLFTagPolicy' {} a -> s {expression = a} :: TableLFTagPolicy) Prelude.. Lens.coerced

instance Data.FromJSON TableLFTagPolicy where
  parseJSON =
    Data.withObject
      "TableLFTagPolicy"
      ( \x ->
          TableLFTagPolicy'
            Prelude.<$> (x Data..:? "Expression" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TableLFTagPolicy where
  hashWithSalt _salt TableLFTagPolicy' {..} =
    _salt `Prelude.hashWithSalt` expression

instance Prelude.NFData TableLFTagPolicy where
  rnf TableLFTagPolicy' {..} = Prelude.rnf expression
