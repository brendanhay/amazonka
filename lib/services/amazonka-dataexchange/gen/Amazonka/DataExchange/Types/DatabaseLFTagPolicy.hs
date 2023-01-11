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
-- Module      : Amazonka.DataExchange.Types.DatabaseLFTagPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.DatabaseLFTagPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.LFTag
import qualified Amazonka.Prelude as Prelude

-- | The LF-tag policy for database resources.
--
-- /See:/ 'newDatabaseLFTagPolicy' smart constructor.
data DatabaseLFTagPolicy = DatabaseLFTagPolicy'
  { -- | A list of LF-tag conditions that apply to database resources.
    expression :: [LFTag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseLFTagPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'databaseLFTagPolicy_expression' - A list of LF-tag conditions that apply to database resources.
newDatabaseLFTagPolicy ::
  DatabaseLFTagPolicy
newDatabaseLFTagPolicy =
  DatabaseLFTagPolicy' {expression = Prelude.mempty}

-- | A list of LF-tag conditions that apply to database resources.
databaseLFTagPolicy_expression :: Lens.Lens' DatabaseLFTagPolicy [LFTag]
databaseLFTagPolicy_expression = Lens.lens (\DatabaseLFTagPolicy' {expression} -> expression) (\s@DatabaseLFTagPolicy' {} a -> s {expression = a} :: DatabaseLFTagPolicy) Prelude.. Lens.coerced

instance Data.FromJSON DatabaseLFTagPolicy where
  parseJSON =
    Data.withObject
      "DatabaseLFTagPolicy"
      ( \x ->
          DatabaseLFTagPolicy'
            Prelude.<$> (x Data..:? "Expression" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DatabaseLFTagPolicy where
  hashWithSalt _salt DatabaseLFTagPolicy' {..} =
    _salt `Prelude.hashWithSalt` expression

instance Prelude.NFData DatabaseLFTagPolicy where
  rnf DatabaseLFTagPolicy' {..} = Prelude.rnf expression
