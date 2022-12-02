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
-- Module      : Amazonka.Textract.Types.QueriesConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.QueriesConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.Query

-- |
--
-- /See:/ 'newQueriesConfig' smart constructor.
data QueriesConfig = QueriesConfig'
  { queries :: Prelude.NonEmpty Query
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueriesConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queries', 'queriesConfig_queries' -
newQueriesConfig ::
  -- | 'queries'
  Prelude.NonEmpty Query ->
  QueriesConfig
newQueriesConfig pQueries_ =
  QueriesConfig'
    { queries =
        Lens.coerced Lens.# pQueries_
    }

-- |
queriesConfig_queries :: Lens.Lens' QueriesConfig (Prelude.NonEmpty Query)
queriesConfig_queries = Lens.lens (\QueriesConfig' {queries} -> queries) (\s@QueriesConfig' {} a -> s {queries = a} :: QueriesConfig) Prelude.. Lens.coerced

instance Prelude.Hashable QueriesConfig where
  hashWithSalt _salt QueriesConfig' {..} =
    _salt `Prelude.hashWithSalt` queries

instance Prelude.NFData QueriesConfig where
  rnf QueriesConfig' {..} = Prelude.rnf queries

instance Data.ToJSON QueriesConfig where
  toJSON QueriesConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Queries" Data..= queries)]
      )
