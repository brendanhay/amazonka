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
-- Module      : Amazonka.DataPipeline.Types.Query
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.Query where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types.Selector
import qualified Amazonka.Prelude as Prelude

-- | Defines the query to run against an object.
--
-- /See:/ 'newQuery' smart constructor.
data Query = Query'
  { -- | List of selectors that define the query. An object must satisfy all of
    -- the selectors to match the query.
    selectors :: Prelude.Maybe [Selector]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Query' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectors', 'query_selectors' - List of selectors that define the query. An object must satisfy all of
-- the selectors to match the query.
newQuery ::
  Query
newQuery = Query' {selectors = Prelude.Nothing}

-- | List of selectors that define the query. An object must satisfy all of
-- the selectors to match the query.
query_selectors :: Lens.Lens' Query (Prelude.Maybe [Selector])
query_selectors = Lens.lens (\Query' {selectors} -> selectors) (\s@Query' {} a -> s {selectors = a} :: Query) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Query where
  hashWithSalt _salt Query' {..} =
    _salt `Prelude.hashWithSalt` selectors

instance Prelude.NFData Query where
  rnf Query' {..} = Prelude.rnf selectors

instance Data.ToJSON Query where
  toJSON Query' {..} =
    Data.object
      ( Prelude.catMaybes
          [("selectors" Data..=) Prelude.<$> selectors]
      )
