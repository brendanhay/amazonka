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
-- Module      : Network.AWS.DataPipeline.Types.Query
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.Query where

import Network.AWS.DataPipeline.Types.Selector
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines the query to run against an object.
--
-- /See:/ 'newQuery' smart constructor.
data Query = Query'
  { -- | List of selectors that define the query. An object must satisfy all of
    -- the selectors to match the query.
    selectors :: Prelude.Maybe [Selector]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
query_selectors = Lens.lens (\Query' {selectors} -> selectors) (\s@Query' {} a -> s {selectors = a} :: Query) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable Query

instance Prelude.NFData Query

instance Prelude.ToJSON Query where
  toJSON Query' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("selectors" Prelude..=) Prelude.<$> selectors]
      )
