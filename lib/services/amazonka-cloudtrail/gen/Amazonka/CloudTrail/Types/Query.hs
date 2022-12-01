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
-- Module      : Amazonka.CloudTrail.Types.Query
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.Query where

import Amazonka.CloudTrail.Types.QueryStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A SQL string of criteria about events that you want to collect in an
-- event data store.
--
-- /See:/ 'newQuery' smart constructor.
data Query = Query'
  { -- | The status of the query. This can be @QUEUED@, @RUNNING@, @FINISHED@,
    -- @FAILED@, @TIMED_OUT@, or @CANCELLED@.
    queryStatus :: Prelude.Maybe QueryStatus,
    -- | The ID of a query.
    queryId :: Prelude.Maybe Prelude.Text,
    -- | The creation time of a query.
    creationTime :: Prelude.Maybe Core.POSIX
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
-- 'queryStatus', 'query_queryStatus' - The status of the query. This can be @QUEUED@, @RUNNING@, @FINISHED@,
-- @FAILED@, @TIMED_OUT@, or @CANCELLED@.
--
-- 'queryId', 'query_queryId' - The ID of a query.
--
-- 'creationTime', 'query_creationTime' - The creation time of a query.
newQuery ::
  Query
newQuery =
  Query'
    { queryStatus = Prelude.Nothing,
      queryId = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The status of the query. This can be @QUEUED@, @RUNNING@, @FINISHED@,
-- @FAILED@, @TIMED_OUT@, or @CANCELLED@.
query_queryStatus :: Lens.Lens' Query (Prelude.Maybe QueryStatus)
query_queryStatus = Lens.lens (\Query' {queryStatus} -> queryStatus) (\s@Query' {} a -> s {queryStatus = a} :: Query)

-- | The ID of a query.
query_queryId :: Lens.Lens' Query (Prelude.Maybe Prelude.Text)
query_queryId = Lens.lens (\Query' {queryId} -> queryId) (\s@Query' {} a -> s {queryId = a} :: Query)

-- | The creation time of a query.
query_creationTime :: Lens.Lens' Query (Prelude.Maybe Prelude.UTCTime)
query_creationTime = Lens.lens (\Query' {creationTime} -> creationTime) (\s@Query' {} a -> s {creationTime = a} :: Query) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Query where
  parseJSON =
    Core.withObject
      "Query"
      ( \x ->
          Query'
            Prelude.<$> (x Core..:? "QueryStatus")
            Prelude.<*> (x Core..:? "QueryId")
            Prelude.<*> (x Core..:? "CreationTime")
      )

instance Prelude.Hashable Query where
  hashWithSalt _salt Query' {..} =
    _salt `Prelude.hashWithSalt` queryStatus
      `Prelude.hashWithSalt` queryId
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData Query where
  rnf Query' {..} =
    Prelude.rnf queryStatus
      `Prelude.seq` Prelude.rnf queryId
      `Prelude.seq` Prelude.rnf creationTime
