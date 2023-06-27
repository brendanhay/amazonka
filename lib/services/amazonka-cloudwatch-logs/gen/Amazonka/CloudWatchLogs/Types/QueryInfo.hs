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
-- Module      : Amazonka.CloudWatchLogs.Types.QueryInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.QueryInfo where

import Amazonka.CloudWatchLogs.Types.QueryStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about one CloudWatch Logs Insights query that matches the
-- request in a @DescribeQueries@ operation.
--
-- /See:/ 'newQueryInfo' smart constructor.
data QueryInfo = QueryInfo'
  { -- | The date and time that this query was created.
    createTime :: Prelude.Maybe Prelude.Natural,
    -- | The name of the log group scanned by this query.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The unique ID number of this query.
    queryId :: Prelude.Maybe Prelude.Text,
    -- | The query string used in this query.
    queryString :: Prelude.Maybe Prelude.Text,
    -- | The status of this query. Possible values are @Cancelled@, @Complete@,
    -- @Failed@, @Running@, @Scheduled@, and @Unknown@.
    status :: Prelude.Maybe QueryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTime', 'queryInfo_createTime' - The date and time that this query was created.
--
-- 'logGroupName', 'queryInfo_logGroupName' - The name of the log group scanned by this query.
--
-- 'queryId', 'queryInfo_queryId' - The unique ID number of this query.
--
-- 'queryString', 'queryInfo_queryString' - The query string used in this query.
--
-- 'status', 'queryInfo_status' - The status of this query. Possible values are @Cancelled@, @Complete@,
-- @Failed@, @Running@, @Scheduled@, and @Unknown@.
newQueryInfo ::
  QueryInfo
newQueryInfo =
  QueryInfo'
    { createTime = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      queryId = Prelude.Nothing,
      queryString = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time that this query was created.
queryInfo_createTime :: Lens.Lens' QueryInfo (Prelude.Maybe Prelude.Natural)
queryInfo_createTime = Lens.lens (\QueryInfo' {createTime} -> createTime) (\s@QueryInfo' {} a -> s {createTime = a} :: QueryInfo)

-- | The name of the log group scanned by this query.
queryInfo_logGroupName :: Lens.Lens' QueryInfo (Prelude.Maybe Prelude.Text)
queryInfo_logGroupName = Lens.lens (\QueryInfo' {logGroupName} -> logGroupName) (\s@QueryInfo' {} a -> s {logGroupName = a} :: QueryInfo)

-- | The unique ID number of this query.
queryInfo_queryId :: Lens.Lens' QueryInfo (Prelude.Maybe Prelude.Text)
queryInfo_queryId = Lens.lens (\QueryInfo' {queryId} -> queryId) (\s@QueryInfo' {} a -> s {queryId = a} :: QueryInfo)

-- | The query string used in this query.
queryInfo_queryString :: Lens.Lens' QueryInfo (Prelude.Maybe Prelude.Text)
queryInfo_queryString = Lens.lens (\QueryInfo' {queryString} -> queryString) (\s@QueryInfo' {} a -> s {queryString = a} :: QueryInfo)

-- | The status of this query. Possible values are @Cancelled@, @Complete@,
-- @Failed@, @Running@, @Scheduled@, and @Unknown@.
queryInfo_status :: Lens.Lens' QueryInfo (Prelude.Maybe QueryStatus)
queryInfo_status = Lens.lens (\QueryInfo' {status} -> status) (\s@QueryInfo' {} a -> s {status = a} :: QueryInfo)

instance Data.FromJSON QueryInfo where
  parseJSON =
    Data.withObject
      "QueryInfo"
      ( \x ->
          QueryInfo'
            Prelude.<$> (x Data..:? "createTime")
            Prelude.<*> (x Data..:? "logGroupName")
            Prelude.<*> (x Data..:? "queryId")
            Prelude.<*> (x Data..:? "queryString")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable QueryInfo where
  hashWithSalt _salt QueryInfo' {..} =
    _salt
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` queryId
      `Prelude.hashWithSalt` queryString
      `Prelude.hashWithSalt` status

instance Prelude.NFData QueryInfo where
  rnf QueryInfo' {..} =
    Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf queryId
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf status
