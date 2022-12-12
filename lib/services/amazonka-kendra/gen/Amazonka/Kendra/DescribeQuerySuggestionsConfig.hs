{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kendra.DescribeQuerySuggestionsConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information on the settings of query suggestions for an index.
--
-- This is used to check the current settings applied to query suggestions.
--
-- @DescribeQuerySuggestionsConfig@ is currently not supported in the
-- Amazon Web Services GovCloud (US-West) region.
module Amazonka.Kendra.DescribeQuerySuggestionsConfig
  ( -- * Creating a Request
    DescribeQuerySuggestionsConfig (..),
    newDescribeQuerySuggestionsConfig,

    -- * Request Lenses
    describeQuerySuggestionsConfig_indexId,

    -- * Destructuring the Response
    DescribeQuerySuggestionsConfigResponse (..),
    newDescribeQuerySuggestionsConfigResponse,

    -- * Response Lenses
    describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation,
    describeQuerySuggestionsConfigResponse_lastClearTime,
    describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime,
    describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers,
    describeQuerySuggestionsConfigResponse_minimumQueryCount,
    describeQuerySuggestionsConfigResponse_mode,
    describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays,
    describeQuerySuggestionsConfigResponse_status,
    describeQuerySuggestionsConfigResponse_totalSuggestionsCount,
    describeQuerySuggestionsConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeQuerySuggestionsConfig' smart constructor.
data DescribeQuerySuggestionsConfig = DescribeQuerySuggestionsConfig'
  { -- | The identifier of the index with query suggestions that you want to get
    -- information on.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQuerySuggestionsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexId', 'describeQuerySuggestionsConfig_indexId' - The identifier of the index with query suggestions that you want to get
-- information on.
newDescribeQuerySuggestionsConfig ::
  -- | 'indexId'
  Prelude.Text ->
  DescribeQuerySuggestionsConfig
newDescribeQuerySuggestionsConfig pIndexId_ =
  DescribeQuerySuggestionsConfig'
    { indexId =
        pIndexId_
    }

-- | The identifier of the index with query suggestions that you want to get
-- information on.
describeQuerySuggestionsConfig_indexId :: Lens.Lens' DescribeQuerySuggestionsConfig Prelude.Text
describeQuerySuggestionsConfig_indexId = Lens.lens (\DescribeQuerySuggestionsConfig' {indexId} -> indexId) (\s@DescribeQuerySuggestionsConfig' {} a -> s {indexId = a} :: DescribeQuerySuggestionsConfig)

instance
  Core.AWSRequest
    DescribeQuerySuggestionsConfig
  where
  type
    AWSResponse DescribeQuerySuggestionsConfig =
      DescribeQuerySuggestionsConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeQuerySuggestionsConfigResponse'
            Prelude.<$> (x Data..?> "IncludeQueriesWithoutUserInformation")
            Prelude.<*> (x Data..?> "LastClearTime")
            Prelude.<*> (x Data..?> "LastSuggestionsBuildTime")
            Prelude.<*> (x Data..?> "MinimumNumberOfQueryingUsers")
            Prelude.<*> (x Data..?> "MinimumQueryCount")
            Prelude.<*> (x Data..?> "Mode")
            Prelude.<*> (x Data..?> "QueryLogLookBackWindowInDays")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TotalSuggestionsCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeQuerySuggestionsConfig
  where
  hashWithSalt
    _salt
    DescribeQuerySuggestionsConfig' {..} =
      _salt `Prelude.hashWithSalt` indexId

instance
  Prelude.NFData
    DescribeQuerySuggestionsConfig
  where
  rnf DescribeQuerySuggestionsConfig' {..} =
    Prelude.rnf indexId

instance
  Data.ToHeaders
    DescribeQuerySuggestionsConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DescribeQuerySuggestionsConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeQuerySuggestionsConfig where
  toJSON DescribeQuerySuggestionsConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("IndexId" Data..= indexId)]
      )

instance Data.ToPath DescribeQuerySuggestionsConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeQuerySuggestionsConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeQuerySuggestionsConfigResponse' smart constructor.
data DescribeQuerySuggestionsConfigResponse = DescribeQuerySuggestionsConfigResponse'
  { -- | @TRUE@ to use all queries, otherwise use only queries that include user
    -- information to generate the query suggestions.
    includeQueriesWithoutUserInformation :: Prelude.Maybe Prelude.Bool,
    -- | The date-time query suggestions for an index was last cleared.
    --
    -- After you clear suggestions, Amazon Kendra learns new suggestions based
    -- on new queries added to the query log from the time you cleared
    -- suggestions. Amazon Kendra only considers re-occurences of a query from
    -- the time you cleared suggestions.
    lastClearTime :: Prelude.Maybe Data.POSIX,
    -- | The date-time query suggestions for an index was last updated.
    lastSuggestionsBuildTime :: Prelude.Maybe Data.POSIX,
    -- | The minimum number of unique users who must search a query in order for
    -- the query to be eligible to suggest to your users.
    minimumNumberOfQueryingUsers :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of times a query must be searched in order for the
    -- query to be eligible to suggest to your users.
    minimumQueryCount :: Prelude.Maybe Prelude.Natural,
    -- | Whether query suggestions are currently in @ENABLED@ mode or
    -- @LEARN_ONLY@ mode.
    --
    -- By default, Amazon Kendra enables query suggestions.@LEARN_ONLY@ turns
    -- off query suggestions for your users. You can change the mode using the
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
    -- API.
    mode :: Prelude.Maybe Mode,
    -- | How recent your queries are in your query log time window (in days).
    queryLogLookBackWindowInDays :: Prelude.Maybe Prelude.Int,
    -- | Whether the status of query suggestions settings is currently @ACTIVE@
    -- or @UPDATING@.
    --
    -- Active means the current settings apply and Updating means your changed
    -- settings are in the process of applying.
    status :: Prelude.Maybe QuerySuggestionsStatus,
    -- | The current total count of query suggestions for an index.
    --
    -- This count can change when you update your query suggestions settings,
    -- if you filter out certain queries from suggestions using a block list,
    -- and as the query log accumulates more queries for Amazon Kendra to learn
    -- from.
    totalSuggestionsCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQuerySuggestionsConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeQueriesWithoutUserInformation', 'describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation' - @TRUE@ to use all queries, otherwise use only queries that include user
-- information to generate the query suggestions.
--
-- 'lastClearTime', 'describeQuerySuggestionsConfigResponse_lastClearTime' - The date-time query suggestions for an index was last cleared.
--
-- After you clear suggestions, Amazon Kendra learns new suggestions based
-- on new queries added to the query log from the time you cleared
-- suggestions. Amazon Kendra only considers re-occurences of a query from
-- the time you cleared suggestions.
--
-- 'lastSuggestionsBuildTime', 'describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime' - The date-time query suggestions for an index was last updated.
--
-- 'minimumNumberOfQueryingUsers', 'describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers' - The minimum number of unique users who must search a query in order for
-- the query to be eligible to suggest to your users.
--
-- 'minimumQueryCount', 'describeQuerySuggestionsConfigResponse_minimumQueryCount' - The minimum number of times a query must be searched in order for the
-- query to be eligible to suggest to your users.
--
-- 'mode', 'describeQuerySuggestionsConfigResponse_mode' - Whether query suggestions are currently in @ENABLED@ mode or
-- @LEARN_ONLY@ mode.
--
-- By default, Amazon Kendra enables query suggestions.@LEARN_ONLY@ turns
-- off query suggestions for your users. You can change the mode using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
-- API.
--
-- 'queryLogLookBackWindowInDays', 'describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays' - How recent your queries are in your query log time window (in days).
--
-- 'status', 'describeQuerySuggestionsConfigResponse_status' - Whether the status of query suggestions settings is currently @ACTIVE@
-- or @UPDATING@.
--
-- Active means the current settings apply and Updating means your changed
-- settings are in the process of applying.
--
-- 'totalSuggestionsCount', 'describeQuerySuggestionsConfigResponse_totalSuggestionsCount' - The current total count of query suggestions for an index.
--
-- This count can change when you update your query suggestions settings,
-- if you filter out certain queries from suggestions using a block list,
-- and as the query log accumulates more queries for Amazon Kendra to learn
-- from.
--
-- 'httpStatus', 'describeQuerySuggestionsConfigResponse_httpStatus' - The response's http status code.
newDescribeQuerySuggestionsConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeQuerySuggestionsConfigResponse
newDescribeQuerySuggestionsConfigResponse
  pHttpStatus_ =
    DescribeQuerySuggestionsConfigResponse'
      { includeQueriesWithoutUserInformation =
          Prelude.Nothing,
        lastClearTime = Prelude.Nothing,
        lastSuggestionsBuildTime =
          Prelude.Nothing,
        minimumNumberOfQueryingUsers =
          Prelude.Nothing,
        minimumQueryCount = Prelude.Nothing,
        mode = Prelude.Nothing,
        queryLogLookBackWindowInDays =
          Prelude.Nothing,
        status = Prelude.Nothing,
        totalSuggestionsCount =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | @TRUE@ to use all queries, otherwise use only queries that include user
-- information to generate the query suggestions.
describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Bool)
describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {includeQueriesWithoutUserInformation} -> includeQueriesWithoutUserInformation) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {includeQueriesWithoutUserInformation = a} :: DescribeQuerySuggestionsConfigResponse)

-- | The date-time query suggestions for an index was last cleared.
--
-- After you clear suggestions, Amazon Kendra learns new suggestions based
-- on new queries added to the query log from the time you cleared
-- suggestions. Amazon Kendra only considers re-occurences of a query from
-- the time you cleared suggestions.
describeQuerySuggestionsConfigResponse_lastClearTime :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeQuerySuggestionsConfigResponse_lastClearTime = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {lastClearTime} -> lastClearTime) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {lastClearTime = a} :: DescribeQuerySuggestionsConfigResponse) Prelude.. Lens.mapping Data._Time

-- | The date-time query suggestions for an index was last updated.
describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {lastSuggestionsBuildTime} -> lastSuggestionsBuildTime) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {lastSuggestionsBuildTime = a} :: DescribeQuerySuggestionsConfigResponse) Prelude.. Lens.mapping Data._Time

-- | The minimum number of unique users who must search a query in order for
-- the query to be eligible to suggest to your users.
describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Natural)
describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {minimumNumberOfQueryingUsers} -> minimumNumberOfQueryingUsers) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {minimumNumberOfQueryingUsers = a} :: DescribeQuerySuggestionsConfigResponse)

-- | The minimum number of times a query must be searched in order for the
-- query to be eligible to suggest to your users.
describeQuerySuggestionsConfigResponse_minimumQueryCount :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Natural)
describeQuerySuggestionsConfigResponse_minimumQueryCount = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {minimumQueryCount} -> minimumQueryCount) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {minimumQueryCount = a} :: DescribeQuerySuggestionsConfigResponse)

-- | Whether query suggestions are currently in @ENABLED@ mode or
-- @LEARN_ONLY@ mode.
--
-- By default, Amazon Kendra enables query suggestions.@LEARN_ONLY@ turns
-- off query suggestions for your users. You can change the mode using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
-- API.
describeQuerySuggestionsConfigResponse_mode :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Mode)
describeQuerySuggestionsConfigResponse_mode = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {mode} -> mode) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {mode = a} :: DescribeQuerySuggestionsConfigResponse)

-- | How recent your queries are in your query log time window (in days).
describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Int)
describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {queryLogLookBackWindowInDays} -> queryLogLookBackWindowInDays) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {queryLogLookBackWindowInDays = a} :: DescribeQuerySuggestionsConfigResponse)

-- | Whether the status of query suggestions settings is currently @ACTIVE@
-- or @UPDATING@.
--
-- Active means the current settings apply and Updating means your changed
-- settings are in the process of applying.
describeQuerySuggestionsConfigResponse_status :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe QuerySuggestionsStatus)
describeQuerySuggestionsConfigResponse_status = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {status} -> status) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {status = a} :: DescribeQuerySuggestionsConfigResponse)

-- | The current total count of query suggestions for an index.
--
-- This count can change when you update your query suggestions settings,
-- if you filter out certain queries from suggestions using a block list,
-- and as the query log accumulates more queries for Amazon Kendra to learn
-- from.
describeQuerySuggestionsConfigResponse_totalSuggestionsCount :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Int)
describeQuerySuggestionsConfigResponse_totalSuggestionsCount = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {totalSuggestionsCount} -> totalSuggestionsCount) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {totalSuggestionsCount = a} :: DescribeQuerySuggestionsConfigResponse)

-- | The response's http status code.
describeQuerySuggestionsConfigResponse_httpStatus :: Lens.Lens' DescribeQuerySuggestionsConfigResponse Prelude.Int
describeQuerySuggestionsConfigResponse_httpStatus = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {httpStatus = a} :: DescribeQuerySuggestionsConfigResponse)

instance
  Prelude.NFData
    DescribeQuerySuggestionsConfigResponse
  where
  rnf DescribeQuerySuggestionsConfigResponse' {..} =
    Prelude.rnf includeQueriesWithoutUserInformation
      `Prelude.seq` Prelude.rnf lastClearTime
      `Prelude.seq` Prelude.rnf lastSuggestionsBuildTime
      `Prelude.seq` Prelude.rnf minimumNumberOfQueryingUsers
      `Prelude.seq` Prelude.rnf minimumQueryCount
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf queryLogLookBackWindowInDays
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf totalSuggestionsCount
      `Prelude.seq` Prelude.rnf httpStatus
