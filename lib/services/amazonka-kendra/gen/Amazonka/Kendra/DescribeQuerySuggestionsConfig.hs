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
    describeQuerySuggestionsConfigResponse_status,
    describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers,
    describeQuerySuggestionsConfigResponse_totalSuggestionsCount,
    describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime,
    describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays,
    describeQuerySuggestionsConfigResponse_mode,
    describeQuerySuggestionsConfigResponse_minimumQueryCount,
    describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation,
    describeQuerySuggestionsConfigResponse_lastClearTime,
    describeQuerySuggestionsConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "MinimumNumberOfQueryingUsers")
            Prelude.<*> (x Core..?> "TotalSuggestionsCount")
            Prelude.<*> (x Core..?> "LastSuggestionsBuildTime")
            Prelude.<*> (x Core..?> "QueryLogLookBackWindowInDays")
            Prelude.<*> (x Core..?> "Mode")
            Prelude.<*> (x Core..?> "MinimumQueryCount")
            Prelude.<*> (x Core..?> "IncludeQueriesWithoutUserInformation")
            Prelude.<*> (x Core..?> "LastClearTime")
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
  Core.ToHeaders
    DescribeQuerySuggestionsConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DescribeQuerySuggestionsConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeQuerySuggestionsConfig where
  toJSON DescribeQuerySuggestionsConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("IndexId" Core..= indexId)]
      )

instance Core.ToPath DescribeQuerySuggestionsConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeQuerySuggestionsConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeQuerySuggestionsConfigResponse' smart constructor.
data DescribeQuerySuggestionsConfigResponse = DescribeQuerySuggestionsConfigResponse'
  { -- | Whether the status of query suggestions settings is currently @ACTIVE@
    -- or @UPDATING@.
    --
    -- Active means the current settings apply and Updating means your changed
    -- settings are in the process of applying.
    status :: Prelude.Maybe QuerySuggestionsStatus,
    -- | The minimum number of unique users who must search a query in order for
    -- the query to be eligible to suggest to your users.
    minimumNumberOfQueryingUsers :: Prelude.Maybe Prelude.Natural,
    -- | The current total count of query suggestions for an index.
    --
    -- This count can change when you update your query suggestions settings,
    -- if you filter out certain queries from suggestions using a block list,
    -- and as the query log accumulates more queries for Amazon Kendra to learn
    -- from.
    totalSuggestionsCount :: Prelude.Maybe Prelude.Int,
    -- | The date-time query suggestions for an index was last updated.
    lastSuggestionsBuildTime :: Prelude.Maybe Core.POSIX,
    -- | How recent your queries are in your query log time window (in days).
    queryLogLookBackWindowInDays :: Prelude.Maybe Prelude.Int,
    -- | Whether query suggestions are currently in @ENABLED@ mode or
    -- @LEARN_ONLY@ mode.
    --
    -- By default, Amazon Kendra enables query suggestions.@LEARN_ONLY@ turns
    -- off query suggestions for your users. You can change the mode using the
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
    -- API.
    mode :: Prelude.Maybe Mode,
    -- | The minimum number of times a query must be searched in order for the
    -- query to be eligible to suggest to your users.
    minimumQueryCount :: Prelude.Maybe Prelude.Natural,
    -- | @TRUE@ to use all queries, otherwise use only queries that include user
    -- information to generate the query suggestions.
    includeQueriesWithoutUserInformation :: Prelude.Maybe Prelude.Bool,
    -- | The date-time query suggestions for an index was last cleared.
    --
    -- After you clear suggestions, Amazon Kendra learns new suggestions based
    -- on new queries added to the query log from the time you cleared
    -- suggestions. Amazon Kendra only considers re-occurences of a query from
    -- the time you cleared suggestions.
    lastClearTime :: Prelude.Maybe Core.POSIX,
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
-- 'status', 'describeQuerySuggestionsConfigResponse_status' - Whether the status of query suggestions settings is currently @ACTIVE@
-- or @UPDATING@.
--
-- Active means the current settings apply and Updating means your changed
-- settings are in the process of applying.
--
-- 'minimumNumberOfQueryingUsers', 'describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers' - The minimum number of unique users who must search a query in order for
-- the query to be eligible to suggest to your users.
--
-- 'totalSuggestionsCount', 'describeQuerySuggestionsConfigResponse_totalSuggestionsCount' - The current total count of query suggestions for an index.
--
-- This count can change when you update your query suggestions settings,
-- if you filter out certain queries from suggestions using a block list,
-- and as the query log accumulates more queries for Amazon Kendra to learn
-- from.
--
-- 'lastSuggestionsBuildTime', 'describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime' - The date-time query suggestions for an index was last updated.
--
-- 'queryLogLookBackWindowInDays', 'describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays' - How recent your queries are in your query log time window (in days).
--
-- 'mode', 'describeQuerySuggestionsConfigResponse_mode' - Whether query suggestions are currently in @ENABLED@ mode or
-- @LEARN_ONLY@ mode.
--
-- By default, Amazon Kendra enables query suggestions.@LEARN_ONLY@ turns
-- off query suggestions for your users. You can change the mode using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
-- API.
--
-- 'minimumQueryCount', 'describeQuerySuggestionsConfigResponse_minimumQueryCount' - The minimum number of times a query must be searched in order for the
-- query to be eligible to suggest to your users.
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
-- 'httpStatus', 'describeQuerySuggestionsConfigResponse_httpStatus' - The response's http status code.
newDescribeQuerySuggestionsConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeQuerySuggestionsConfigResponse
newDescribeQuerySuggestionsConfigResponse
  pHttpStatus_ =
    DescribeQuerySuggestionsConfigResponse'
      { status =
          Prelude.Nothing,
        minimumNumberOfQueryingUsers =
          Prelude.Nothing,
        totalSuggestionsCount =
          Prelude.Nothing,
        lastSuggestionsBuildTime =
          Prelude.Nothing,
        queryLogLookBackWindowInDays =
          Prelude.Nothing,
        mode = Prelude.Nothing,
        minimumQueryCount = Prelude.Nothing,
        includeQueriesWithoutUserInformation =
          Prelude.Nothing,
        lastClearTime = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Whether the status of query suggestions settings is currently @ACTIVE@
-- or @UPDATING@.
--
-- Active means the current settings apply and Updating means your changed
-- settings are in the process of applying.
describeQuerySuggestionsConfigResponse_status :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe QuerySuggestionsStatus)
describeQuerySuggestionsConfigResponse_status = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {status} -> status) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {status = a} :: DescribeQuerySuggestionsConfigResponse)

-- | The minimum number of unique users who must search a query in order for
-- the query to be eligible to suggest to your users.
describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Natural)
describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {minimumNumberOfQueryingUsers} -> minimumNumberOfQueryingUsers) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {minimumNumberOfQueryingUsers = a} :: DescribeQuerySuggestionsConfigResponse)

-- | The current total count of query suggestions for an index.
--
-- This count can change when you update your query suggestions settings,
-- if you filter out certain queries from suggestions using a block list,
-- and as the query log accumulates more queries for Amazon Kendra to learn
-- from.
describeQuerySuggestionsConfigResponse_totalSuggestionsCount :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Int)
describeQuerySuggestionsConfigResponse_totalSuggestionsCount = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {totalSuggestionsCount} -> totalSuggestionsCount) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {totalSuggestionsCount = a} :: DescribeQuerySuggestionsConfigResponse)

-- | The date-time query suggestions for an index was last updated.
describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {lastSuggestionsBuildTime} -> lastSuggestionsBuildTime) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {lastSuggestionsBuildTime = a} :: DescribeQuerySuggestionsConfigResponse) Prelude.. Lens.mapping Core._Time

-- | How recent your queries are in your query log time window (in days).
describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Int)
describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {queryLogLookBackWindowInDays} -> queryLogLookBackWindowInDays) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {queryLogLookBackWindowInDays = a} :: DescribeQuerySuggestionsConfigResponse)

-- | Whether query suggestions are currently in @ENABLED@ mode or
-- @LEARN_ONLY@ mode.
--
-- By default, Amazon Kendra enables query suggestions.@LEARN_ONLY@ turns
-- off query suggestions for your users. You can change the mode using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
-- API.
describeQuerySuggestionsConfigResponse_mode :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Mode)
describeQuerySuggestionsConfigResponse_mode = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {mode} -> mode) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {mode = a} :: DescribeQuerySuggestionsConfigResponse)

-- | The minimum number of times a query must be searched in order for the
-- query to be eligible to suggest to your users.
describeQuerySuggestionsConfigResponse_minimumQueryCount :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Natural)
describeQuerySuggestionsConfigResponse_minimumQueryCount = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {minimumQueryCount} -> minimumQueryCount) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {minimumQueryCount = a} :: DescribeQuerySuggestionsConfigResponse)

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
describeQuerySuggestionsConfigResponse_lastClearTime = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {lastClearTime} -> lastClearTime) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {lastClearTime = a} :: DescribeQuerySuggestionsConfigResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeQuerySuggestionsConfigResponse_httpStatus :: Lens.Lens' DescribeQuerySuggestionsConfigResponse Prelude.Int
describeQuerySuggestionsConfigResponse_httpStatus = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {httpStatus = a} :: DescribeQuerySuggestionsConfigResponse)

instance
  Prelude.NFData
    DescribeQuerySuggestionsConfigResponse
  where
  rnf DescribeQuerySuggestionsConfigResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf minimumNumberOfQueryingUsers
      `Prelude.seq` Prelude.rnf totalSuggestionsCount
      `Prelude.seq` Prelude.rnf lastSuggestionsBuildTime
      `Prelude.seq` Prelude.rnf queryLogLookBackWindowInDays
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf minimumQueryCount
      `Prelude.seq` Prelude.rnf includeQueriesWithoutUserInformation
      `Prelude.seq` Prelude.rnf lastClearTime
      `Prelude.seq` Prelude.rnf httpStatus
