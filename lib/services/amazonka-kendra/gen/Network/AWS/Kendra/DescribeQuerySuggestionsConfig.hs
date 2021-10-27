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
-- Module      : Network.AWS.Kendra.DescribeQuerySuggestionsConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings of query suggestions for an index.
--
-- This is used to check the current settings applied to query suggestions.
module Network.AWS.Kendra.DescribeQuerySuggestionsConfig
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
    describeQuerySuggestionsConfigResponse_minimumQueryCount,
    describeQuerySuggestionsConfigResponse_mode,
    describeQuerySuggestionsConfigResponse_lastClearTime,
    describeQuerySuggestionsConfigResponse_totalSuggestionsCount,
    describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation,
    describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers,
    describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime,
    describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays,
    describeQuerySuggestionsConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeQuerySuggestionsConfig' smart constructor.
data DescribeQuerySuggestionsConfig = DescribeQuerySuggestionsConfig'
  { -- | The identifier of the index you want to describe query suggestions
    -- settings for.
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
-- 'indexId', 'describeQuerySuggestionsConfig_indexId' - The identifier of the index you want to describe query suggestions
-- settings for.
newDescribeQuerySuggestionsConfig ::
  -- | 'indexId'
  Prelude.Text ->
  DescribeQuerySuggestionsConfig
newDescribeQuerySuggestionsConfig pIndexId_ =
  DescribeQuerySuggestionsConfig'
    { indexId =
        pIndexId_
    }

-- | The identifier of the index you want to describe query suggestions
-- settings for.
describeQuerySuggestionsConfig_indexId :: Lens.Lens' DescribeQuerySuggestionsConfig Prelude.Text
describeQuerySuggestionsConfig_indexId = Lens.lens (\DescribeQuerySuggestionsConfig' {indexId} -> indexId) (\s@DescribeQuerySuggestionsConfig' {} a -> s {indexId = a} :: DescribeQuerySuggestionsConfig)

instance
  Core.AWSRequest
    DescribeQuerySuggestionsConfig
  where
  type
    AWSResponse DescribeQuerySuggestionsConfig =
      DescribeQuerySuggestionsConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeQuerySuggestionsConfigResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "MinimumQueryCount")
            Prelude.<*> (x Core..?> "Mode")
            Prelude.<*> (x Core..?> "LastClearTime")
            Prelude.<*> (x Core..?> "TotalSuggestionsCount")
            Prelude.<*> (x Core..?> "IncludeQueriesWithoutUserInformation")
            Prelude.<*> (x Core..?> "MinimumNumberOfQueryingUsers")
            Prelude.<*> (x Core..?> "LastSuggestionsBuildTime")
            Prelude.<*> (x Core..?> "QueryLogLookBackWindowInDays")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeQuerySuggestionsConfig

instance
  Prelude.NFData
    DescribeQuerySuggestionsConfig

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
  { -- | Shows whether the status of query suggestions settings is currently
    -- Active or Updating.
    --
    -- Active means the current settings apply and Updating means your changed
    -- settings are in the process of applying.
    status :: Prelude.Maybe QuerySuggestionsStatus,
    -- | Shows the minimum number of times a query must be searched in order for
    -- the query to be eligible to suggest to your users.
    minimumQueryCount :: Prelude.Maybe Prelude.Natural,
    -- | Shows whether query suggestions are currently in @ENABLED@ mode or
    -- @LEARN_ONLY@ mode.
    --
    -- By default, Amazon Kendra enables query suggestions.@LEARN_ONLY@ turns
    -- off query suggestions for your users. You can change the mode using the
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
    -- operation.
    mode :: Prelude.Maybe Mode,
    -- | Shows the date-time query suggestions for an index was last cleared.
    --
    -- After you clear suggestions, Amazon Kendra learns new suggestions based
    -- on new queries added to the query log from the time you cleared
    -- suggestions. Amazon Kendra only considers re-occurences of a query from
    -- the time you cleared suggestions.
    lastClearTime :: Prelude.Maybe Core.POSIX,
    -- | Shows the current total count of query suggestions for an index.
    --
    -- This count can change when you update your query suggestions settings,
    -- if you filter out certain queries from suggestions using a block list,
    -- and as the query log accumulates more queries for Amazon Kendra to learn
    -- from.
    totalSuggestionsCount :: Prelude.Maybe Prelude.Int,
    -- | Shows whether Amazon Kendra uses all queries or only uses queries that
    -- include user information to generate query suggestions.
    includeQueriesWithoutUserInformation :: Prelude.Maybe Prelude.Bool,
    -- | Shows the minimum number of unique users who must search a query in
    -- order for the query to be eligible to suggest to your users.
    minimumNumberOfQueryingUsers :: Prelude.Maybe Prelude.Natural,
    -- | Shows the date-time query suggestions for an index was last updated.
    lastSuggestionsBuildTime :: Prelude.Maybe Core.POSIX,
    -- | Shows how recent your queries are in your query log time window (in
    -- days).
    queryLogLookBackWindowInDays :: Prelude.Maybe Prelude.Int,
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
-- 'status', 'describeQuerySuggestionsConfigResponse_status' - Shows whether the status of query suggestions settings is currently
-- Active or Updating.
--
-- Active means the current settings apply and Updating means your changed
-- settings are in the process of applying.
--
-- 'minimumQueryCount', 'describeQuerySuggestionsConfigResponse_minimumQueryCount' - Shows the minimum number of times a query must be searched in order for
-- the query to be eligible to suggest to your users.
--
-- 'mode', 'describeQuerySuggestionsConfigResponse_mode' - Shows whether query suggestions are currently in @ENABLED@ mode or
-- @LEARN_ONLY@ mode.
--
-- By default, Amazon Kendra enables query suggestions.@LEARN_ONLY@ turns
-- off query suggestions for your users. You can change the mode using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
-- operation.
--
-- 'lastClearTime', 'describeQuerySuggestionsConfigResponse_lastClearTime' - Shows the date-time query suggestions for an index was last cleared.
--
-- After you clear suggestions, Amazon Kendra learns new suggestions based
-- on new queries added to the query log from the time you cleared
-- suggestions. Amazon Kendra only considers re-occurences of a query from
-- the time you cleared suggestions.
--
-- 'totalSuggestionsCount', 'describeQuerySuggestionsConfigResponse_totalSuggestionsCount' - Shows the current total count of query suggestions for an index.
--
-- This count can change when you update your query suggestions settings,
-- if you filter out certain queries from suggestions using a block list,
-- and as the query log accumulates more queries for Amazon Kendra to learn
-- from.
--
-- 'includeQueriesWithoutUserInformation', 'describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation' - Shows whether Amazon Kendra uses all queries or only uses queries that
-- include user information to generate query suggestions.
--
-- 'minimumNumberOfQueryingUsers', 'describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers' - Shows the minimum number of unique users who must search a query in
-- order for the query to be eligible to suggest to your users.
--
-- 'lastSuggestionsBuildTime', 'describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime' - Shows the date-time query suggestions for an index was last updated.
--
-- 'queryLogLookBackWindowInDays', 'describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays' - Shows how recent your queries are in your query log time window (in
-- days).
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
        minimumQueryCount = Prelude.Nothing,
        mode = Prelude.Nothing,
        lastClearTime = Prelude.Nothing,
        totalSuggestionsCount =
          Prelude.Nothing,
        includeQueriesWithoutUserInformation =
          Prelude.Nothing,
        minimumNumberOfQueryingUsers =
          Prelude.Nothing,
        lastSuggestionsBuildTime =
          Prelude.Nothing,
        queryLogLookBackWindowInDays =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Shows whether the status of query suggestions settings is currently
-- Active or Updating.
--
-- Active means the current settings apply and Updating means your changed
-- settings are in the process of applying.
describeQuerySuggestionsConfigResponse_status :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe QuerySuggestionsStatus)
describeQuerySuggestionsConfigResponse_status = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {status} -> status) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {status = a} :: DescribeQuerySuggestionsConfigResponse)

-- | Shows the minimum number of times a query must be searched in order for
-- the query to be eligible to suggest to your users.
describeQuerySuggestionsConfigResponse_minimumQueryCount :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Natural)
describeQuerySuggestionsConfigResponse_minimumQueryCount = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {minimumQueryCount} -> minimumQueryCount) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {minimumQueryCount = a} :: DescribeQuerySuggestionsConfigResponse)

-- | Shows whether query suggestions are currently in @ENABLED@ mode or
-- @LEARN_ONLY@ mode.
--
-- By default, Amazon Kendra enables query suggestions.@LEARN_ONLY@ turns
-- off query suggestions for your users. You can change the mode using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
-- operation.
describeQuerySuggestionsConfigResponse_mode :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Mode)
describeQuerySuggestionsConfigResponse_mode = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {mode} -> mode) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {mode = a} :: DescribeQuerySuggestionsConfigResponse)

-- | Shows the date-time query suggestions for an index was last cleared.
--
-- After you clear suggestions, Amazon Kendra learns new suggestions based
-- on new queries added to the query log from the time you cleared
-- suggestions. Amazon Kendra only considers re-occurences of a query from
-- the time you cleared suggestions.
describeQuerySuggestionsConfigResponse_lastClearTime :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeQuerySuggestionsConfigResponse_lastClearTime = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {lastClearTime} -> lastClearTime) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {lastClearTime = a} :: DescribeQuerySuggestionsConfigResponse) Prelude.. Lens.mapping Core._Time

-- | Shows the current total count of query suggestions for an index.
--
-- This count can change when you update your query suggestions settings,
-- if you filter out certain queries from suggestions using a block list,
-- and as the query log accumulates more queries for Amazon Kendra to learn
-- from.
describeQuerySuggestionsConfigResponse_totalSuggestionsCount :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Int)
describeQuerySuggestionsConfigResponse_totalSuggestionsCount = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {totalSuggestionsCount} -> totalSuggestionsCount) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {totalSuggestionsCount = a} :: DescribeQuerySuggestionsConfigResponse)

-- | Shows whether Amazon Kendra uses all queries or only uses queries that
-- include user information to generate query suggestions.
describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Bool)
describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {includeQueriesWithoutUserInformation} -> includeQueriesWithoutUserInformation) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {includeQueriesWithoutUserInformation = a} :: DescribeQuerySuggestionsConfigResponse)

-- | Shows the minimum number of unique users who must search a query in
-- order for the query to be eligible to suggest to your users.
describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Natural)
describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {minimumNumberOfQueryingUsers} -> minimumNumberOfQueryingUsers) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {minimumNumberOfQueryingUsers = a} :: DescribeQuerySuggestionsConfigResponse)

-- | Shows the date-time query suggestions for an index was last updated.
describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.UTCTime)
describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {lastSuggestionsBuildTime} -> lastSuggestionsBuildTime) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {lastSuggestionsBuildTime = a} :: DescribeQuerySuggestionsConfigResponse) Prelude.. Lens.mapping Core._Time

-- | Shows how recent your queries are in your query log time window (in
-- days).
describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays :: Lens.Lens' DescribeQuerySuggestionsConfigResponse (Prelude.Maybe Prelude.Int)
describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {queryLogLookBackWindowInDays} -> queryLogLookBackWindowInDays) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {queryLogLookBackWindowInDays = a} :: DescribeQuerySuggestionsConfigResponse)

-- | The response's http status code.
describeQuerySuggestionsConfigResponse_httpStatus :: Lens.Lens' DescribeQuerySuggestionsConfigResponse Prelude.Int
describeQuerySuggestionsConfigResponse_httpStatus = Lens.lens (\DescribeQuerySuggestionsConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeQuerySuggestionsConfigResponse' {} a -> s {httpStatus = a} :: DescribeQuerySuggestionsConfigResponse)

instance
  Prelude.NFData
    DescribeQuerySuggestionsConfigResponse
