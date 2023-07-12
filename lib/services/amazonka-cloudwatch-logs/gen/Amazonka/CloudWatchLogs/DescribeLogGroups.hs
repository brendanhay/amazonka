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
-- Module      : Amazonka.CloudWatchLogs.DescribeLogGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified log groups. You can list all your log groups or
-- filter the results by prefix. The results are ASCII-sorted by log group
-- name.
--
-- CloudWatch Logs doesn’t support IAM policies that control access to the
-- @DescribeLogGroups@ action by using the
-- @aws:ResourceTag\/@/@key-name@/@ @ condition key. Other CloudWatch Logs
-- actions do support the use of the @aws:ResourceTag\/@/@key-name@/@ @
-- condition key to control access. For more information about using tags
-- to control access, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
--
-- If you are using CloudWatch cross-account observability, you can use
-- this operation in a monitoring account and view data from the linked
-- source accounts. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Unified-Cross-Account.html CloudWatch cross-account observability>.
--
-- This operation returns paginated results.
module Amazonka.CloudWatchLogs.DescribeLogGroups
  ( -- * Creating a Request
    DescribeLogGroups (..),
    newDescribeLogGroups,

    -- * Request Lenses
    describeLogGroups_accountIdentifiers,
    describeLogGroups_includeLinkedAccounts,
    describeLogGroups_limit,
    describeLogGroups_logGroupNamePattern,
    describeLogGroups_logGroupNamePrefix,
    describeLogGroups_nextToken,

    -- * Destructuring the Response
    DescribeLogGroupsResponse (..),
    newDescribeLogGroupsResponse,

    -- * Response Lenses
    describeLogGroupsResponse_logGroups,
    describeLogGroupsResponse_nextToken,
    describeLogGroupsResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLogGroups' smart constructor.
data DescribeLogGroups = DescribeLogGroups'
  { -- | When @includeLinkedAccounts@ is set to @True@, use this parameter to
    -- specify the list of accounts to search. You can specify as many as 20
    -- account IDs in the array.
    accountIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | If you are using a monitoring account, set this to @True@ to have the
    -- operation return log groups in the accounts listed in
    -- @accountIdentifiers@.
    --
    -- If this parameter is set to @true@ and @accountIdentifiers@ contains a
    -- null value, the operation returns all log groups in the monitoring
    -- account and all log groups in all source accounts that are linked to the
    -- monitoring account.
    --
    -- If you specify @includeLinkedAccounts@ in your request, then
    -- @metricFilterCount@, @retentionInDays@, and @storedBytes@ are not
    -- included in the response.
    includeLinkedAccounts :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default is up to 50 items.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you specify a string for this parameter, the operation returns only
    -- log groups that have names that match the string based on a
    -- case-sensitive substring search. For example, if you specify @Foo@, log
    -- groups named @FooBar@, @aws\/Foo@, and @GroupFoo@ would match, but
    -- @foo@, @F\/o\/o@ and @Froo@ would not match.
    --
    -- @logGroupNamePattern@ and @logGroupNamePrefix@ are mutually exclusive.
    -- Only one of these parameters can be passed.
    logGroupNamePattern :: Prelude.Maybe Prelude.Text,
    -- | The prefix to match.
    --
    -- @logGroupNamePrefix@ and @logGroupNamePattern@ are mutually exclusive.
    -- Only one of these parameters can be passed.
    logGroupNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLogGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIdentifiers', 'describeLogGroups_accountIdentifiers' - When @includeLinkedAccounts@ is set to @True@, use this parameter to
-- specify the list of accounts to search. You can specify as many as 20
-- account IDs in the array.
--
-- 'includeLinkedAccounts', 'describeLogGroups_includeLinkedAccounts' - If you are using a monitoring account, set this to @True@ to have the
-- operation return log groups in the accounts listed in
-- @accountIdentifiers@.
--
-- If this parameter is set to @true@ and @accountIdentifiers@ contains a
-- null value, the operation returns all log groups in the monitoring
-- account and all log groups in all source accounts that are linked to the
-- monitoring account.
--
-- If you specify @includeLinkedAccounts@ in your request, then
-- @metricFilterCount@, @retentionInDays@, and @storedBytes@ are not
-- included in the response.
--
-- 'limit', 'describeLogGroups_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
--
-- 'logGroupNamePattern', 'describeLogGroups_logGroupNamePattern' - If you specify a string for this parameter, the operation returns only
-- log groups that have names that match the string based on a
-- case-sensitive substring search. For example, if you specify @Foo@, log
-- groups named @FooBar@, @aws\/Foo@, and @GroupFoo@ would match, but
-- @foo@, @F\/o\/o@ and @Froo@ would not match.
--
-- @logGroupNamePattern@ and @logGroupNamePrefix@ are mutually exclusive.
-- Only one of these parameters can be passed.
--
-- 'logGroupNamePrefix', 'describeLogGroups_logGroupNamePrefix' - The prefix to match.
--
-- @logGroupNamePrefix@ and @logGroupNamePattern@ are mutually exclusive.
-- Only one of these parameters can be passed.
--
-- 'nextToken', 'describeLogGroups_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
newDescribeLogGroups ::
  DescribeLogGroups
newDescribeLogGroups =
  DescribeLogGroups'
    { accountIdentifiers =
        Prelude.Nothing,
      includeLinkedAccounts = Prelude.Nothing,
      limit = Prelude.Nothing,
      logGroupNamePattern = Prelude.Nothing,
      logGroupNamePrefix = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | When @includeLinkedAccounts@ is set to @True@, use this parameter to
-- specify the list of accounts to search. You can specify as many as 20
-- account IDs in the array.
describeLogGroups_accountIdentifiers :: Lens.Lens' DescribeLogGroups (Prelude.Maybe [Prelude.Text])
describeLogGroups_accountIdentifiers = Lens.lens (\DescribeLogGroups' {accountIdentifiers} -> accountIdentifiers) (\s@DescribeLogGroups' {} a -> s {accountIdentifiers = a} :: DescribeLogGroups) Prelude.. Lens.mapping Lens.coerced

-- | If you are using a monitoring account, set this to @True@ to have the
-- operation return log groups in the accounts listed in
-- @accountIdentifiers@.
--
-- If this parameter is set to @true@ and @accountIdentifiers@ contains a
-- null value, the operation returns all log groups in the monitoring
-- account and all log groups in all source accounts that are linked to the
-- monitoring account.
--
-- If you specify @includeLinkedAccounts@ in your request, then
-- @metricFilterCount@, @retentionInDays@, and @storedBytes@ are not
-- included in the response.
describeLogGroups_includeLinkedAccounts :: Lens.Lens' DescribeLogGroups (Prelude.Maybe Prelude.Bool)
describeLogGroups_includeLinkedAccounts = Lens.lens (\DescribeLogGroups' {includeLinkedAccounts} -> includeLinkedAccounts) (\s@DescribeLogGroups' {} a -> s {includeLinkedAccounts = a} :: DescribeLogGroups)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
describeLogGroups_limit :: Lens.Lens' DescribeLogGroups (Prelude.Maybe Prelude.Natural)
describeLogGroups_limit = Lens.lens (\DescribeLogGroups' {limit} -> limit) (\s@DescribeLogGroups' {} a -> s {limit = a} :: DescribeLogGroups)

-- | If you specify a string for this parameter, the operation returns only
-- log groups that have names that match the string based on a
-- case-sensitive substring search. For example, if you specify @Foo@, log
-- groups named @FooBar@, @aws\/Foo@, and @GroupFoo@ would match, but
-- @foo@, @F\/o\/o@ and @Froo@ would not match.
--
-- @logGroupNamePattern@ and @logGroupNamePrefix@ are mutually exclusive.
-- Only one of these parameters can be passed.
describeLogGroups_logGroupNamePattern :: Lens.Lens' DescribeLogGroups (Prelude.Maybe Prelude.Text)
describeLogGroups_logGroupNamePattern = Lens.lens (\DescribeLogGroups' {logGroupNamePattern} -> logGroupNamePattern) (\s@DescribeLogGroups' {} a -> s {logGroupNamePattern = a} :: DescribeLogGroups)

-- | The prefix to match.
--
-- @logGroupNamePrefix@ and @logGroupNamePattern@ are mutually exclusive.
-- Only one of these parameters can be passed.
describeLogGroups_logGroupNamePrefix :: Lens.Lens' DescribeLogGroups (Prelude.Maybe Prelude.Text)
describeLogGroups_logGroupNamePrefix = Lens.lens (\DescribeLogGroups' {logGroupNamePrefix} -> logGroupNamePrefix) (\s@DescribeLogGroups' {} a -> s {logGroupNamePrefix = a} :: DescribeLogGroups)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeLogGroups_nextToken :: Lens.Lens' DescribeLogGroups (Prelude.Maybe Prelude.Text)
describeLogGroups_nextToken = Lens.lens (\DescribeLogGroups' {nextToken} -> nextToken) (\s@DescribeLogGroups' {} a -> s {nextToken = a} :: DescribeLogGroups)

instance Core.AWSPager DescribeLogGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLogGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLogGroupsResponse_logGroups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeLogGroups_nextToken
          Lens..~ rs
          Lens.^? describeLogGroupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeLogGroups where
  type
    AWSResponse DescribeLogGroups =
      DescribeLogGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLogGroupsResponse'
            Prelude.<$> (x Data..?> "logGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLogGroups where
  hashWithSalt _salt DescribeLogGroups' {..} =
    _salt
      `Prelude.hashWithSalt` accountIdentifiers
      `Prelude.hashWithSalt` includeLinkedAccounts
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` logGroupNamePattern
      `Prelude.hashWithSalt` logGroupNamePrefix
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeLogGroups where
  rnf DescribeLogGroups' {..} =
    Prelude.rnf accountIdentifiers
      `Prelude.seq` Prelude.rnf includeLinkedAccounts
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf logGroupNamePattern
      `Prelude.seq` Prelude.rnf logGroupNamePrefix
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeLogGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DescribeLogGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLogGroups where
  toJSON DescribeLogGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountIdentifiers" Data..=)
              Prelude.<$> accountIdentifiers,
            ("includeLinkedAccounts" Data..=)
              Prelude.<$> includeLinkedAccounts,
            ("limit" Data..=) Prelude.<$> limit,
            ("logGroupNamePattern" Data..=)
              Prelude.<$> logGroupNamePattern,
            ("logGroupNamePrefix" Data..=)
              Prelude.<$> logGroupNamePrefix,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeLogGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLogGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLogGroupsResponse' smart constructor.
data DescribeLogGroupsResponse = DescribeLogGroupsResponse'
  { -- | The log groups.
    --
    -- If the @retentionInDays@ value is not included for a log group, then
    -- that log group\'s events do not expire.
    logGroups :: Prelude.Maybe [LogGroup],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLogGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroups', 'describeLogGroupsResponse_logGroups' - The log groups.
--
-- If the @retentionInDays@ value is not included for a log group, then
-- that log group\'s events do not expire.
--
-- 'nextToken', 'describeLogGroupsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeLogGroupsResponse_httpStatus' - The response's http status code.
newDescribeLogGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLogGroupsResponse
newDescribeLogGroupsResponse pHttpStatus_ =
  DescribeLogGroupsResponse'
    { logGroups =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The log groups.
--
-- If the @retentionInDays@ value is not included for a log group, then
-- that log group\'s events do not expire.
describeLogGroupsResponse_logGroups :: Lens.Lens' DescribeLogGroupsResponse (Prelude.Maybe [LogGroup])
describeLogGroupsResponse_logGroups = Lens.lens (\DescribeLogGroupsResponse' {logGroups} -> logGroups) (\s@DescribeLogGroupsResponse' {} a -> s {logGroups = a} :: DescribeLogGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeLogGroupsResponse_nextToken :: Lens.Lens' DescribeLogGroupsResponse (Prelude.Maybe Prelude.Text)
describeLogGroupsResponse_nextToken = Lens.lens (\DescribeLogGroupsResponse' {nextToken} -> nextToken) (\s@DescribeLogGroupsResponse' {} a -> s {nextToken = a} :: DescribeLogGroupsResponse)

-- | The response's http status code.
describeLogGroupsResponse_httpStatus :: Lens.Lens' DescribeLogGroupsResponse Prelude.Int
describeLogGroupsResponse_httpStatus = Lens.lens (\DescribeLogGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeLogGroupsResponse' {} a -> s {httpStatus = a} :: DescribeLogGroupsResponse)

instance Prelude.NFData DescribeLogGroupsResponse where
  rnf DescribeLogGroupsResponse' {..} =
    Prelude.rnf logGroups
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
