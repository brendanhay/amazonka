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
-- Module      : Network.AWS.CloudWatchLogs.DescribeLogGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified log groups. You can list all your log groups or
-- filter the results by prefix. The results are ASCII-sorted by log group
-- name.
--
-- CloudWatch Logs doesn’t support IAM policies that control access to the
-- @DescribeLogGroups@ action by using the @aws:ResourceTag\/key-name @
-- condition key. Other CloudWatch Logs actions do support the use of the
-- @aws:ResourceTag\/key-name @ condition key to control access. For more
-- information about using tags to control access, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeLogGroups
  ( -- * Creating a Request
    DescribeLogGroups (..),
    newDescribeLogGroups,

    -- * Request Lenses
    describeLogGroups_nextToken,
    describeLogGroups_logGroupNamePrefix,
    describeLogGroups_limit,

    -- * Destructuring the Response
    DescribeLogGroupsResponse (..),
    newDescribeLogGroupsResponse,

    -- * Response Lenses
    describeLogGroupsResponse_nextToken,
    describeLogGroupsResponse_logGroups,
    describeLogGroupsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLogGroups' smart constructor.
data DescribeLogGroups = DescribeLogGroups'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The prefix to match.
    logGroupNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default is up to 50 items.
    limit :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'describeLogGroups_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'logGroupNamePrefix', 'describeLogGroups_logGroupNamePrefix' - The prefix to match.
--
-- 'limit', 'describeLogGroups_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
newDescribeLogGroups ::
  DescribeLogGroups
newDescribeLogGroups =
  DescribeLogGroups'
    { nextToken = Prelude.Nothing,
      logGroupNamePrefix = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeLogGroups_nextToken :: Lens.Lens' DescribeLogGroups (Prelude.Maybe Prelude.Text)
describeLogGroups_nextToken = Lens.lens (\DescribeLogGroups' {nextToken} -> nextToken) (\s@DescribeLogGroups' {} a -> s {nextToken = a} :: DescribeLogGroups)

-- | The prefix to match.
describeLogGroups_logGroupNamePrefix :: Lens.Lens' DescribeLogGroups (Prelude.Maybe Prelude.Text)
describeLogGroups_logGroupNamePrefix = Lens.lens (\DescribeLogGroups' {logGroupNamePrefix} -> logGroupNamePrefix) (\s@DescribeLogGroups' {} a -> s {logGroupNamePrefix = a} :: DescribeLogGroups)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
describeLogGroups_limit :: Lens.Lens' DescribeLogGroups (Prelude.Maybe Prelude.Natural)
describeLogGroups_limit = Lens.lens (\DescribeLogGroups' {limit} -> limit) (\s@DescribeLogGroups' {} a -> s {limit = a} :: DescribeLogGroups)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& describeLogGroups_nextToken
          Lens..~ rs
          Lens.^? describeLogGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeLogGroups where
  type
    AWSResponse DescribeLogGroups =
      DescribeLogGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLogGroupsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "logGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLogGroups

instance Prelude.NFData DescribeLogGroups

instance Core.ToHeaders DescribeLogGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DescribeLogGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeLogGroups where
  toJSON DescribeLogGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("logGroupNamePrefix" Core..=)
              Prelude.<$> logGroupNamePrefix,
            ("limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeLogGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLogGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLogGroupsResponse' smart constructor.
data DescribeLogGroupsResponse = DescribeLogGroupsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The log groups.
    --
    -- If the @retentionInDays@ value if not included for a log group, then
    -- that log group is set to have its events never expire.
    logGroups :: Prelude.Maybe [LogGroup],
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
-- 'nextToken', 'describeLogGroupsResponse_nextToken' - Undocumented member.
--
-- 'logGroups', 'describeLogGroupsResponse_logGroups' - The log groups.
--
-- If the @retentionInDays@ value if not included for a log group, then
-- that log group is set to have its events never expire.
--
-- 'httpStatus', 'describeLogGroupsResponse_httpStatus' - The response's http status code.
newDescribeLogGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLogGroupsResponse
newDescribeLogGroupsResponse pHttpStatus_ =
  DescribeLogGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      logGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeLogGroupsResponse_nextToken :: Lens.Lens' DescribeLogGroupsResponse (Prelude.Maybe Prelude.Text)
describeLogGroupsResponse_nextToken = Lens.lens (\DescribeLogGroupsResponse' {nextToken} -> nextToken) (\s@DescribeLogGroupsResponse' {} a -> s {nextToken = a} :: DescribeLogGroupsResponse)

-- | The log groups.
--
-- If the @retentionInDays@ value if not included for a log group, then
-- that log group is set to have its events never expire.
describeLogGroupsResponse_logGroups :: Lens.Lens' DescribeLogGroupsResponse (Prelude.Maybe [LogGroup])
describeLogGroupsResponse_logGroups = Lens.lens (\DescribeLogGroupsResponse' {logGroups} -> logGroups) (\s@DescribeLogGroupsResponse' {} a -> s {logGroups = a} :: DescribeLogGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLogGroupsResponse_httpStatus :: Lens.Lens' DescribeLogGroupsResponse Prelude.Int
describeLogGroupsResponse_httpStatus = Lens.lens (\DescribeLogGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeLogGroupsResponse' {} a -> s {httpStatus = a} :: DescribeLogGroupsResponse)

instance Prelude.NFData DescribeLogGroupsResponse
