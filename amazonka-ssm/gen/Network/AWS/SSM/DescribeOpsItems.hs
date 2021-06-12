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
-- Module      : Network.AWS.SSM.DescribeOpsItems
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Query a set of OpsItems. You must have permission in AWS Identity and
-- Access Management (IAM) to query a list of OpsItems. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter>
-- in the /AWS Systems Manager User Guide/.
--
-- Operations engineers and IT professionals use OpsCenter to view,
-- investigate, and remediate operational issues impacting the performance
-- and health of their AWS resources. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter>
-- in the /AWS Systems Manager User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeOpsItems
  ( -- * Creating a Request
    DescribeOpsItems (..),
    newDescribeOpsItems,

    -- * Request Lenses
    describeOpsItems_nextToken,
    describeOpsItems_maxResults,
    describeOpsItems_opsItemFilters,

    -- * Destructuring the Response
    DescribeOpsItemsResponse (..),
    newDescribeOpsItemsResponse,

    -- * Response Lenses
    describeOpsItemsResponse_nextToken,
    describeOpsItemsResponse_opsItemSummaries,
    describeOpsItemsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeOpsItems' smart constructor.
data DescribeOpsItems = DescribeOpsItems'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters to limit the response.
    --
    -- -   Key: CreatedTime
    --
    --     Operations: GreaterThan, LessThan
    --
    -- -   Key: LastModifiedBy
    --
    --     Operations: Contains, Equals
    --
    -- -   Key: LastModifiedTime
    --
    --     Operations: GreaterThan, LessThan
    --
    -- -   Key: Priority
    --
    --     Operations: Equals
    --
    -- -   Key: Source
    --
    --     Operations: Contains, Equals
    --
    -- -   Key: Status
    --
    --     Operations: Equals
    --
    -- -   Key: Title
    --
    --     Operations: Contains
    --
    -- -   Key: OperationalData*
    --
    --     Operations: Equals
    --
    -- -   Key: OperationalDataKey
    --
    --     Operations: Equals
    --
    -- -   Key: OperationalDataValue
    --
    --     Operations: Equals, Contains
    --
    -- -   Key: OpsItemId
    --
    --     Operations: Equals
    --
    -- -   Key: ResourceId
    --
    --     Operations: Contains
    --
    -- -   Key: AutomationId
    --
    --     Operations: Equals
    --
    -- *If you filter the response by using the OperationalData operator,
    -- specify a key-value pair by using the following JSON format:
    -- {\"key\":\"key_name\",\"value\":\"a_value\"}
    opsItemFilters :: Core.Maybe [OpsItemFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOpsItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeOpsItems_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'describeOpsItems_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'opsItemFilters', 'describeOpsItems_opsItemFilters' - One or more filters to limit the response.
--
-- -   Key: CreatedTime
--
--     Operations: GreaterThan, LessThan
--
-- -   Key: LastModifiedBy
--
--     Operations: Contains, Equals
--
-- -   Key: LastModifiedTime
--
--     Operations: GreaterThan, LessThan
--
-- -   Key: Priority
--
--     Operations: Equals
--
-- -   Key: Source
--
--     Operations: Contains, Equals
--
-- -   Key: Status
--
--     Operations: Equals
--
-- -   Key: Title
--
--     Operations: Contains
--
-- -   Key: OperationalData*
--
--     Operations: Equals
--
-- -   Key: OperationalDataKey
--
--     Operations: Equals
--
-- -   Key: OperationalDataValue
--
--     Operations: Equals, Contains
--
-- -   Key: OpsItemId
--
--     Operations: Equals
--
-- -   Key: ResourceId
--
--     Operations: Contains
--
-- -   Key: AutomationId
--
--     Operations: Equals
--
-- *If you filter the response by using the OperationalData operator,
-- specify a key-value pair by using the following JSON format:
-- {\"key\":\"key_name\",\"value\":\"a_value\"}
newDescribeOpsItems ::
  DescribeOpsItems
newDescribeOpsItems =
  DescribeOpsItems'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      opsItemFilters = Core.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeOpsItems_nextToken :: Lens.Lens' DescribeOpsItems (Core.Maybe Core.Text)
describeOpsItems_nextToken = Lens.lens (\DescribeOpsItems' {nextToken} -> nextToken) (\s@DescribeOpsItems' {} a -> s {nextToken = a} :: DescribeOpsItems)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeOpsItems_maxResults :: Lens.Lens' DescribeOpsItems (Core.Maybe Core.Natural)
describeOpsItems_maxResults = Lens.lens (\DescribeOpsItems' {maxResults} -> maxResults) (\s@DescribeOpsItems' {} a -> s {maxResults = a} :: DescribeOpsItems)

-- | One or more filters to limit the response.
--
-- -   Key: CreatedTime
--
--     Operations: GreaterThan, LessThan
--
-- -   Key: LastModifiedBy
--
--     Operations: Contains, Equals
--
-- -   Key: LastModifiedTime
--
--     Operations: GreaterThan, LessThan
--
-- -   Key: Priority
--
--     Operations: Equals
--
-- -   Key: Source
--
--     Operations: Contains, Equals
--
-- -   Key: Status
--
--     Operations: Equals
--
-- -   Key: Title
--
--     Operations: Contains
--
-- -   Key: OperationalData*
--
--     Operations: Equals
--
-- -   Key: OperationalDataKey
--
--     Operations: Equals
--
-- -   Key: OperationalDataValue
--
--     Operations: Equals, Contains
--
-- -   Key: OpsItemId
--
--     Operations: Equals
--
-- -   Key: ResourceId
--
--     Operations: Contains
--
-- -   Key: AutomationId
--
--     Operations: Equals
--
-- *If you filter the response by using the OperationalData operator,
-- specify a key-value pair by using the following JSON format:
-- {\"key\":\"key_name\",\"value\":\"a_value\"}
describeOpsItems_opsItemFilters :: Lens.Lens' DescribeOpsItems (Core.Maybe [OpsItemFilter])
describeOpsItems_opsItemFilters = Lens.lens (\DescribeOpsItems' {opsItemFilters} -> opsItemFilters) (\s@DescribeOpsItems' {} a -> s {opsItemFilters = a} :: DescribeOpsItems) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeOpsItems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOpsItemsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOpsItemsResponse_opsItemSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeOpsItems_nextToken
          Lens..~ rs
          Lens.^? describeOpsItemsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeOpsItems where
  type
    AWSResponse DescribeOpsItems =
      DescribeOpsItemsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOpsItemsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "OpsItemSummaries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeOpsItems

instance Core.NFData DescribeOpsItems

instance Core.ToHeaders DescribeOpsItems where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.DescribeOpsItems" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeOpsItems where
  toJSON DescribeOpsItems' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("OpsItemFilters" Core..=) Core.<$> opsItemFilters
          ]
      )

instance Core.ToPath DescribeOpsItems where
  toPath = Core.const "/"

instance Core.ToQuery DescribeOpsItems where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeOpsItemsResponse' smart constructor.
data DescribeOpsItemsResponse = DescribeOpsItemsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of OpsItems.
    opsItemSummaries :: Core.Maybe [OpsItemSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOpsItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeOpsItemsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'opsItemSummaries', 'describeOpsItemsResponse_opsItemSummaries' - A list of OpsItems.
--
-- 'httpStatus', 'describeOpsItemsResponse_httpStatus' - The response's http status code.
newDescribeOpsItemsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeOpsItemsResponse
newDescribeOpsItemsResponse pHttpStatus_ =
  DescribeOpsItemsResponse'
    { nextToken = Core.Nothing,
      opsItemSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeOpsItemsResponse_nextToken :: Lens.Lens' DescribeOpsItemsResponse (Core.Maybe Core.Text)
describeOpsItemsResponse_nextToken = Lens.lens (\DescribeOpsItemsResponse' {nextToken} -> nextToken) (\s@DescribeOpsItemsResponse' {} a -> s {nextToken = a} :: DescribeOpsItemsResponse)

-- | A list of OpsItems.
describeOpsItemsResponse_opsItemSummaries :: Lens.Lens' DescribeOpsItemsResponse (Core.Maybe [OpsItemSummary])
describeOpsItemsResponse_opsItemSummaries = Lens.lens (\DescribeOpsItemsResponse' {opsItemSummaries} -> opsItemSummaries) (\s@DescribeOpsItemsResponse' {} a -> s {opsItemSummaries = a} :: DescribeOpsItemsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeOpsItemsResponse_httpStatus :: Lens.Lens' DescribeOpsItemsResponse Core.Int
describeOpsItemsResponse_httpStatus = Lens.lens (\DescribeOpsItemsResponse' {httpStatus} -> httpStatus) (\s@DescribeOpsItemsResponse' {} a -> s {httpStatus = a} :: DescribeOpsItemsResponse)

instance Core.NFData DescribeOpsItemsResponse
