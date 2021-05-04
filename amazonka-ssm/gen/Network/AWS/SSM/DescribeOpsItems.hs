{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeOpsItems' smart constructor.
data DescribeOpsItems = DescribeOpsItems'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
    opsItemFilters :: Prelude.Maybe [OpsItemFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      opsItemFilters = Prelude.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeOpsItems_nextToken :: Lens.Lens' DescribeOpsItems (Prelude.Maybe Prelude.Text)
describeOpsItems_nextToken = Lens.lens (\DescribeOpsItems' {nextToken} -> nextToken) (\s@DescribeOpsItems' {} a -> s {nextToken = a} :: DescribeOpsItems)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeOpsItems_maxResults :: Lens.Lens' DescribeOpsItems (Prelude.Maybe Prelude.Natural)
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
describeOpsItems_opsItemFilters :: Lens.Lens' DescribeOpsItems (Prelude.Maybe [OpsItemFilter])
describeOpsItems_opsItemFilters = Lens.lens (\DescribeOpsItems' {opsItemFilters} -> opsItemFilters) (\s@DescribeOpsItems' {} a -> s {opsItemFilters = a} :: DescribeOpsItems) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager DescribeOpsItems where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeOpsItemsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeOpsItemsResponse_opsItemSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeOpsItems_nextToken
          Lens..~ rs
          Lens.^? describeOpsItemsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeOpsItems where
  type Rs DescribeOpsItems = DescribeOpsItemsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOpsItemsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "OpsItemSummaries"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOpsItems

instance Prelude.NFData DescribeOpsItems

instance Prelude.ToHeaders DescribeOpsItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AmazonSSM.DescribeOpsItems" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeOpsItems where
  toJSON DescribeOpsItems' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("OpsItemFilters" Prelude..=)
              Prelude.<$> opsItemFilters
          ]
      )

instance Prelude.ToPath DescribeOpsItems where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeOpsItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOpsItemsResponse' smart constructor.
data DescribeOpsItemsResponse = DescribeOpsItemsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of OpsItems.
    opsItemSummaries :: Prelude.Maybe [OpsItemSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeOpsItemsResponse
newDescribeOpsItemsResponse pHttpStatus_ =
  DescribeOpsItemsResponse'
    { nextToken =
        Prelude.Nothing,
      opsItemSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeOpsItemsResponse_nextToken :: Lens.Lens' DescribeOpsItemsResponse (Prelude.Maybe Prelude.Text)
describeOpsItemsResponse_nextToken = Lens.lens (\DescribeOpsItemsResponse' {nextToken} -> nextToken) (\s@DescribeOpsItemsResponse' {} a -> s {nextToken = a} :: DescribeOpsItemsResponse)

-- | A list of OpsItems.
describeOpsItemsResponse_opsItemSummaries :: Lens.Lens' DescribeOpsItemsResponse (Prelude.Maybe [OpsItemSummary])
describeOpsItemsResponse_opsItemSummaries = Lens.lens (\DescribeOpsItemsResponse' {opsItemSummaries} -> opsItemSummaries) (\s@DescribeOpsItemsResponse' {} a -> s {opsItemSummaries = a} :: DescribeOpsItemsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeOpsItemsResponse_httpStatus :: Lens.Lens' DescribeOpsItemsResponse Prelude.Int
describeOpsItemsResponse_httpStatus = Lens.lens (\DescribeOpsItemsResponse' {httpStatus} -> httpStatus) (\s@DescribeOpsItemsResponse' {} a -> s {httpStatus = a} :: DescribeOpsItemsResponse)

instance Prelude.NFData DescribeOpsItemsResponse
