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
-- Module      : Amazonka.SSM.DescribeOpsItems
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Query a set of OpsItems. You must have permission in Identity and Access
-- Management (IAM) to query a list of OpsItems. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- Operations engineers and IT professionals use Amazon Web Services
-- Systems Manager OpsCenter to view, investigate, and remediate
-- operational issues impacting the performance and health of their Amazon
-- Web Services resources. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html OpsCenter>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeOpsItems
  ( -- * Creating a Request
    DescribeOpsItems (..),
    newDescribeOpsItems,

    -- * Request Lenses
    describeOpsItems_nextToken,
    describeOpsItems_opsItemFilters,
    describeOpsItems_maxResults,

    -- * Destructuring the Response
    DescribeOpsItemsResponse (..),
    newDescribeOpsItemsResponse,

    -- * Response Lenses
    describeOpsItemsResponse_opsItemSummaries,
    describeOpsItemsResponse_nextToken,
    describeOpsItemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeOpsItems' smart constructor.
data DescribeOpsItems = DescribeOpsItems'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    -- -   Key: Title*
    --
    --     Operations: Equals,Contains
    --
    -- -   Key: OperationalData**
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
    -- *The Equals operator for Title matches the first 100 characters. If you
    -- specify more than 100 characters, they system returns an error that the
    -- filter value exceeds the length limit.
    --
    -- **If you filter the response by using the OperationalData operator,
    -- specify a key-value pair by using the following JSON format:
    -- {\"key\":\"key_name\",\"value\":\"a_value\"}
    opsItemFilters :: Prelude.Maybe [OpsItemFilter],
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- -   Key: Title*
--
--     Operations: Equals,Contains
--
-- -   Key: OperationalData**
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
-- *The Equals operator for Title matches the first 100 characters. If you
-- specify more than 100 characters, they system returns an error that the
-- filter value exceeds the length limit.
--
-- **If you filter the response by using the OperationalData operator,
-- specify a key-value pair by using the following JSON format:
-- {\"key\":\"key_name\",\"value\":\"a_value\"}
--
-- 'maxResults', 'describeOpsItems_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
newDescribeOpsItems ::
  DescribeOpsItems
newDescribeOpsItems =
  DescribeOpsItems'
    { nextToken = Prelude.Nothing,
      opsItemFilters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeOpsItems_nextToken :: Lens.Lens' DescribeOpsItems (Prelude.Maybe Prelude.Text)
describeOpsItems_nextToken = Lens.lens (\DescribeOpsItems' {nextToken} -> nextToken) (\s@DescribeOpsItems' {} a -> s {nextToken = a} :: DescribeOpsItems)

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
-- -   Key: Title*
--
--     Operations: Equals,Contains
--
-- -   Key: OperationalData**
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
-- *The Equals operator for Title matches the first 100 characters. If you
-- specify more than 100 characters, they system returns an error that the
-- filter value exceeds the length limit.
--
-- **If you filter the response by using the OperationalData operator,
-- specify a key-value pair by using the following JSON format:
-- {\"key\":\"key_name\",\"value\":\"a_value\"}
describeOpsItems_opsItemFilters :: Lens.Lens' DescribeOpsItems (Prelude.Maybe [OpsItemFilter])
describeOpsItems_opsItemFilters = Lens.lens (\DescribeOpsItems' {opsItemFilters} -> opsItemFilters) (\s@DescribeOpsItems' {} a -> s {opsItemFilters = a} :: DescribeOpsItems) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeOpsItems_maxResults :: Lens.Lens' DescribeOpsItems (Prelude.Maybe Prelude.Natural)
describeOpsItems_maxResults = Lens.lens (\DescribeOpsItems' {maxResults} -> maxResults) (\s@DescribeOpsItems' {} a -> s {maxResults = a} :: DescribeOpsItems)

instance Core.AWSPager DescribeOpsItems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOpsItemsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOpsItemsResponse_opsItemSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeOpsItems_nextToken
          Lens..~ rs
          Lens.^? describeOpsItemsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeOpsItems where
  type
    AWSResponse DescribeOpsItems =
      DescribeOpsItemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOpsItemsResponse'
            Prelude.<$> ( x Data..?> "OpsItemSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOpsItems where
  hashWithSalt _salt DescribeOpsItems' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` opsItemFilters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeOpsItems where
  rnf DescribeOpsItems' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf opsItemFilters
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeOpsItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.DescribeOpsItems" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeOpsItems where
  toJSON DescribeOpsItems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("OpsItemFilters" Data..=)
              Prelude.<$> opsItemFilters,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribeOpsItems where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeOpsItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOpsItemsResponse' smart constructor.
data DescribeOpsItemsResponse = DescribeOpsItemsResponse'
  { -- | A list of OpsItems.
    opsItemSummaries :: Prelude.Maybe [OpsItemSummary],
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOpsItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsItemSummaries', 'describeOpsItemsResponse_opsItemSummaries' - A list of OpsItems.
--
-- 'nextToken', 'describeOpsItemsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'describeOpsItemsResponse_httpStatus' - The response's http status code.
newDescribeOpsItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOpsItemsResponse
newDescribeOpsItemsResponse pHttpStatus_ =
  DescribeOpsItemsResponse'
    { opsItemSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of OpsItems.
describeOpsItemsResponse_opsItemSummaries :: Lens.Lens' DescribeOpsItemsResponse (Prelude.Maybe [OpsItemSummary])
describeOpsItemsResponse_opsItemSummaries = Lens.lens (\DescribeOpsItemsResponse' {opsItemSummaries} -> opsItemSummaries) (\s@DescribeOpsItemsResponse' {} a -> s {opsItemSummaries = a} :: DescribeOpsItemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeOpsItemsResponse_nextToken :: Lens.Lens' DescribeOpsItemsResponse (Prelude.Maybe Prelude.Text)
describeOpsItemsResponse_nextToken = Lens.lens (\DescribeOpsItemsResponse' {nextToken} -> nextToken) (\s@DescribeOpsItemsResponse' {} a -> s {nextToken = a} :: DescribeOpsItemsResponse)

-- | The response's http status code.
describeOpsItemsResponse_httpStatus :: Lens.Lens' DescribeOpsItemsResponse Prelude.Int
describeOpsItemsResponse_httpStatus = Lens.lens (\DescribeOpsItemsResponse' {httpStatus} -> httpStatus) (\s@DescribeOpsItemsResponse' {} a -> s {httpStatus = a} :: DescribeOpsItemsResponse)

instance Prelude.NFData DescribeOpsItemsResponse where
  rnf DescribeOpsItemsResponse' {..} =
    Prelude.rnf opsItemSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
