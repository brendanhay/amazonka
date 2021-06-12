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
-- Module      : Network.AWS.SSM.DescribeInventoryDeletions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specific delete inventory operation.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInventoryDeletions
  ( -- * Creating a Request
    DescribeInventoryDeletions (..),
    newDescribeInventoryDeletions,

    -- * Request Lenses
    describeInventoryDeletions_nextToken,
    describeInventoryDeletions_maxResults,
    describeInventoryDeletions_deletionId,

    -- * Destructuring the Response
    DescribeInventoryDeletionsResponse (..),
    newDescribeInventoryDeletionsResponse,

    -- * Response Lenses
    describeInventoryDeletionsResponse_nextToken,
    describeInventoryDeletionsResponse_inventoryDeletions,
    describeInventoryDeletionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeInventoryDeletions' smart constructor.
data DescribeInventoryDeletions = DescribeInventoryDeletions'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specify the delete inventory ID for which you want information. This ID
    -- was returned by the @DeleteInventory@ action.
    deletionId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInventoryDeletions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInventoryDeletions_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'describeInventoryDeletions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'deletionId', 'describeInventoryDeletions_deletionId' - Specify the delete inventory ID for which you want information. This ID
-- was returned by the @DeleteInventory@ action.
newDescribeInventoryDeletions ::
  DescribeInventoryDeletions
newDescribeInventoryDeletions =
  DescribeInventoryDeletions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      deletionId = Core.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeInventoryDeletions_nextToken :: Lens.Lens' DescribeInventoryDeletions (Core.Maybe Core.Text)
describeInventoryDeletions_nextToken = Lens.lens (\DescribeInventoryDeletions' {nextToken} -> nextToken) (\s@DescribeInventoryDeletions' {} a -> s {nextToken = a} :: DescribeInventoryDeletions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeInventoryDeletions_maxResults :: Lens.Lens' DescribeInventoryDeletions (Core.Maybe Core.Natural)
describeInventoryDeletions_maxResults = Lens.lens (\DescribeInventoryDeletions' {maxResults} -> maxResults) (\s@DescribeInventoryDeletions' {} a -> s {maxResults = a} :: DescribeInventoryDeletions)

-- | Specify the delete inventory ID for which you want information. This ID
-- was returned by the @DeleteInventory@ action.
describeInventoryDeletions_deletionId :: Lens.Lens' DescribeInventoryDeletions (Core.Maybe Core.Text)
describeInventoryDeletions_deletionId = Lens.lens (\DescribeInventoryDeletions' {deletionId} -> deletionId) (\s@DescribeInventoryDeletions' {} a -> s {deletionId = a} :: DescribeInventoryDeletions)

instance Core.AWSPager DescribeInventoryDeletions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInventoryDeletionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInventoryDeletionsResponse_inventoryDeletions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeInventoryDeletions_nextToken
          Lens..~ rs
          Lens.^? describeInventoryDeletionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeInventoryDeletions where
  type
    AWSResponse DescribeInventoryDeletions =
      DescribeInventoryDeletionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInventoryDeletionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "InventoryDeletions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInventoryDeletions

instance Core.NFData DescribeInventoryDeletions

instance Core.ToHeaders DescribeInventoryDeletions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeInventoryDeletions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeInventoryDeletions where
  toJSON DescribeInventoryDeletions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("DeletionId" Core..=) Core.<$> deletionId
          ]
      )

instance Core.ToPath DescribeInventoryDeletions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeInventoryDeletions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeInventoryDeletionsResponse' smart constructor.
data DescribeInventoryDeletionsResponse = DescribeInventoryDeletionsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of status items for deleted inventory.
    inventoryDeletions :: Core.Maybe [InventoryDeletionStatusItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInventoryDeletionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInventoryDeletionsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'inventoryDeletions', 'describeInventoryDeletionsResponse_inventoryDeletions' - A list of status items for deleted inventory.
--
-- 'httpStatus', 'describeInventoryDeletionsResponse_httpStatus' - The response's http status code.
newDescribeInventoryDeletionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInventoryDeletionsResponse
newDescribeInventoryDeletionsResponse pHttpStatus_ =
  DescribeInventoryDeletionsResponse'
    { nextToken =
        Core.Nothing,
      inventoryDeletions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeInventoryDeletionsResponse_nextToken :: Lens.Lens' DescribeInventoryDeletionsResponse (Core.Maybe Core.Text)
describeInventoryDeletionsResponse_nextToken = Lens.lens (\DescribeInventoryDeletionsResponse' {nextToken} -> nextToken) (\s@DescribeInventoryDeletionsResponse' {} a -> s {nextToken = a} :: DescribeInventoryDeletionsResponse)

-- | A list of status items for deleted inventory.
describeInventoryDeletionsResponse_inventoryDeletions :: Lens.Lens' DescribeInventoryDeletionsResponse (Core.Maybe [InventoryDeletionStatusItem])
describeInventoryDeletionsResponse_inventoryDeletions = Lens.lens (\DescribeInventoryDeletionsResponse' {inventoryDeletions} -> inventoryDeletions) (\s@DescribeInventoryDeletionsResponse' {} a -> s {inventoryDeletions = a} :: DescribeInventoryDeletionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInventoryDeletionsResponse_httpStatus :: Lens.Lens' DescribeInventoryDeletionsResponse Core.Int
describeInventoryDeletionsResponse_httpStatus = Lens.lens (\DescribeInventoryDeletionsResponse' {httpStatus} -> httpStatus) (\s@DescribeInventoryDeletionsResponse' {} a -> s {httpStatus = a} :: DescribeInventoryDeletionsResponse)

instance
  Core.NFData
    DescribeInventoryDeletionsResponse
