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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeInventoryDeletions' smart constructor.
data DescribeInventoryDeletions = DescribeInventoryDeletions'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the delete inventory ID for which you want information. This ID
    -- was returned by the @DeleteInventory@ action.
    deletionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      deletionId = Prelude.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeInventoryDeletions_nextToken :: Lens.Lens' DescribeInventoryDeletions (Prelude.Maybe Prelude.Text)
describeInventoryDeletions_nextToken = Lens.lens (\DescribeInventoryDeletions' {nextToken} -> nextToken) (\s@DescribeInventoryDeletions' {} a -> s {nextToken = a} :: DescribeInventoryDeletions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeInventoryDeletions_maxResults :: Lens.Lens' DescribeInventoryDeletions (Prelude.Maybe Prelude.Natural)
describeInventoryDeletions_maxResults = Lens.lens (\DescribeInventoryDeletions' {maxResults} -> maxResults) (\s@DescribeInventoryDeletions' {} a -> s {maxResults = a} :: DescribeInventoryDeletions)

-- | Specify the delete inventory ID for which you want information. This ID
-- was returned by the @DeleteInventory@ action.
describeInventoryDeletions_deletionId :: Lens.Lens' DescribeInventoryDeletions (Prelude.Maybe Prelude.Text)
describeInventoryDeletions_deletionId = Lens.lens (\DescribeInventoryDeletions' {deletionId} -> deletionId) (\s@DescribeInventoryDeletions' {} a -> s {deletionId = a} :: DescribeInventoryDeletions)

instance Pager.AWSPager DescribeInventoryDeletions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeInventoryDeletionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeInventoryDeletionsResponse_inventoryDeletions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeInventoryDeletions_nextToken
          Lens..~ rs
          Lens.^? describeInventoryDeletionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeInventoryDeletions
  where
  type
    Rs DescribeInventoryDeletions =
      DescribeInventoryDeletionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInventoryDeletionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "InventoryDeletions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInventoryDeletions

instance Prelude.NFData DescribeInventoryDeletions

instance Prelude.ToHeaders DescribeInventoryDeletions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.DescribeInventoryDeletions" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeInventoryDeletions where
  toJSON DescribeInventoryDeletions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("DeletionId" Prelude..=) Prelude.<$> deletionId
          ]
      )

instance Prelude.ToPath DescribeInventoryDeletions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeInventoryDeletions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInventoryDeletionsResponse' smart constructor.
data DescribeInventoryDeletionsResponse = DescribeInventoryDeletionsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of status items for deleted inventory.
    inventoryDeletions :: Prelude.Maybe [InventoryDeletionStatusItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeInventoryDeletionsResponse
newDescribeInventoryDeletionsResponse pHttpStatus_ =
  DescribeInventoryDeletionsResponse'
    { nextToken =
        Prelude.Nothing,
      inventoryDeletions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeInventoryDeletionsResponse_nextToken :: Lens.Lens' DescribeInventoryDeletionsResponse (Prelude.Maybe Prelude.Text)
describeInventoryDeletionsResponse_nextToken = Lens.lens (\DescribeInventoryDeletionsResponse' {nextToken} -> nextToken) (\s@DescribeInventoryDeletionsResponse' {} a -> s {nextToken = a} :: DescribeInventoryDeletionsResponse)

-- | A list of status items for deleted inventory.
describeInventoryDeletionsResponse_inventoryDeletions :: Lens.Lens' DescribeInventoryDeletionsResponse (Prelude.Maybe [InventoryDeletionStatusItem])
describeInventoryDeletionsResponse_inventoryDeletions = Lens.lens (\DescribeInventoryDeletionsResponse' {inventoryDeletions} -> inventoryDeletions) (\s@DescribeInventoryDeletionsResponse' {} a -> s {inventoryDeletions = a} :: DescribeInventoryDeletionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeInventoryDeletionsResponse_httpStatus :: Lens.Lens' DescribeInventoryDeletionsResponse Prelude.Int
describeInventoryDeletionsResponse_httpStatus = Lens.lens (\DescribeInventoryDeletionsResponse' {httpStatus} -> httpStatus) (\s@DescribeInventoryDeletionsResponse' {} a -> s {httpStatus = a} :: DescribeInventoryDeletionsResponse)

instance
  Prelude.NFData
    DescribeInventoryDeletionsResponse
