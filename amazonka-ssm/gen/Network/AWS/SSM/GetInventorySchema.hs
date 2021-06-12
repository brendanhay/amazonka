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
-- Module      : Network.AWS.SSM.GetInventorySchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a list of inventory type names for the account, or return a list
-- of attribute names for a specific Inventory item type.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetInventorySchema
  ( -- * Creating a Request
    GetInventorySchema (..),
    newGetInventorySchema,

    -- * Request Lenses
    getInventorySchema_typeName,
    getInventorySchema_nextToken,
    getInventorySchema_subType,
    getInventorySchema_aggregator,
    getInventorySchema_maxResults,

    -- * Destructuring the Response
    GetInventorySchemaResponse (..),
    newGetInventorySchemaResponse,

    -- * Response Lenses
    getInventorySchemaResponse_nextToken,
    getInventorySchemaResponse_schemas,
    getInventorySchemaResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetInventorySchema' smart constructor.
data GetInventorySchema = GetInventorySchema'
  { -- | The type of inventory item to return.
    typeName :: Core.Maybe Core.Text,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | Returns the sub-type schema for a specified inventory type.
    subType :: Core.Maybe Core.Bool,
    -- | Returns inventory schemas that support aggregation. For example, this
    -- call returns the @AWS:InstanceInformation@ type, because it supports
    -- aggregation based on the @PlatformName@, @PlatformType@, and
    -- @PlatformVersion@ attributes.
    aggregator :: Core.Maybe Core.Bool,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInventorySchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'getInventorySchema_typeName' - The type of inventory item to return.
--
-- 'nextToken', 'getInventorySchema_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'subType', 'getInventorySchema_subType' - Returns the sub-type schema for a specified inventory type.
--
-- 'aggregator', 'getInventorySchema_aggregator' - Returns inventory schemas that support aggregation. For example, this
-- call returns the @AWS:InstanceInformation@ type, because it supports
-- aggregation based on the @PlatformName@, @PlatformType@, and
-- @PlatformVersion@ attributes.
--
-- 'maxResults', 'getInventorySchema_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
newGetInventorySchema ::
  GetInventorySchema
newGetInventorySchema =
  GetInventorySchema'
    { typeName = Core.Nothing,
      nextToken = Core.Nothing,
      subType = Core.Nothing,
      aggregator = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The type of inventory item to return.
getInventorySchema_typeName :: Lens.Lens' GetInventorySchema (Core.Maybe Core.Text)
getInventorySchema_typeName = Lens.lens (\GetInventorySchema' {typeName} -> typeName) (\s@GetInventorySchema' {} a -> s {typeName = a} :: GetInventorySchema)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
getInventorySchema_nextToken :: Lens.Lens' GetInventorySchema (Core.Maybe Core.Text)
getInventorySchema_nextToken = Lens.lens (\GetInventorySchema' {nextToken} -> nextToken) (\s@GetInventorySchema' {} a -> s {nextToken = a} :: GetInventorySchema)

-- | Returns the sub-type schema for a specified inventory type.
getInventorySchema_subType :: Lens.Lens' GetInventorySchema (Core.Maybe Core.Bool)
getInventorySchema_subType = Lens.lens (\GetInventorySchema' {subType} -> subType) (\s@GetInventorySchema' {} a -> s {subType = a} :: GetInventorySchema)

-- | Returns inventory schemas that support aggregation. For example, this
-- call returns the @AWS:InstanceInformation@ type, because it supports
-- aggregation based on the @PlatformName@, @PlatformType@, and
-- @PlatformVersion@ attributes.
getInventorySchema_aggregator :: Lens.Lens' GetInventorySchema (Core.Maybe Core.Bool)
getInventorySchema_aggregator = Lens.lens (\GetInventorySchema' {aggregator} -> aggregator) (\s@GetInventorySchema' {} a -> s {aggregator = a} :: GetInventorySchema)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getInventorySchema_maxResults :: Lens.Lens' GetInventorySchema (Core.Maybe Core.Natural)
getInventorySchema_maxResults = Lens.lens (\GetInventorySchema' {maxResults} -> maxResults) (\s@GetInventorySchema' {} a -> s {maxResults = a} :: GetInventorySchema)

instance Core.AWSPager GetInventorySchema where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getInventorySchemaResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getInventorySchemaResponse_schemas Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getInventorySchema_nextToken
          Lens..~ rs
          Lens.^? getInventorySchemaResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetInventorySchema where
  type
    AWSResponse GetInventorySchema =
      GetInventorySchemaResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInventorySchemaResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Schemas" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetInventorySchema

instance Core.NFData GetInventorySchema

instance Core.ToHeaders GetInventorySchema where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetInventorySchema" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetInventorySchema where
  toJSON GetInventorySchema' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TypeName" Core..=) Core.<$> typeName,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SubType" Core..=) Core.<$> subType,
            ("Aggregator" Core..=) Core.<$> aggregator,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath GetInventorySchema where
  toPath = Core.const "/"

instance Core.ToQuery GetInventorySchema where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetInventorySchemaResponse' smart constructor.
data GetInventorySchemaResponse = GetInventorySchemaResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Inventory schemas returned by the request.
    schemas :: Core.Maybe [InventoryItemSchema],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInventorySchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInventorySchemaResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'schemas', 'getInventorySchemaResponse_schemas' - Inventory schemas returned by the request.
--
-- 'httpStatus', 'getInventorySchemaResponse_httpStatus' - The response's http status code.
newGetInventorySchemaResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetInventorySchemaResponse
newGetInventorySchemaResponse pHttpStatus_ =
  GetInventorySchemaResponse'
    { nextToken =
        Core.Nothing,
      schemas = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
getInventorySchemaResponse_nextToken :: Lens.Lens' GetInventorySchemaResponse (Core.Maybe Core.Text)
getInventorySchemaResponse_nextToken = Lens.lens (\GetInventorySchemaResponse' {nextToken} -> nextToken) (\s@GetInventorySchemaResponse' {} a -> s {nextToken = a} :: GetInventorySchemaResponse)

-- | Inventory schemas returned by the request.
getInventorySchemaResponse_schemas :: Lens.Lens' GetInventorySchemaResponse (Core.Maybe [InventoryItemSchema])
getInventorySchemaResponse_schemas = Lens.lens (\GetInventorySchemaResponse' {schemas} -> schemas) (\s@GetInventorySchemaResponse' {} a -> s {schemas = a} :: GetInventorySchemaResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getInventorySchemaResponse_httpStatus :: Lens.Lens' GetInventorySchemaResponse Core.Int
getInventorySchemaResponse_httpStatus = Lens.lens (\GetInventorySchemaResponse' {httpStatus} -> httpStatus) (\s@GetInventorySchemaResponse' {} a -> s {httpStatus = a} :: GetInventorySchemaResponse)

instance Core.NFData GetInventorySchemaResponse
