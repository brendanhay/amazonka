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
-- Module      : Amazonka.SSM.GetInventorySchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a list of inventory type names for the account, or return a list
-- of attribute names for a specific Inventory item type.
--
-- This operation returns paginated results.
module Amazonka.SSM.GetInventorySchema
  ( -- * Creating a Request
    GetInventorySchema (..),
    newGetInventorySchema,

    -- * Request Lenses
    getInventorySchema_nextToken,
    getInventorySchema_typeName,
    getInventorySchema_aggregator,
    getInventorySchema_maxResults,
    getInventorySchema_subType,

    -- * Destructuring the Response
    GetInventorySchemaResponse (..),
    newGetInventorySchemaResponse,

    -- * Response Lenses
    getInventorySchemaResponse_nextToken,
    getInventorySchemaResponse_schemas,
    getInventorySchemaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetInventorySchema' smart constructor.
data GetInventorySchema = GetInventorySchema'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of inventory item to return.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | Returns inventory schemas that support aggregation. For example, this
    -- call returns the @AWS:InstanceInformation@ type, because it supports
    -- aggregation based on the @PlatformName@, @PlatformType@, and
    -- @PlatformVersion@ attributes.
    aggregator :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Returns the sub-type schema for a specified inventory type.
    subType :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInventorySchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInventorySchema_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'typeName', 'getInventorySchema_typeName' - The type of inventory item to return.
--
-- 'aggregator', 'getInventorySchema_aggregator' - Returns inventory schemas that support aggregation. For example, this
-- call returns the @AWS:InstanceInformation@ type, because it supports
-- aggregation based on the @PlatformName@, @PlatformType@, and
-- @PlatformVersion@ attributes.
--
-- 'maxResults', 'getInventorySchema_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'subType', 'getInventorySchema_subType' - Returns the sub-type schema for a specified inventory type.
newGetInventorySchema ::
  GetInventorySchema
newGetInventorySchema =
  GetInventorySchema'
    { nextToken = Prelude.Nothing,
      typeName = Prelude.Nothing,
      aggregator = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      subType = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
getInventorySchema_nextToken :: Lens.Lens' GetInventorySchema (Prelude.Maybe Prelude.Text)
getInventorySchema_nextToken = Lens.lens (\GetInventorySchema' {nextToken} -> nextToken) (\s@GetInventorySchema' {} a -> s {nextToken = a} :: GetInventorySchema)

-- | The type of inventory item to return.
getInventorySchema_typeName :: Lens.Lens' GetInventorySchema (Prelude.Maybe Prelude.Text)
getInventorySchema_typeName = Lens.lens (\GetInventorySchema' {typeName} -> typeName) (\s@GetInventorySchema' {} a -> s {typeName = a} :: GetInventorySchema)

-- | Returns inventory schemas that support aggregation. For example, this
-- call returns the @AWS:InstanceInformation@ type, because it supports
-- aggregation based on the @PlatformName@, @PlatformType@, and
-- @PlatformVersion@ attributes.
getInventorySchema_aggregator :: Lens.Lens' GetInventorySchema (Prelude.Maybe Prelude.Bool)
getInventorySchema_aggregator = Lens.lens (\GetInventorySchema' {aggregator} -> aggregator) (\s@GetInventorySchema' {} a -> s {aggregator = a} :: GetInventorySchema)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getInventorySchema_maxResults :: Lens.Lens' GetInventorySchema (Prelude.Maybe Prelude.Natural)
getInventorySchema_maxResults = Lens.lens (\GetInventorySchema' {maxResults} -> maxResults) (\s@GetInventorySchema' {} a -> s {maxResults = a} :: GetInventorySchema)

-- | Returns the sub-type schema for a specified inventory type.
getInventorySchema_subType :: Lens.Lens' GetInventorySchema (Prelude.Maybe Prelude.Bool)
getInventorySchema_subType = Lens.lens (\GetInventorySchema' {subType} -> subType) (\s@GetInventorySchema' {} a -> s {subType = a} :: GetInventorySchema)

instance Core.AWSPager GetInventorySchema where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getInventorySchemaResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getInventorySchemaResponse_schemas
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getInventorySchema_nextToken
          Lens..~ rs
          Lens.^? getInventorySchemaResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetInventorySchema where
  type
    AWSResponse GetInventorySchema =
      GetInventorySchemaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInventorySchemaResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Schemas" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInventorySchema where
  hashWithSalt _salt GetInventorySchema' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` aggregator
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` subType

instance Prelude.NFData GetInventorySchema where
  rnf GetInventorySchema' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf aggregator
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf subType

instance Data.ToHeaders GetInventorySchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.GetInventorySchema" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInventorySchema where
  toJSON GetInventorySchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("TypeName" Data..=) Prelude.<$> typeName,
            ("Aggregator" Data..=) Prelude.<$> aggregator,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("SubType" Data..=) Prelude.<$> subType
          ]
      )

instance Data.ToPath GetInventorySchema where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInventorySchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInventorySchemaResponse' smart constructor.
data GetInventorySchemaResponse = GetInventorySchemaResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Inventory schemas returned by the request.
    schemas :: Prelude.Maybe [InventoryItemSchema],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetInventorySchemaResponse
newGetInventorySchemaResponse pHttpStatus_ =
  GetInventorySchemaResponse'
    { nextToken =
        Prelude.Nothing,
      schemas = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
getInventorySchemaResponse_nextToken :: Lens.Lens' GetInventorySchemaResponse (Prelude.Maybe Prelude.Text)
getInventorySchemaResponse_nextToken = Lens.lens (\GetInventorySchemaResponse' {nextToken} -> nextToken) (\s@GetInventorySchemaResponse' {} a -> s {nextToken = a} :: GetInventorySchemaResponse)

-- | Inventory schemas returned by the request.
getInventorySchemaResponse_schemas :: Lens.Lens' GetInventorySchemaResponse (Prelude.Maybe [InventoryItemSchema])
getInventorySchemaResponse_schemas = Lens.lens (\GetInventorySchemaResponse' {schemas} -> schemas) (\s@GetInventorySchemaResponse' {} a -> s {schemas = a} :: GetInventorySchemaResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getInventorySchemaResponse_httpStatus :: Lens.Lens' GetInventorySchemaResponse Prelude.Int
getInventorySchemaResponse_httpStatus = Lens.lens (\GetInventorySchemaResponse' {httpStatus} -> httpStatus) (\s@GetInventorySchemaResponse' {} a -> s {httpStatus = a} :: GetInventorySchemaResponse)

instance Prelude.NFData GetInventorySchemaResponse where
  rnf GetInventorySchemaResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemas
      `Prelude.seq` Prelude.rnf httpStatus
