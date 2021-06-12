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
-- Module      : Network.AWS.APIGateway.GetApiKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current ApiKeys resource.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetApiKeys
  ( -- * Creating a Request
    GetApiKeys (..),
    newGetApiKeys,

    -- * Request Lenses
    getApiKeys_customerId,
    getApiKeys_includeValues,
    getApiKeys_position,
    getApiKeys_limit,
    getApiKeys_nameQuery,

    -- * Destructuring the Response
    GetApiKeysResponse (..),
    newGetApiKeysResponse,

    -- * Response Lenses
    getApiKeysResponse_warnings,
    getApiKeysResponse_items,
    getApiKeysResponse_position,
    getApiKeysResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to get information about the current ApiKeys resource.
--
-- /See:/ 'newGetApiKeys' smart constructor.
data GetApiKeys = GetApiKeys'
  { -- | The identifier of a customer in AWS Marketplace or an external system,
    -- such as a developer portal.
    customerId :: Core.Maybe Core.Text,
    -- | A boolean flag to specify whether (@true@) or not (@false@) the result
    -- contains key values.
    includeValues :: Core.Maybe Core.Bool,
    -- | The current pagination position in the paged result set.
    position :: Core.Maybe Core.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Core.Maybe Core.Int,
    -- | The name of queried API keys.
    nameQuery :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApiKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerId', 'getApiKeys_customerId' - The identifier of a customer in AWS Marketplace or an external system,
-- such as a developer portal.
--
-- 'includeValues', 'getApiKeys_includeValues' - A boolean flag to specify whether (@true@) or not (@false@) the result
-- contains key values.
--
-- 'position', 'getApiKeys_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getApiKeys_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'nameQuery', 'getApiKeys_nameQuery' - The name of queried API keys.
newGetApiKeys ::
  GetApiKeys
newGetApiKeys =
  GetApiKeys'
    { customerId = Core.Nothing,
      includeValues = Core.Nothing,
      position = Core.Nothing,
      limit = Core.Nothing,
      nameQuery = Core.Nothing
    }

-- | The identifier of a customer in AWS Marketplace or an external system,
-- such as a developer portal.
getApiKeys_customerId :: Lens.Lens' GetApiKeys (Core.Maybe Core.Text)
getApiKeys_customerId = Lens.lens (\GetApiKeys' {customerId} -> customerId) (\s@GetApiKeys' {} a -> s {customerId = a} :: GetApiKeys)

-- | A boolean flag to specify whether (@true@) or not (@false@) the result
-- contains key values.
getApiKeys_includeValues :: Lens.Lens' GetApiKeys (Core.Maybe Core.Bool)
getApiKeys_includeValues = Lens.lens (\GetApiKeys' {includeValues} -> includeValues) (\s@GetApiKeys' {} a -> s {includeValues = a} :: GetApiKeys)

-- | The current pagination position in the paged result set.
getApiKeys_position :: Lens.Lens' GetApiKeys (Core.Maybe Core.Text)
getApiKeys_position = Lens.lens (\GetApiKeys' {position} -> position) (\s@GetApiKeys' {} a -> s {position = a} :: GetApiKeys)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getApiKeys_limit :: Lens.Lens' GetApiKeys (Core.Maybe Core.Int)
getApiKeys_limit = Lens.lens (\GetApiKeys' {limit} -> limit) (\s@GetApiKeys' {} a -> s {limit = a} :: GetApiKeys)

-- | The name of queried API keys.
getApiKeys_nameQuery :: Lens.Lens' GetApiKeys (Core.Maybe Core.Text)
getApiKeys_nameQuery = Lens.lens (\GetApiKeys' {nameQuery} -> nameQuery) (\s@GetApiKeys' {} a -> s {nameQuery = a} :: GetApiKeys)

instance Core.AWSPager GetApiKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getApiKeysResponse_position Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getApiKeysResponse_items Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getApiKeys_position
          Lens..~ rs
          Lens.^? getApiKeysResponse_position Core.. Lens._Just

instance Core.AWSRequest GetApiKeys where
  type AWSResponse GetApiKeys = GetApiKeysResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApiKeysResponse'
            Core.<$> (x Core..?> "warnings" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetApiKeys

instance Core.NFData GetApiKeys

instance Core.ToHeaders GetApiKeys where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetApiKeys where
  toPath = Core.const "/apikeys"

instance Core.ToQuery GetApiKeys where
  toQuery GetApiKeys' {..} =
    Core.mconcat
      [ "customerId" Core.=: customerId,
        "includeValues" Core.=: includeValues,
        "position" Core.=: position,
        "limit" Core.=: limit,
        "name" Core.=: nameQuery
      ]

-- | Represents a collection of API keys as represented by an ApiKeys
-- resource.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html Use API Keys>
--
-- /See:/ 'newGetApiKeysResponse' smart constructor.
data GetApiKeysResponse = GetApiKeysResponse'
  { -- | A list of warning messages logged during the import of API keys when the
    -- @failOnWarnings@ option is set to true.
    warnings :: Core.Maybe [Core.Text],
    -- | The current page of elements from this collection.
    items :: Core.Maybe [ApiKey],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApiKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warnings', 'getApiKeysResponse_warnings' - A list of warning messages logged during the import of API keys when the
-- @failOnWarnings@ option is set to true.
--
-- 'items', 'getApiKeysResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getApiKeysResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getApiKeysResponse_httpStatus' - The response's http status code.
newGetApiKeysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetApiKeysResponse
newGetApiKeysResponse pHttpStatus_ =
  GetApiKeysResponse'
    { warnings = Core.Nothing,
      items = Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of warning messages logged during the import of API keys when the
-- @failOnWarnings@ option is set to true.
getApiKeysResponse_warnings :: Lens.Lens' GetApiKeysResponse (Core.Maybe [Core.Text])
getApiKeysResponse_warnings = Lens.lens (\GetApiKeysResponse' {warnings} -> warnings) (\s@GetApiKeysResponse' {} a -> s {warnings = a} :: GetApiKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | The current page of elements from this collection.
getApiKeysResponse_items :: Lens.Lens' GetApiKeysResponse (Core.Maybe [ApiKey])
getApiKeysResponse_items = Lens.lens (\GetApiKeysResponse' {items} -> items) (\s@GetApiKeysResponse' {} a -> s {items = a} :: GetApiKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getApiKeysResponse_position :: Lens.Lens' GetApiKeysResponse (Core.Maybe Core.Text)
getApiKeysResponse_position = Lens.lens (\GetApiKeysResponse' {position} -> position) (\s@GetApiKeysResponse' {} a -> s {position = a} :: GetApiKeysResponse)

-- | The response's http status code.
getApiKeysResponse_httpStatus :: Lens.Lens' GetApiKeysResponse Core.Int
getApiKeysResponse_httpStatus = Lens.lens (\GetApiKeysResponse' {httpStatus} -> httpStatus) (\s@GetApiKeysResponse' {} a -> s {httpStatus = a} :: GetApiKeysResponse)

instance Core.NFData GetApiKeysResponse
