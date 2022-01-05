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
-- Module      : Amazonka.APIGateway.GetApiKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current ApiKeys resource.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetApiKeys
  ( -- * Creating a Request
    GetApiKeys (..),
    newGetApiKeys,

    -- * Request Lenses
    getApiKeys_includeValues,
    getApiKeys_customerId,
    getApiKeys_nameQuery,
    getApiKeys_limit,
    getApiKeys_position,

    -- * Destructuring the Response
    GetApiKeysResponse (..),
    newGetApiKeysResponse,

    -- * Response Lenses
    getApiKeysResponse_items,
    getApiKeysResponse_warnings,
    getApiKeysResponse_position,
    getApiKeysResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to get information about the current ApiKeys resource.
--
-- /See:/ 'newGetApiKeys' smart constructor.
data GetApiKeys = GetApiKeys'
  { -- | A boolean flag to specify whether (@true@) or not (@false@) the result
    -- contains key values.
    includeValues :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of a customer in AWS Marketplace or an external system,
    -- such as a developer portal.
    customerId :: Prelude.Maybe Prelude.Text,
    -- | The name of queried API keys.
    nameQuery :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApiKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeValues', 'getApiKeys_includeValues' - A boolean flag to specify whether (@true@) or not (@false@) the result
-- contains key values.
--
-- 'customerId', 'getApiKeys_customerId' - The identifier of a customer in AWS Marketplace or an external system,
-- such as a developer portal.
--
-- 'nameQuery', 'getApiKeys_nameQuery' - The name of queried API keys.
--
-- 'limit', 'getApiKeys_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'position', 'getApiKeys_position' - The current pagination position in the paged result set.
newGetApiKeys ::
  GetApiKeys
newGetApiKeys =
  GetApiKeys'
    { includeValues = Prelude.Nothing,
      customerId = Prelude.Nothing,
      nameQuery = Prelude.Nothing,
      limit = Prelude.Nothing,
      position = Prelude.Nothing
    }

-- | A boolean flag to specify whether (@true@) or not (@false@) the result
-- contains key values.
getApiKeys_includeValues :: Lens.Lens' GetApiKeys (Prelude.Maybe Prelude.Bool)
getApiKeys_includeValues = Lens.lens (\GetApiKeys' {includeValues} -> includeValues) (\s@GetApiKeys' {} a -> s {includeValues = a} :: GetApiKeys)

-- | The identifier of a customer in AWS Marketplace or an external system,
-- such as a developer portal.
getApiKeys_customerId :: Lens.Lens' GetApiKeys (Prelude.Maybe Prelude.Text)
getApiKeys_customerId = Lens.lens (\GetApiKeys' {customerId} -> customerId) (\s@GetApiKeys' {} a -> s {customerId = a} :: GetApiKeys)

-- | The name of queried API keys.
getApiKeys_nameQuery :: Lens.Lens' GetApiKeys (Prelude.Maybe Prelude.Text)
getApiKeys_nameQuery = Lens.lens (\GetApiKeys' {nameQuery} -> nameQuery) (\s@GetApiKeys' {} a -> s {nameQuery = a} :: GetApiKeys)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getApiKeys_limit :: Lens.Lens' GetApiKeys (Prelude.Maybe Prelude.Int)
getApiKeys_limit = Lens.lens (\GetApiKeys' {limit} -> limit) (\s@GetApiKeys' {} a -> s {limit = a} :: GetApiKeys)

-- | The current pagination position in the paged result set.
getApiKeys_position :: Lens.Lens' GetApiKeys (Prelude.Maybe Prelude.Text)
getApiKeys_position = Lens.lens (\GetApiKeys' {position} -> position) (\s@GetApiKeys' {} a -> s {position = a} :: GetApiKeys)

instance Core.AWSPager GetApiKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getApiKeysResponse_position Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getApiKeysResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getApiKeys_position
          Lens..~ rs
          Lens.^? getApiKeysResponse_position Prelude.. Lens._Just

instance Core.AWSRequest GetApiKeys where
  type AWSResponse GetApiKeys = GetApiKeysResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApiKeysResponse'
            Prelude.<$> (x Core..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApiKeys where
  hashWithSalt _salt GetApiKeys' {..} =
    _salt `Prelude.hashWithSalt` includeValues
      `Prelude.hashWithSalt` customerId
      `Prelude.hashWithSalt` nameQuery
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position

instance Prelude.NFData GetApiKeys where
  rnf GetApiKeys' {..} =
    Prelude.rnf includeValues
      `Prelude.seq` Prelude.rnf customerId
      `Prelude.seq` Prelude.rnf nameQuery
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf position

instance Core.ToHeaders GetApiKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetApiKeys where
  toPath = Prelude.const "/apikeys"

instance Core.ToQuery GetApiKeys where
  toQuery GetApiKeys' {..} =
    Prelude.mconcat
      [ "includeValues" Core.=: includeValues,
        "customerId" Core.=: customerId,
        "name" Core.=: nameQuery,
        "limit" Core.=: limit,
        "position" Core.=: position
      ]

-- | Represents a collection of API keys as represented by an ApiKeys
-- resource.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html Use API Keys>
--
-- /See:/ 'newGetApiKeysResponse' smart constructor.
data GetApiKeysResponse = GetApiKeysResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [ApiKey],
    -- | A list of warning messages logged during the import of API keys when the
    -- @failOnWarnings@ option is set to true.
    warnings :: Prelude.Maybe [Prelude.Text],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApiKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getApiKeysResponse_items' - The current page of elements from this collection.
--
-- 'warnings', 'getApiKeysResponse_warnings' - A list of warning messages logged during the import of API keys when the
-- @failOnWarnings@ option is set to true.
--
-- 'position', 'getApiKeysResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getApiKeysResponse_httpStatus' - The response's http status code.
newGetApiKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApiKeysResponse
newGetApiKeysResponse pHttpStatus_ =
  GetApiKeysResponse'
    { items = Prelude.Nothing,
      warnings = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getApiKeysResponse_items :: Lens.Lens' GetApiKeysResponse (Prelude.Maybe [ApiKey])
getApiKeysResponse_items = Lens.lens (\GetApiKeysResponse' {items} -> items) (\s@GetApiKeysResponse' {} a -> s {items = a} :: GetApiKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of warning messages logged during the import of API keys when the
-- @failOnWarnings@ option is set to true.
getApiKeysResponse_warnings :: Lens.Lens' GetApiKeysResponse (Prelude.Maybe [Prelude.Text])
getApiKeysResponse_warnings = Lens.lens (\GetApiKeysResponse' {warnings} -> warnings) (\s@GetApiKeysResponse' {} a -> s {warnings = a} :: GetApiKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getApiKeysResponse_position :: Lens.Lens' GetApiKeysResponse (Prelude.Maybe Prelude.Text)
getApiKeysResponse_position = Lens.lens (\GetApiKeysResponse' {position} -> position) (\s@GetApiKeysResponse' {} a -> s {position = a} :: GetApiKeysResponse)

-- | The response's http status code.
getApiKeysResponse_httpStatus :: Lens.Lens' GetApiKeysResponse Prelude.Int
getApiKeysResponse_httpStatus = Lens.lens (\GetApiKeysResponse' {httpStatus} -> httpStatus) (\s@GetApiKeysResponse' {} a -> s {httpStatus = a} :: GetApiKeysResponse)

instance Prelude.NFData GetApiKeysResponse where
  rnf GetApiKeysResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
