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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    getApiKeys_customerId,
    getApiKeys_includeValues,
    getApiKeys_limit,
    getApiKeys_nameQuery,
    getApiKeys_position,

    -- * Destructuring the Response
    GetApiKeysResponse (..),
    newGetApiKeysResponse,

    -- * Response Lenses
    getApiKeysResponse_items,
    getApiKeysResponse_position,
    getApiKeysResponse_warnings,
    getApiKeysResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to get information about the current ApiKeys resource.
--
-- /See:/ 'newGetApiKeys' smart constructor.
data GetApiKeys = GetApiKeys'
  { -- | The identifier of a customer in AWS Marketplace or an external system,
    -- such as a developer portal.
    customerId :: Prelude.Maybe Prelude.Text,
    -- | A boolean flag to specify whether (@true@) or not (@false@) the result
    -- contains key values.
    includeValues :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The name of queried API keys.
    nameQuery :: Prelude.Maybe Prelude.Text,
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
-- 'customerId', 'getApiKeys_customerId' - The identifier of a customer in AWS Marketplace or an external system,
-- such as a developer portal.
--
-- 'includeValues', 'getApiKeys_includeValues' - A boolean flag to specify whether (@true@) or not (@false@) the result
-- contains key values.
--
-- 'limit', 'getApiKeys_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'nameQuery', 'getApiKeys_nameQuery' - The name of queried API keys.
--
-- 'position', 'getApiKeys_position' - The current pagination position in the paged result set.
newGetApiKeys ::
  GetApiKeys
newGetApiKeys =
  GetApiKeys'
    { customerId = Prelude.Nothing,
      includeValues = Prelude.Nothing,
      limit = Prelude.Nothing,
      nameQuery = Prelude.Nothing,
      position = Prelude.Nothing
    }

-- | The identifier of a customer in AWS Marketplace or an external system,
-- such as a developer portal.
getApiKeys_customerId :: Lens.Lens' GetApiKeys (Prelude.Maybe Prelude.Text)
getApiKeys_customerId = Lens.lens (\GetApiKeys' {customerId} -> customerId) (\s@GetApiKeys' {} a -> s {customerId = a} :: GetApiKeys)

-- | A boolean flag to specify whether (@true@) or not (@false@) the result
-- contains key values.
getApiKeys_includeValues :: Lens.Lens' GetApiKeys (Prelude.Maybe Prelude.Bool)
getApiKeys_includeValues = Lens.lens (\GetApiKeys' {includeValues} -> includeValues) (\s@GetApiKeys' {} a -> s {includeValues = a} :: GetApiKeys)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getApiKeys_limit :: Lens.Lens' GetApiKeys (Prelude.Maybe Prelude.Int)
getApiKeys_limit = Lens.lens (\GetApiKeys' {limit} -> limit) (\s@GetApiKeys' {} a -> s {limit = a} :: GetApiKeys)

-- | The name of queried API keys.
getApiKeys_nameQuery :: Lens.Lens' GetApiKeys (Prelude.Maybe Prelude.Text)
getApiKeys_nameQuery = Lens.lens (\GetApiKeys' {nameQuery} -> nameQuery) (\s@GetApiKeys' {} a -> s {nameQuery = a} :: GetApiKeys)

-- | The current pagination position in the paged result set.
getApiKeys_position :: Lens.Lens' GetApiKeys (Prelude.Maybe Prelude.Text)
getApiKeys_position = Lens.lens (\GetApiKeys' {position} -> position) (\s@GetApiKeys' {} a -> s {position = a} :: GetApiKeys)

instance Core.AWSPager GetApiKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getApiKeysResponse_position
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getApiKeysResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getApiKeys_position
              Lens..~ rs
              Lens.^? getApiKeysResponse_position
              Prelude.. Lens._Just

instance Core.AWSRequest GetApiKeys where
  type AWSResponse GetApiKeys = GetApiKeysResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApiKeysResponse'
            Prelude.<$> (x Data..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "position")
            Prelude.<*> (x Data..?> "warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApiKeys where
  hashWithSalt _salt GetApiKeys' {..} =
    _salt
      `Prelude.hashWithSalt` customerId
      `Prelude.hashWithSalt` includeValues
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nameQuery
      `Prelude.hashWithSalt` position

instance Prelude.NFData GetApiKeys where
  rnf GetApiKeys' {..} =
    Prelude.rnf customerId `Prelude.seq`
      Prelude.rnf includeValues `Prelude.seq`
        Prelude.rnf limit `Prelude.seq`
          Prelude.rnf nameQuery `Prelude.seq`
            Prelude.rnf position

instance Data.ToHeaders GetApiKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetApiKeys where
  toPath = Prelude.const "/apikeys"

instance Data.ToQuery GetApiKeys where
  toQuery GetApiKeys' {..} =
    Prelude.mconcat
      [ "customerId" Data.=: customerId,
        "includeValues" Data.=: includeValues,
        "limit" Data.=: limit,
        "name" Data.=: nameQuery,
        "position" Data.=: position
      ]

-- | Represents a collection of API keys as represented by an ApiKeys
-- resource.
--
-- /See:/ 'newGetApiKeysResponse' smart constructor.
data GetApiKeysResponse = GetApiKeysResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [ApiKey],
    position :: Prelude.Maybe Prelude.Text,
    -- | A list of warning messages logged during the import of API keys when the
    -- @failOnWarnings@ option is set to true.
    warnings :: Prelude.Maybe [Prelude.Text],
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
-- 'position', 'getApiKeysResponse_position' - Undocumented member.
--
-- 'warnings', 'getApiKeysResponse_warnings' - A list of warning messages logged during the import of API keys when the
-- @failOnWarnings@ option is set to true.
--
-- 'httpStatus', 'getApiKeysResponse_httpStatus' - The response's http status code.
newGetApiKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApiKeysResponse
newGetApiKeysResponse pHttpStatus_ =
  GetApiKeysResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      warnings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getApiKeysResponse_items :: Lens.Lens' GetApiKeysResponse (Prelude.Maybe [ApiKey])
getApiKeysResponse_items = Lens.lens (\GetApiKeysResponse' {items} -> items) (\s@GetApiKeysResponse' {} a -> s {items = a} :: GetApiKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getApiKeysResponse_position :: Lens.Lens' GetApiKeysResponse (Prelude.Maybe Prelude.Text)
getApiKeysResponse_position = Lens.lens (\GetApiKeysResponse' {position} -> position) (\s@GetApiKeysResponse' {} a -> s {position = a} :: GetApiKeysResponse)

-- | A list of warning messages logged during the import of API keys when the
-- @failOnWarnings@ option is set to true.
getApiKeysResponse_warnings :: Lens.Lens' GetApiKeysResponse (Prelude.Maybe [Prelude.Text])
getApiKeysResponse_warnings = Lens.lens (\GetApiKeysResponse' {warnings} -> warnings) (\s@GetApiKeysResponse' {} a -> s {warnings = a} :: GetApiKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getApiKeysResponse_httpStatus :: Lens.Lens' GetApiKeysResponse Prelude.Int
getApiKeysResponse_httpStatus = Lens.lens (\GetApiKeysResponse' {httpStatus} -> httpStatus) (\s@GetApiKeysResponse' {} a -> s {httpStatus = a} :: GetApiKeysResponse)

instance Prelude.NFData GetApiKeysResponse where
  rnf GetApiKeysResponse' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf position `Prelude.seq`
        Prelude.rnf warnings `Prelude.seq`
          Prelude.rnf httpStatus
