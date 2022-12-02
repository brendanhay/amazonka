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
-- Module      : Amazonka.SSM.ListInventoryEntries
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of inventory items returned by the request.
module Amazonka.SSM.ListInventoryEntries
  ( -- * Creating a Request
    ListInventoryEntries (..),
    newListInventoryEntries,

    -- * Request Lenses
    listInventoryEntries_nextToken,
    listInventoryEntries_filters,
    listInventoryEntries_maxResults,
    listInventoryEntries_instanceId,
    listInventoryEntries_typeName,

    -- * Destructuring the Response
    ListInventoryEntriesResponse (..),
    newListInventoryEntriesResponse,

    -- * Response Lenses
    listInventoryEntriesResponse_nextToken,
    listInventoryEntriesResponse_typeName,
    listInventoryEntriesResponse_instanceId,
    listInventoryEntriesResponse_entries,
    listInventoryEntriesResponse_captureTime,
    listInventoryEntriesResponse_schemaVersion,
    listInventoryEntriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newListInventoryEntries' smart constructor.
data ListInventoryEntries = ListInventoryEntries'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters. Use a filter to return a more specific list of
    -- results.
    filters :: Prelude.Maybe (Prelude.NonEmpty InventoryFilter),
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The managed node ID for which you want inventory information.
    instanceId :: Prelude.Text,
    -- | The type of inventory item for which you want information.
    typeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInventoryEntries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInventoryEntries_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'filters', 'listInventoryEntries_filters' - One or more filters. Use a filter to return a more specific list of
-- results.
--
-- 'maxResults', 'listInventoryEntries_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'instanceId', 'listInventoryEntries_instanceId' - The managed node ID for which you want inventory information.
--
-- 'typeName', 'listInventoryEntries_typeName' - The type of inventory item for which you want information.
newListInventoryEntries ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'typeName'
  Prelude.Text ->
  ListInventoryEntries
newListInventoryEntries pInstanceId_ pTypeName_ =
  ListInventoryEntries'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_,
      typeName = pTypeName_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
listInventoryEntries_nextToken :: Lens.Lens' ListInventoryEntries (Prelude.Maybe Prelude.Text)
listInventoryEntries_nextToken = Lens.lens (\ListInventoryEntries' {nextToken} -> nextToken) (\s@ListInventoryEntries' {} a -> s {nextToken = a} :: ListInventoryEntries)

-- | One or more filters. Use a filter to return a more specific list of
-- results.
listInventoryEntries_filters :: Lens.Lens' ListInventoryEntries (Prelude.Maybe (Prelude.NonEmpty InventoryFilter))
listInventoryEntries_filters = Lens.lens (\ListInventoryEntries' {filters} -> filters) (\s@ListInventoryEntries' {} a -> s {filters = a} :: ListInventoryEntries) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listInventoryEntries_maxResults :: Lens.Lens' ListInventoryEntries (Prelude.Maybe Prelude.Natural)
listInventoryEntries_maxResults = Lens.lens (\ListInventoryEntries' {maxResults} -> maxResults) (\s@ListInventoryEntries' {} a -> s {maxResults = a} :: ListInventoryEntries)

-- | The managed node ID for which you want inventory information.
listInventoryEntries_instanceId :: Lens.Lens' ListInventoryEntries Prelude.Text
listInventoryEntries_instanceId = Lens.lens (\ListInventoryEntries' {instanceId} -> instanceId) (\s@ListInventoryEntries' {} a -> s {instanceId = a} :: ListInventoryEntries)

-- | The type of inventory item for which you want information.
listInventoryEntries_typeName :: Lens.Lens' ListInventoryEntries Prelude.Text
listInventoryEntries_typeName = Lens.lens (\ListInventoryEntries' {typeName} -> typeName) (\s@ListInventoryEntries' {} a -> s {typeName = a} :: ListInventoryEntries)

instance Core.AWSRequest ListInventoryEntries where
  type
    AWSResponse ListInventoryEntries =
      ListInventoryEntriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInventoryEntriesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TypeName")
            Prelude.<*> (x Data..?> "InstanceId")
            Prelude.<*> (x Data..?> "Entries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "CaptureTime")
            Prelude.<*> (x Data..?> "SchemaVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInventoryEntries where
  hashWithSalt _salt ListInventoryEntries' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` typeName

instance Prelude.NFData ListInventoryEntries where
  rnf ListInventoryEntries' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf typeName

instance Data.ToHeaders ListInventoryEntries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.ListInventoryEntries" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListInventoryEntries where
  toJSON ListInventoryEntries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("TypeName" Data..= typeName)
          ]
      )

instance Data.ToPath ListInventoryEntries where
  toPath = Prelude.const "/"

instance Data.ToQuery ListInventoryEntries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInventoryEntriesResponse' smart constructor.
data ListInventoryEntriesResponse = ListInventoryEntriesResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of inventory item returned by the request.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The managed node ID targeted by the request to query inventory
    -- information.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | A list of inventory items on the managed node(s).
    entries :: Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text],
    -- | The time that inventory information was collected for the managed
    -- node(s).
    captureTime :: Prelude.Maybe Prelude.Text,
    -- | The inventory schema version used by the managed node(s).
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInventoryEntriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInventoryEntriesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'typeName', 'listInventoryEntriesResponse_typeName' - The type of inventory item returned by the request.
--
-- 'instanceId', 'listInventoryEntriesResponse_instanceId' - The managed node ID targeted by the request to query inventory
-- information.
--
-- 'entries', 'listInventoryEntriesResponse_entries' - A list of inventory items on the managed node(s).
--
-- 'captureTime', 'listInventoryEntriesResponse_captureTime' - The time that inventory information was collected for the managed
-- node(s).
--
-- 'schemaVersion', 'listInventoryEntriesResponse_schemaVersion' - The inventory schema version used by the managed node(s).
--
-- 'httpStatus', 'listInventoryEntriesResponse_httpStatus' - The response's http status code.
newListInventoryEntriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInventoryEntriesResponse
newListInventoryEntriesResponse pHttpStatus_ =
  ListInventoryEntriesResponse'
    { nextToken =
        Prelude.Nothing,
      typeName = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      entries = Prelude.Nothing,
      captureTime = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
listInventoryEntriesResponse_nextToken :: Lens.Lens' ListInventoryEntriesResponse (Prelude.Maybe Prelude.Text)
listInventoryEntriesResponse_nextToken = Lens.lens (\ListInventoryEntriesResponse' {nextToken} -> nextToken) (\s@ListInventoryEntriesResponse' {} a -> s {nextToken = a} :: ListInventoryEntriesResponse)

-- | The type of inventory item returned by the request.
listInventoryEntriesResponse_typeName :: Lens.Lens' ListInventoryEntriesResponse (Prelude.Maybe Prelude.Text)
listInventoryEntriesResponse_typeName = Lens.lens (\ListInventoryEntriesResponse' {typeName} -> typeName) (\s@ListInventoryEntriesResponse' {} a -> s {typeName = a} :: ListInventoryEntriesResponse)

-- | The managed node ID targeted by the request to query inventory
-- information.
listInventoryEntriesResponse_instanceId :: Lens.Lens' ListInventoryEntriesResponse (Prelude.Maybe Prelude.Text)
listInventoryEntriesResponse_instanceId = Lens.lens (\ListInventoryEntriesResponse' {instanceId} -> instanceId) (\s@ListInventoryEntriesResponse' {} a -> s {instanceId = a} :: ListInventoryEntriesResponse)

-- | A list of inventory items on the managed node(s).
listInventoryEntriesResponse_entries :: Lens.Lens' ListInventoryEntriesResponse (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
listInventoryEntriesResponse_entries = Lens.lens (\ListInventoryEntriesResponse' {entries} -> entries) (\s@ListInventoryEntriesResponse' {} a -> s {entries = a} :: ListInventoryEntriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time that inventory information was collected for the managed
-- node(s).
listInventoryEntriesResponse_captureTime :: Lens.Lens' ListInventoryEntriesResponse (Prelude.Maybe Prelude.Text)
listInventoryEntriesResponse_captureTime = Lens.lens (\ListInventoryEntriesResponse' {captureTime} -> captureTime) (\s@ListInventoryEntriesResponse' {} a -> s {captureTime = a} :: ListInventoryEntriesResponse)

-- | The inventory schema version used by the managed node(s).
listInventoryEntriesResponse_schemaVersion :: Lens.Lens' ListInventoryEntriesResponse (Prelude.Maybe Prelude.Text)
listInventoryEntriesResponse_schemaVersion = Lens.lens (\ListInventoryEntriesResponse' {schemaVersion} -> schemaVersion) (\s@ListInventoryEntriesResponse' {} a -> s {schemaVersion = a} :: ListInventoryEntriesResponse)

-- | The response's http status code.
listInventoryEntriesResponse_httpStatus :: Lens.Lens' ListInventoryEntriesResponse Prelude.Int
listInventoryEntriesResponse_httpStatus = Lens.lens (\ListInventoryEntriesResponse' {httpStatus} -> httpStatus) (\s@ListInventoryEntriesResponse' {} a -> s {httpStatus = a} :: ListInventoryEntriesResponse)

instance Prelude.NFData ListInventoryEntriesResponse where
  rnf ListInventoryEntriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf entries
      `Prelude.seq` Prelude.rnf captureTime
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf httpStatus
