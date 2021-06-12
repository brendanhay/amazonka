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
-- Module      : Network.AWS.SSM.ListInventoryEntries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of inventory items returned by the request.
module Network.AWS.SSM.ListInventoryEntries
  ( -- * Creating a Request
    ListInventoryEntries (..),
    newListInventoryEntries,

    -- * Request Lenses
    listInventoryEntries_nextToken,
    listInventoryEntries_maxResults,
    listInventoryEntries_filters,
    listInventoryEntries_instanceId,
    listInventoryEntries_typeName,

    -- * Destructuring the Response
    ListInventoryEntriesResponse (..),
    newListInventoryEntriesResponse,

    -- * Response Lenses
    listInventoryEntriesResponse_typeName,
    listInventoryEntriesResponse_nextToken,
    listInventoryEntriesResponse_instanceId,
    listInventoryEntriesResponse_captureTime,
    listInventoryEntriesResponse_schemaVersion,
    listInventoryEntriesResponse_entries,
    listInventoryEntriesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListInventoryEntries' smart constructor.
data ListInventoryEntries = ListInventoryEntries'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters. Use a filter to return a more specific list of
    -- results.
    filters :: Core.Maybe (Core.NonEmpty InventoryFilter),
    -- | The instance ID for which you want inventory information.
    instanceId :: Core.Text,
    -- | The type of inventory item for which you want information.
    typeName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxResults', 'listInventoryEntries_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'listInventoryEntries_filters' - One or more filters. Use a filter to return a more specific list of
-- results.
--
-- 'instanceId', 'listInventoryEntries_instanceId' - The instance ID for which you want inventory information.
--
-- 'typeName', 'listInventoryEntries_typeName' - The type of inventory item for which you want information.
newListInventoryEntries ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'typeName'
  Core.Text ->
  ListInventoryEntries
newListInventoryEntries pInstanceId_ pTypeName_ =
  ListInventoryEntries'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      instanceId = pInstanceId_,
      typeName = pTypeName_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
listInventoryEntries_nextToken :: Lens.Lens' ListInventoryEntries (Core.Maybe Core.Text)
listInventoryEntries_nextToken = Lens.lens (\ListInventoryEntries' {nextToken} -> nextToken) (\s@ListInventoryEntries' {} a -> s {nextToken = a} :: ListInventoryEntries)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listInventoryEntries_maxResults :: Lens.Lens' ListInventoryEntries (Core.Maybe Core.Natural)
listInventoryEntries_maxResults = Lens.lens (\ListInventoryEntries' {maxResults} -> maxResults) (\s@ListInventoryEntries' {} a -> s {maxResults = a} :: ListInventoryEntries)

-- | One or more filters. Use a filter to return a more specific list of
-- results.
listInventoryEntries_filters :: Lens.Lens' ListInventoryEntries (Core.Maybe (Core.NonEmpty InventoryFilter))
listInventoryEntries_filters = Lens.lens (\ListInventoryEntries' {filters} -> filters) (\s@ListInventoryEntries' {} a -> s {filters = a} :: ListInventoryEntries) Core.. Lens.mapping Lens._Coerce

-- | The instance ID for which you want inventory information.
listInventoryEntries_instanceId :: Lens.Lens' ListInventoryEntries Core.Text
listInventoryEntries_instanceId = Lens.lens (\ListInventoryEntries' {instanceId} -> instanceId) (\s@ListInventoryEntries' {} a -> s {instanceId = a} :: ListInventoryEntries)

-- | The type of inventory item for which you want information.
listInventoryEntries_typeName :: Lens.Lens' ListInventoryEntries Core.Text
listInventoryEntries_typeName = Lens.lens (\ListInventoryEntries' {typeName} -> typeName) (\s@ListInventoryEntries' {} a -> s {typeName = a} :: ListInventoryEntries)

instance Core.AWSRequest ListInventoryEntries where
  type
    AWSResponse ListInventoryEntries =
      ListInventoryEntriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInventoryEntriesResponse'
            Core.<$> (x Core..?> "TypeName")
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "InstanceId")
            Core.<*> (x Core..?> "CaptureTime")
            Core.<*> (x Core..?> "SchemaVersion")
            Core.<*> (x Core..?> "Entries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListInventoryEntries

instance Core.NFData ListInventoryEntries

instance Core.ToHeaders ListInventoryEntries where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListInventoryEntries" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListInventoryEntries where
  toJSON ListInventoryEntries' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters,
            Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("TypeName" Core..= typeName)
          ]
      )

instance Core.ToPath ListInventoryEntries where
  toPath = Core.const "/"

instance Core.ToQuery ListInventoryEntries where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListInventoryEntriesResponse' smart constructor.
data ListInventoryEntriesResponse = ListInventoryEntriesResponse'
  { -- | The type of inventory item returned by the request.
    typeName :: Core.Maybe Core.Text,
    -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The instance ID targeted by the request to query inventory information.
    instanceId :: Core.Maybe Core.Text,
    -- | The time that inventory information was collected for the instance(s).
    captureTime :: Core.Maybe Core.Text,
    -- | The inventory schema version used by the instance(s).
    schemaVersion :: Core.Maybe Core.Text,
    -- | A list of inventory items on the instance(s).
    entries :: Core.Maybe [Core.HashMap Core.Text Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInventoryEntriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'listInventoryEntriesResponse_typeName' - The type of inventory item returned by the request.
--
-- 'nextToken', 'listInventoryEntriesResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'instanceId', 'listInventoryEntriesResponse_instanceId' - The instance ID targeted by the request to query inventory information.
--
-- 'captureTime', 'listInventoryEntriesResponse_captureTime' - The time that inventory information was collected for the instance(s).
--
-- 'schemaVersion', 'listInventoryEntriesResponse_schemaVersion' - The inventory schema version used by the instance(s).
--
-- 'entries', 'listInventoryEntriesResponse_entries' - A list of inventory items on the instance(s).
--
-- 'httpStatus', 'listInventoryEntriesResponse_httpStatus' - The response's http status code.
newListInventoryEntriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListInventoryEntriesResponse
newListInventoryEntriesResponse pHttpStatus_ =
  ListInventoryEntriesResponse'
    { typeName =
        Core.Nothing,
      nextToken = Core.Nothing,
      instanceId = Core.Nothing,
      captureTime = Core.Nothing,
      schemaVersion = Core.Nothing,
      entries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The type of inventory item returned by the request.
listInventoryEntriesResponse_typeName :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe Core.Text)
listInventoryEntriesResponse_typeName = Lens.lens (\ListInventoryEntriesResponse' {typeName} -> typeName) (\s@ListInventoryEntriesResponse' {} a -> s {typeName = a} :: ListInventoryEntriesResponse)

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
listInventoryEntriesResponse_nextToken :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe Core.Text)
listInventoryEntriesResponse_nextToken = Lens.lens (\ListInventoryEntriesResponse' {nextToken} -> nextToken) (\s@ListInventoryEntriesResponse' {} a -> s {nextToken = a} :: ListInventoryEntriesResponse)

-- | The instance ID targeted by the request to query inventory information.
listInventoryEntriesResponse_instanceId :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe Core.Text)
listInventoryEntriesResponse_instanceId = Lens.lens (\ListInventoryEntriesResponse' {instanceId} -> instanceId) (\s@ListInventoryEntriesResponse' {} a -> s {instanceId = a} :: ListInventoryEntriesResponse)

-- | The time that inventory information was collected for the instance(s).
listInventoryEntriesResponse_captureTime :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe Core.Text)
listInventoryEntriesResponse_captureTime = Lens.lens (\ListInventoryEntriesResponse' {captureTime} -> captureTime) (\s@ListInventoryEntriesResponse' {} a -> s {captureTime = a} :: ListInventoryEntriesResponse)

-- | The inventory schema version used by the instance(s).
listInventoryEntriesResponse_schemaVersion :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe Core.Text)
listInventoryEntriesResponse_schemaVersion = Lens.lens (\ListInventoryEntriesResponse' {schemaVersion} -> schemaVersion) (\s@ListInventoryEntriesResponse' {} a -> s {schemaVersion = a} :: ListInventoryEntriesResponse)

-- | A list of inventory items on the instance(s).
listInventoryEntriesResponse_entries :: Lens.Lens' ListInventoryEntriesResponse (Core.Maybe [Core.HashMap Core.Text Core.Text])
listInventoryEntriesResponse_entries = Lens.lens (\ListInventoryEntriesResponse' {entries} -> entries) (\s@ListInventoryEntriesResponse' {} a -> s {entries = a} :: ListInventoryEntriesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listInventoryEntriesResponse_httpStatus :: Lens.Lens' ListInventoryEntriesResponse Core.Int
listInventoryEntriesResponse_httpStatus = Lens.lens (\ListInventoryEntriesResponse' {httpStatus} -> httpStatus) (\s@ListInventoryEntriesResponse' {} a -> s {httpStatus = a} :: ListInventoryEntriesResponse)

instance Core.NFData ListInventoryEntriesResponse
