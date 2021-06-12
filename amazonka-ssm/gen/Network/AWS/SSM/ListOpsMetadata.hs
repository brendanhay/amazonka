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
-- Module      : Network.AWS.SSM.ListOpsMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Systems Manager calls this API action when displaying all Application
-- Manager OpsMetadata objects or blobs.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListOpsMetadata
  ( -- * Creating a Request
    ListOpsMetadata (..),
    newListOpsMetadata,

    -- * Request Lenses
    listOpsMetadata_nextToken,
    listOpsMetadata_maxResults,
    listOpsMetadata_filters,

    -- * Destructuring the Response
    ListOpsMetadataResponse (..),
    newListOpsMetadataResponse,

    -- * Response Lenses
    listOpsMetadataResponse_nextToken,
    listOpsMetadataResponse_opsMetadataList,
    listOpsMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListOpsMetadata' smart constructor.
data ListOpsMetadata = ListOpsMetadata'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters to limit the number of OpsMetadata objects returned
    -- by the call.
    filters :: Core.Maybe [OpsMetadataFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOpsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOpsMetadata_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'listOpsMetadata_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'listOpsMetadata_filters' - One or more filters to limit the number of OpsMetadata objects returned
-- by the call.
newListOpsMetadata ::
  ListOpsMetadata
newListOpsMetadata =
  ListOpsMetadata'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
listOpsMetadata_nextToken :: Lens.Lens' ListOpsMetadata (Core.Maybe Core.Text)
listOpsMetadata_nextToken = Lens.lens (\ListOpsMetadata' {nextToken} -> nextToken) (\s@ListOpsMetadata' {} a -> s {nextToken = a} :: ListOpsMetadata)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listOpsMetadata_maxResults :: Lens.Lens' ListOpsMetadata (Core.Maybe Core.Natural)
listOpsMetadata_maxResults = Lens.lens (\ListOpsMetadata' {maxResults} -> maxResults) (\s@ListOpsMetadata' {} a -> s {maxResults = a} :: ListOpsMetadata)

-- | One or more filters to limit the number of OpsMetadata objects returned
-- by the call.
listOpsMetadata_filters :: Lens.Lens' ListOpsMetadata (Core.Maybe [OpsMetadataFilter])
listOpsMetadata_filters = Lens.lens (\ListOpsMetadata' {filters} -> filters) (\s@ListOpsMetadata' {} a -> s {filters = a} :: ListOpsMetadata) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListOpsMetadata where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOpsMetadataResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOpsMetadataResponse_opsMetadataList
              Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOpsMetadata_nextToken
          Lens..~ rs
          Lens.^? listOpsMetadataResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListOpsMetadata where
  type
    AWSResponse ListOpsMetadata =
      ListOpsMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOpsMetadataResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "OpsMetadataList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOpsMetadata

instance Core.NFData ListOpsMetadata

instance Core.ToHeaders ListOpsMetadata where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.ListOpsMetadata" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListOpsMetadata where
  toJSON ListOpsMetadata' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath ListOpsMetadata where
  toPath = Core.const "/"

instance Core.ToQuery ListOpsMetadata where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListOpsMetadataResponse' smart constructor.
data ListOpsMetadataResponse = ListOpsMetadataResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a list of OpsMetadata objects.
    opsMetadataList :: Core.Maybe (Core.NonEmpty OpsMetadata),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOpsMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOpsMetadataResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'opsMetadataList', 'listOpsMetadataResponse_opsMetadataList' - Returns a list of OpsMetadata objects.
--
-- 'httpStatus', 'listOpsMetadataResponse_httpStatus' - The response's http status code.
newListOpsMetadataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOpsMetadataResponse
newListOpsMetadataResponse pHttpStatus_ =
  ListOpsMetadataResponse'
    { nextToken = Core.Nothing,
      opsMetadataList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listOpsMetadataResponse_nextToken :: Lens.Lens' ListOpsMetadataResponse (Core.Maybe Core.Text)
listOpsMetadataResponse_nextToken = Lens.lens (\ListOpsMetadataResponse' {nextToken} -> nextToken) (\s@ListOpsMetadataResponse' {} a -> s {nextToken = a} :: ListOpsMetadataResponse)

-- | Returns a list of OpsMetadata objects.
listOpsMetadataResponse_opsMetadataList :: Lens.Lens' ListOpsMetadataResponse (Core.Maybe (Core.NonEmpty OpsMetadata))
listOpsMetadataResponse_opsMetadataList = Lens.lens (\ListOpsMetadataResponse' {opsMetadataList} -> opsMetadataList) (\s@ListOpsMetadataResponse' {} a -> s {opsMetadataList = a} :: ListOpsMetadataResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOpsMetadataResponse_httpStatus :: Lens.Lens' ListOpsMetadataResponse Core.Int
listOpsMetadataResponse_httpStatus = Lens.lens (\ListOpsMetadataResponse' {httpStatus} -> httpStatus) (\s@ListOpsMetadataResponse' {} a -> s {httpStatus = a} :: ListOpsMetadataResponse)

instance Core.NFData ListOpsMetadataResponse
