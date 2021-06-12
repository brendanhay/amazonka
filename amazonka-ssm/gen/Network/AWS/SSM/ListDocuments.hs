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
-- Module      : Network.AWS.SSM.ListDocuments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all Systems Manager (SSM) documents in the current AWS account
-- and Region. You can limit the results of this request by using a filter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListDocuments
  ( -- * Creating a Request
    ListDocuments (..),
    newListDocuments,

    -- * Request Lenses
    listDocuments_nextToken,
    listDocuments_maxResults,
    listDocuments_filters,
    listDocuments_documentFilterList,

    -- * Destructuring the Response
    ListDocumentsResponse (..),
    newListDocumentsResponse,

    -- * Response Lenses
    listDocumentsResponse_nextToken,
    listDocumentsResponse_documentIdentifiers,
    listDocumentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListDocuments' smart constructor.
data ListDocuments = ListDocuments'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more @DocumentKeyValuesFilter@ objects. Use a filter to return a
    -- more specific list of results. For keys, you can specify one or more
    -- key-value pair tags that have been applied to a document. Other valid
    -- keys include @Owner@, @Name@, @PlatformTypes@, @DocumentType@, and
    -- @TargetType@. For example, to return documents you own use
    -- @Key=Owner,Values=Self@. To specify a custom key-value pair, use the
    -- format @Key=tag:tagName,Values=valueName@.
    --
    -- This API action only supports filtering documents by using a single tag
    -- key and one or more tag values. For example:
    -- @Key=tag:tagName,Values=valueName1,valueName2@
    filters :: Core.Maybe [DocumentKeyValuesFilter],
    -- | This data type is deprecated. Instead, use @Filters@.
    documentFilterList :: Core.Maybe (Core.NonEmpty DocumentFilter)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDocuments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDocuments_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'listDocuments_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'listDocuments_filters' - One or more @DocumentKeyValuesFilter@ objects. Use a filter to return a
-- more specific list of results. For keys, you can specify one or more
-- key-value pair tags that have been applied to a document. Other valid
-- keys include @Owner@, @Name@, @PlatformTypes@, @DocumentType@, and
-- @TargetType@. For example, to return documents you own use
-- @Key=Owner,Values=Self@. To specify a custom key-value pair, use the
-- format @Key=tag:tagName,Values=valueName@.
--
-- This API action only supports filtering documents by using a single tag
-- key and one or more tag values. For example:
-- @Key=tag:tagName,Values=valueName1,valueName2@
--
-- 'documentFilterList', 'listDocuments_documentFilterList' - This data type is deprecated. Instead, use @Filters@.
newListDocuments ::
  ListDocuments
newListDocuments =
  ListDocuments'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      documentFilterList = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
listDocuments_nextToken :: Lens.Lens' ListDocuments (Core.Maybe Core.Text)
listDocuments_nextToken = Lens.lens (\ListDocuments' {nextToken} -> nextToken) (\s@ListDocuments' {} a -> s {nextToken = a} :: ListDocuments)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listDocuments_maxResults :: Lens.Lens' ListDocuments (Core.Maybe Core.Natural)
listDocuments_maxResults = Lens.lens (\ListDocuments' {maxResults} -> maxResults) (\s@ListDocuments' {} a -> s {maxResults = a} :: ListDocuments)

-- | One or more @DocumentKeyValuesFilter@ objects. Use a filter to return a
-- more specific list of results. For keys, you can specify one or more
-- key-value pair tags that have been applied to a document. Other valid
-- keys include @Owner@, @Name@, @PlatformTypes@, @DocumentType@, and
-- @TargetType@. For example, to return documents you own use
-- @Key=Owner,Values=Self@. To specify a custom key-value pair, use the
-- format @Key=tag:tagName,Values=valueName@.
--
-- This API action only supports filtering documents by using a single tag
-- key and one or more tag values. For example:
-- @Key=tag:tagName,Values=valueName1,valueName2@
listDocuments_filters :: Lens.Lens' ListDocuments (Core.Maybe [DocumentKeyValuesFilter])
listDocuments_filters = Lens.lens (\ListDocuments' {filters} -> filters) (\s@ListDocuments' {} a -> s {filters = a} :: ListDocuments) Core.. Lens.mapping Lens._Coerce

-- | This data type is deprecated. Instead, use @Filters@.
listDocuments_documentFilterList :: Lens.Lens' ListDocuments (Core.Maybe (Core.NonEmpty DocumentFilter))
listDocuments_documentFilterList = Lens.lens (\ListDocuments' {documentFilterList} -> documentFilterList) (\s@ListDocuments' {} a -> s {documentFilterList = a} :: ListDocuments) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListDocuments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDocumentsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDocumentsResponse_documentIdentifiers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDocuments_nextToken
          Lens..~ rs
          Lens.^? listDocumentsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDocuments where
  type
    AWSResponse ListDocuments =
      ListDocumentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "DocumentIdentifiers"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDocuments

instance Core.NFData ListDocuments

instance Core.ToHeaders ListDocuments where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.ListDocuments" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDocuments where
  toJSON ListDocuments' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters,
            ("DocumentFilterList" Core..=)
              Core.<$> documentFilterList
          ]
      )

instance Core.ToPath ListDocuments where
  toPath = Core.const "/"

instance Core.ToQuery ListDocuments where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDocumentsResponse' smart constructor.
data ListDocumentsResponse = ListDocumentsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The names of the Systems Manager documents.
    documentIdentifiers :: Core.Maybe [DocumentIdentifier],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDocumentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDocumentsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'documentIdentifiers', 'listDocumentsResponse_documentIdentifiers' - The names of the Systems Manager documents.
--
-- 'httpStatus', 'listDocumentsResponse_httpStatus' - The response's http status code.
newListDocumentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDocumentsResponse
newListDocumentsResponse pHttpStatus_ =
  ListDocumentsResponse'
    { nextToken = Core.Nothing,
      documentIdentifiers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
listDocumentsResponse_nextToken :: Lens.Lens' ListDocumentsResponse (Core.Maybe Core.Text)
listDocumentsResponse_nextToken = Lens.lens (\ListDocumentsResponse' {nextToken} -> nextToken) (\s@ListDocumentsResponse' {} a -> s {nextToken = a} :: ListDocumentsResponse)

-- | The names of the Systems Manager documents.
listDocumentsResponse_documentIdentifiers :: Lens.Lens' ListDocumentsResponse (Core.Maybe [DocumentIdentifier])
listDocumentsResponse_documentIdentifiers = Lens.lens (\ListDocumentsResponse' {documentIdentifiers} -> documentIdentifiers) (\s@ListDocumentsResponse' {} a -> s {documentIdentifiers = a} :: ListDocumentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDocumentsResponse_httpStatus :: Lens.Lens' ListDocumentsResponse Core.Int
listDocumentsResponse_httpStatus = Lens.lens (\ListDocumentsResponse' {httpStatus} -> httpStatus) (\s@ListDocumentsResponse' {} a -> s {httpStatus = a} :: ListDocumentsResponse)

instance Core.NFData ListDocumentsResponse
