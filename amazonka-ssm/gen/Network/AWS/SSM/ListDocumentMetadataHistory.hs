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
-- Module      : Network.AWS.SSM.ListDocumentMetadataHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about approval reviews for a version of an SSM document.
module Network.AWS.SSM.ListDocumentMetadataHistory
  ( -- * Creating a Request
    ListDocumentMetadataHistory (..),
    newListDocumentMetadataHistory,

    -- * Request Lenses
    listDocumentMetadataHistory_nextToken,
    listDocumentMetadataHistory_maxResults,
    listDocumentMetadataHistory_documentVersion,
    listDocumentMetadataHistory_name,
    listDocumentMetadataHistory_metadata,

    -- * Destructuring the Response
    ListDocumentMetadataHistoryResponse (..),
    newListDocumentMetadataHistoryResponse,

    -- * Response Lenses
    listDocumentMetadataHistoryResponse_nextToken,
    listDocumentMetadataHistoryResponse_author,
    listDocumentMetadataHistoryResponse_metadata,
    listDocumentMetadataHistoryResponse_name,
    listDocumentMetadataHistoryResponse_documentVersion,
    listDocumentMetadataHistoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListDocumentMetadataHistory' smart constructor.
data ListDocumentMetadataHistory = ListDocumentMetadataHistory'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The version of the document.
    documentVersion :: Core.Maybe Core.Text,
    -- | The name of the document.
    name :: Core.Text,
    -- | The type of data for which details are being requested. Currently, the
    -- only supported value is @DocumentReviews@.
    metadata :: DocumentMetadataEnum
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDocumentMetadataHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDocumentMetadataHistory_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'listDocumentMetadataHistory_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'documentVersion', 'listDocumentMetadataHistory_documentVersion' - The version of the document.
--
-- 'name', 'listDocumentMetadataHistory_name' - The name of the document.
--
-- 'metadata', 'listDocumentMetadataHistory_metadata' - The type of data for which details are being requested. Currently, the
-- only supported value is @DocumentReviews@.
newListDocumentMetadataHistory ::
  -- | 'name'
  Core.Text ->
  -- | 'metadata'
  DocumentMetadataEnum ->
  ListDocumentMetadataHistory
newListDocumentMetadataHistory pName_ pMetadata_ =
  ListDocumentMetadataHistory'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      documentVersion = Core.Nothing,
      name = pName_,
      metadata = pMetadata_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
listDocumentMetadataHistory_nextToken :: Lens.Lens' ListDocumentMetadataHistory (Core.Maybe Core.Text)
listDocumentMetadataHistory_nextToken = Lens.lens (\ListDocumentMetadataHistory' {nextToken} -> nextToken) (\s@ListDocumentMetadataHistory' {} a -> s {nextToken = a} :: ListDocumentMetadataHistory)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listDocumentMetadataHistory_maxResults :: Lens.Lens' ListDocumentMetadataHistory (Core.Maybe Core.Natural)
listDocumentMetadataHistory_maxResults = Lens.lens (\ListDocumentMetadataHistory' {maxResults} -> maxResults) (\s@ListDocumentMetadataHistory' {} a -> s {maxResults = a} :: ListDocumentMetadataHistory)

-- | The version of the document.
listDocumentMetadataHistory_documentVersion :: Lens.Lens' ListDocumentMetadataHistory (Core.Maybe Core.Text)
listDocumentMetadataHistory_documentVersion = Lens.lens (\ListDocumentMetadataHistory' {documentVersion} -> documentVersion) (\s@ListDocumentMetadataHistory' {} a -> s {documentVersion = a} :: ListDocumentMetadataHistory)

-- | The name of the document.
listDocumentMetadataHistory_name :: Lens.Lens' ListDocumentMetadataHistory Core.Text
listDocumentMetadataHistory_name = Lens.lens (\ListDocumentMetadataHistory' {name} -> name) (\s@ListDocumentMetadataHistory' {} a -> s {name = a} :: ListDocumentMetadataHistory)

-- | The type of data for which details are being requested. Currently, the
-- only supported value is @DocumentReviews@.
listDocumentMetadataHistory_metadata :: Lens.Lens' ListDocumentMetadataHistory DocumentMetadataEnum
listDocumentMetadataHistory_metadata = Lens.lens (\ListDocumentMetadataHistory' {metadata} -> metadata) (\s@ListDocumentMetadataHistory' {} a -> s {metadata = a} :: ListDocumentMetadataHistory)

instance Core.AWSRequest ListDocumentMetadataHistory where
  type
    AWSResponse ListDocumentMetadataHistory =
      ListDocumentMetadataHistoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentMetadataHistoryResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Author")
            Core.<*> (x Core..?> "Metadata")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "DocumentVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDocumentMetadataHistory

instance Core.NFData ListDocumentMetadataHistory

instance Core.ToHeaders ListDocumentMetadataHistory where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListDocumentMetadataHistory" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDocumentMetadataHistory where
  toJSON ListDocumentMetadataHistory' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            Core.Just ("Name" Core..= name),
            Core.Just ("Metadata" Core..= metadata)
          ]
      )

instance Core.ToPath ListDocumentMetadataHistory where
  toPath = Core.const "/"

instance Core.ToQuery ListDocumentMetadataHistory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDocumentMetadataHistoryResponse' smart constructor.
data ListDocumentMetadataHistoryResponse = ListDocumentMetadataHistoryResponse'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The user ID of the person in the organization who requested the document
    -- review.
    author :: Core.Maybe Core.Text,
    -- | Information about the response to the document approval request.
    metadata :: Core.Maybe DocumentMetadataResponseInfo,
    -- | The name of the document.
    name :: Core.Maybe Core.Text,
    -- | The version of the document.
    documentVersion :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDocumentMetadataHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDocumentMetadataHistoryResponse_nextToken' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'author', 'listDocumentMetadataHistoryResponse_author' - The user ID of the person in the organization who requested the document
-- review.
--
-- 'metadata', 'listDocumentMetadataHistoryResponse_metadata' - Information about the response to the document approval request.
--
-- 'name', 'listDocumentMetadataHistoryResponse_name' - The name of the document.
--
-- 'documentVersion', 'listDocumentMetadataHistoryResponse_documentVersion' - The version of the document.
--
-- 'httpStatus', 'listDocumentMetadataHistoryResponse_httpStatus' - The response's http status code.
newListDocumentMetadataHistoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDocumentMetadataHistoryResponse
newListDocumentMetadataHistoryResponse pHttpStatus_ =
  ListDocumentMetadataHistoryResponse'
    { nextToken =
        Core.Nothing,
      author = Core.Nothing,
      metadata = Core.Nothing,
      name = Core.Nothing,
      documentVersion = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listDocumentMetadataHistoryResponse_nextToken :: Lens.Lens' ListDocumentMetadataHistoryResponse (Core.Maybe Core.Text)
listDocumentMetadataHistoryResponse_nextToken = Lens.lens (\ListDocumentMetadataHistoryResponse' {nextToken} -> nextToken) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {nextToken = a} :: ListDocumentMetadataHistoryResponse)

-- | The user ID of the person in the organization who requested the document
-- review.
listDocumentMetadataHistoryResponse_author :: Lens.Lens' ListDocumentMetadataHistoryResponse (Core.Maybe Core.Text)
listDocumentMetadataHistoryResponse_author = Lens.lens (\ListDocumentMetadataHistoryResponse' {author} -> author) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {author = a} :: ListDocumentMetadataHistoryResponse)

-- | Information about the response to the document approval request.
listDocumentMetadataHistoryResponse_metadata :: Lens.Lens' ListDocumentMetadataHistoryResponse (Core.Maybe DocumentMetadataResponseInfo)
listDocumentMetadataHistoryResponse_metadata = Lens.lens (\ListDocumentMetadataHistoryResponse' {metadata} -> metadata) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {metadata = a} :: ListDocumentMetadataHistoryResponse)

-- | The name of the document.
listDocumentMetadataHistoryResponse_name :: Lens.Lens' ListDocumentMetadataHistoryResponse (Core.Maybe Core.Text)
listDocumentMetadataHistoryResponse_name = Lens.lens (\ListDocumentMetadataHistoryResponse' {name} -> name) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {name = a} :: ListDocumentMetadataHistoryResponse)

-- | The version of the document.
listDocumentMetadataHistoryResponse_documentVersion :: Lens.Lens' ListDocumentMetadataHistoryResponse (Core.Maybe Core.Text)
listDocumentMetadataHistoryResponse_documentVersion = Lens.lens (\ListDocumentMetadataHistoryResponse' {documentVersion} -> documentVersion) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {documentVersion = a} :: ListDocumentMetadataHistoryResponse)

-- | The response's http status code.
listDocumentMetadataHistoryResponse_httpStatus :: Lens.Lens' ListDocumentMetadataHistoryResponse Core.Int
listDocumentMetadataHistoryResponse_httpStatus = Lens.lens (\ListDocumentMetadataHistoryResponse' {httpStatus} -> httpStatus) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {httpStatus = a} :: ListDocumentMetadataHistoryResponse)

instance
  Core.NFData
    ListDocumentMetadataHistoryResponse
