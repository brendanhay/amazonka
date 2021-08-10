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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListDocumentMetadataHistory' smart constructor.
data ListDocumentMetadataHistory = ListDocumentMetadataHistory'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The version of the document.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the document.
    name :: Prelude.Text,
    -- | The type of data for which details are being requested. Currently, the
    -- only supported value is @DocumentReviews@.
    metadata :: DocumentMetadataEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'metadata'
  DocumentMetadataEnum ->
  ListDocumentMetadataHistory
newListDocumentMetadataHistory pName_ pMetadata_ =
  ListDocumentMetadataHistory'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      name = pName_,
      metadata = pMetadata_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
listDocumentMetadataHistory_nextToken :: Lens.Lens' ListDocumentMetadataHistory (Prelude.Maybe Prelude.Text)
listDocumentMetadataHistory_nextToken = Lens.lens (\ListDocumentMetadataHistory' {nextToken} -> nextToken) (\s@ListDocumentMetadataHistory' {} a -> s {nextToken = a} :: ListDocumentMetadataHistory)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listDocumentMetadataHistory_maxResults :: Lens.Lens' ListDocumentMetadataHistory (Prelude.Maybe Prelude.Natural)
listDocumentMetadataHistory_maxResults = Lens.lens (\ListDocumentMetadataHistory' {maxResults} -> maxResults) (\s@ListDocumentMetadataHistory' {} a -> s {maxResults = a} :: ListDocumentMetadataHistory)

-- | The version of the document.
listDocumentMetadataHistory_documentVersion :: Lens.Lens' ListDocumentMetadataHistory (Prelude.Maybe Prelude.Text)
listDocumentMetadataHistory_documentVersion = Lens.lens (\ListDocumentMetadataHistory' {documentVersion} -> documentVersion) (\s@ListDocumentMetadataHistory' {} a -> s {documentVersion = a} :: ListDocumentMetadataHistory)

-- | The name of the document.
listDocumentMetadataHistory_name :: Lens.Lens' ListDocumentMetadataHistory Prelude.Text
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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Author")
            Prelude.<*> (x Core..?> "Metadata")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "DocumentVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDocumentMetadataHistory

instance Prelude.NFData ListDocumentMetadataHistory

instance Core.ToHeaders ListDocumentMetadataHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListDocumentMetadataHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDocumentMetadataHistory where
  toJSON ListDocumentMetadataHistory' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("DocumentVersion" Core..=)
              Prelude.<$> documentVersion,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Metadata" Core..= metadata)
          ]
      )

instance Core.ToPath ListDocumentMetadataHistory where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDocumentMetadataHistory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDocumentMetadataHistoryResponse' smart constructor.
data ListDocumentMetadataHistoryResponse = ListDocumentMetadataHistoryResponse'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The user ID of the person in the organization who requested the document
    -- review.
    author :: Prelude.Maybe Prelude.Text,
    -- | Information about the response to the document approval request.
    metadata :: Prelude.Maybe DocumentMetadataResponseInfo,
    -- | The name of the document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the document.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListDocumentMetadataHistoryResponse
newListDocumentMetadataHistoryResponse pHttpStatus_ =
  ListDocumentMetadataHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      author = Prelude.Nothing,
      metadata = Prelude.Nothing,
      name = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listDocumentMetadataHistoryResponse_nextToken :: Lens.Lens' ListDocumentMetadataHistoryResponse (Prelude.Maybe Prelude.Text)
listDocumentMetadataHistoryResponse_nextToken = Lens.lens (\ListDocumentMetadataHistoryResponse' {nextToken} -> nextToken) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {nextToken = a} :: ListDocumentMetadataHistoryResponse)

-- | The user ID of the person in the organization who requested the document
-- review.
listDocumentMetadataHistoryResponse_author :: Lens.Lens' ListDocumentMetadataHistoryResponse (Prelude.Maybe Prelude.Text)
listDocumentMetadataHistoryResponse_author = Lens.lens (\ListDocumentMetadataHistoryResponse' {author} -> author) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {author = a} :: ListDocumentMetadataHistoryResponse)

-- | Information about the response to the document approval request.
listDocumentMetadataHistoryResponse_metadata :: Lens.Lens' ListDocumentMetadataHistoryResponse (Prelude.Maybe DocumentMetadataResponseInfo)
listDocumentMetadataHistoryResponse_metadata = Lens.lens (\ListDocumentMetadataHistoryResponse' {metadata} -> metadata) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {metadata = a} :: ListDocumentMetadataHistoryResponse)

-- | The name of the document.
listDocumentMetadataHistoryResponse_name :: Lens.Lens' ListDocumentMetadataHistoryResponse (Prelude.Maybe Prelude.Text)
listDocumentMetadataHistoryResponse_name = Lens.lens (\ListDocumentMetadataHistoryResponse' {name} -> name) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {name = a} :: ListDocumentMetadataHistoryResponse)

-- | The version of the document.
listDocumentMetadataHistoryResponse_documentVersion :: Lens.Lens' ListDocumentMetadataHistoryResponse (Prelude.Maybe Prelude.Text)
listDocumentMetadataHistoryResponse_documentVersion = Lens.lens (\ListDocumentMetadataHistoryResponse' {documentVersion} -> documentVersion) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {documentVersion = a} :: ListDocumentMetadataHistoryResponse)

-- | The response's http status code.
listDocumentMetadataHistoryResponse_httpStatus :: Lens.Lens' ListDocumentMetadataHistoryResponse Prelude.Int
listDocumentMetadataHistoryResponse_httpStatus = Lens.lens (\ListDocumentMetadataHistoryResponse' {httpStatus} -> httpStatus) (\s@ListDocumentMetadataHistoryResponse' {} a -> s {httpStatus = a} :: ListDocumentMetadataHistoryResponse)

instance
  Prelude.NFData
    ListDocumentMetadataHistoryResponse
