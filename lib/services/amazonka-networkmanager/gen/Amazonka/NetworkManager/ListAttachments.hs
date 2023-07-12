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
-- Module      : Amazonka.NetworkManager.ListAttachments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of core network attachments.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.ListAttachments
  ( -- * Creating a Request
    ListAttachments (..),
    newListAttachments,

    -- * Request Lenses
    listAttachments_attachmentType,
    listAttachments_coreNetworkId,
    listAttachments_edgeLocation,
    listAttachments_maxResults,
    listAttachments_nextToken,
    listAttachments_state,

    -- * Destructuring the Response
    ListAttachmentsResponse (..),
    newListAttachmentsResponse,

    -- * Response Lenses
    listAttachmentsResponse_attachments,
    listAttachmentsResponse_nextToken,
    listAttachmentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAttachments' smart constructor.
data ListAttachments = ListAttachments'
  { -- | The type of attachment.
    attachmentType :: Prelude.Maybe AttachmentType,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The Region where the edge is located.
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The state of the attachment.
    state :: Prelude.Maybe AttachmentState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttachments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentType', 'listAttachments_attachmentType' - The type of attachment.
--
-- 'coreNetworkId', 'listAttachments_coreNetworkId' - The ID of a core network.
--
-- 'edgeLocation', 'listAttachments_edgeLocation' - The Region where the edge is located.
--
-- 'maxResults', 'listAttachments_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listAttachments_nextToken' - The token for the next page of results.
--
-- 'state', 'listAttachments_state' - The state of the attachment.
newListAttachments ::
  ListAttachments
newListAttachments =
  ListAttachments'
    { attachmentType = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      edgeLocation = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The type of attachment.
listAttachments_attachmentType :: Lens.Lens' ListAttachments (Prelude.Maybe AttachmentType)
listAttachments_attachmentType = Lens.lens (\ListAttachments' {attachmentType} -> attachmentType) (\s@ListAttachments' {} a -> s {attachmentType = a} :: ListAttachments)

-- | The ID of a core network.
listAttachments_coreNetworkId :: Lens.Lens' ListAttachments (Prelude.Maybe Prelude.Text)
listAttachments_coreNetworkId = Lens.lens (\ListAttachments' {coreNetworkId} -> coreNetworkId) (\s@ListAttachments' {} a -> s {coreNetworkId = a} :: ListAttachments)

-- | The Region where the edge is located.
listAttachments_edgeLocation :: Lens.Lens' ListAttachments (Prelude.Maybe Prelude.Text)
listAttachments_edgeLocation = Lens.lens (\ListAttachments' {edgeLocation} -> edgeLocation) (\s@ListAttachments' {} a -> s {edgeLocation = a} :: ListAttachments)

-- | The maximum number of results to return.
listAttachments_maxResults :: Lens.Lens' ListAttachments (Prelude.Maybe Prelude.Natural)
listAttachments_maxResults = Lens.lens (\ListAttachments' {maxResults} -> maxResults) (\s@ListAttachments' {} a -> s {maxResults = a} :: ListAttachments)

-- | The token for the next page of results.
listAttachments_nextToken :: Lens.Lens' ListAttachments (Prelude.Maybe Prelude.Text)
listAttachments_nextToken = Lens.lens (\ListAttachments' {nextToken} -> nextToken) (\s@ListAttachments' {} a -> s {nextToken = a} :: ListAttachments)

-- | The state of the attachment.
listAttachments_state :: Lens.Lens' ListAttachments (Prelude.Maybe AttachmentState)
listAttachments_state = Lens.lens (\ListAttachments' {state} -> state) (\s@ListAttachments' {} a -> s {state = a} :: ListAttachments)

instance Core.AWSPager ListAttachments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttachmentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAttachmentsResponse_attachments
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAttachments_nextToken
          Lens..~ rs
          Lens.^? listAttachmentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAttachments where
  type
    AWSResponse ListAttachments =
      ListAttachmentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttachmentsResponse'
            Prelude.<$> (x Data..?> "Attachments" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttachments where
  hashWithSalt _salt ListAttachments' {..} =
    _salt
      `Prelude.hashWithSalt` attachmentType
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` state

instance Prelude.NFData ListAttachments where
  rnf ListAttachments' {..} =
    Prelude.rnf attachmentType
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf state

instance Data.ToHeaders ListAttachments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAttachments where
  toPath = Prelude.const "/attachments"

instance Data.ToQuery ListAttachments where
  toQuery ListAttachments' {..} =
    Prelude.mconcat
      [ "attachmentType" Data.=: attachmentType,
        "coreNetworkId" Data.=: coreNetworkId,
        "edgeLocation" Data.=: edgeLocation,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "state" Data.=: state
      ]

-- | /See:/ 'newListAttachmentsResponse' smart constructor.
data ListAttachmentsResponse = ListAttachmentsResponse'
  { -- | Describes the list of attachments.
    attachments :: Prelude.Maybe [Attachment],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttachmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachments', 'listAttachmentsResponse_attachments' - Describes the list of attachments.
--
-- 'nextToken', 'listAttachmentsResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'listAttachmentsResponse_httpStatus' - The response's http status code.
newListAttachmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAttachmentsResponse
newListAttachmentsResponse pHttpStatus_ =
  ListAttachmentsResponse'
    { attachments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the list of attachments.
listAttachmentsResponse_attachments :: Lens.Lens' ListAttachmentsResponse (Prelude.Maybe [Attachment])
listAttachmentsResponse_attachments = Lens.lens (\ListAttachmentsResponse' {attachments} -> attachments) (\s@ListAttachmentsResponse' {} a -> s {attachments = a} :: ListAttachmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
listAttachmentsResponse_nextToken :: Lens.Lens' ListAttachmentsResponse (Prelude.Maybe Prelude.Text)
listAttachmentsResponse_nextToken = Lens.lens (\ListAttachmentsResponse' {nextToken} -> nextToken) (\s@ListAttachmentsResponse' {} a -> s {nextToken = a} :: ListAttachmentsResponse)

-- | The response's http status code.
listAttachmentsResponse_httpStatus :: Lens.Lens' ListAttachmentsResponse Prelude.Int
listAttachmentsResponse_httpStatus = Lens.lens (\ListAttachmentsResponse' {httpStatus} -> httpStatus) (\s@ListAttachmentsResponse' {} a -> s {httpStatus = a} :: ListAttachmentsResponse)

instance Prelude.NFData ListAttachmentsResponse where
  rnf ListAttachmentsResponse' {..} =
    Prelude.rnf attachments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
