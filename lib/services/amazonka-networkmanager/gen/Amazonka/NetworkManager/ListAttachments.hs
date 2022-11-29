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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    listAttachments_coreNetworkId,
    listAttachments_nextToken,
    listAttachments_state,
    listAttachments_edgeLocation,
    listAttachments_maxResults,
    listAttachments_attachmentType,

    -- * Destructuring the Response
    ListAttachmentsResponse (..),
    newListAttachmentsResponse,

    -- * Response Lenses
    listAttachmentsResponse_nextToken,
    listAttachmentsResponse_attachments,
    listAttachmentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAttachments' smart constructor.
data ListAttachments = ListAttachments'
  { -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The state of the attachment.
    state :: Prelude.Maybe AttachmentState,
    -- | The Region where the edge is located.
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The type of attachment.
    attachmentType :: Prelude.Maybe AttachmentType
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
-- 'coreNetworkId', 'listAttachments_coreNetworkId' - The ID of a core network.
--
-- 'nextToken', 'listAttachments_nextToken' - The token for the next page of results.
--
-- 'state', 'listAttachments_state' - The state of the attachment.
--
-- 'edgeLocation', 'listAttachments_edgeLocation' - The Region where the edge is located.
--
-- 'maxResults', 'listAttachments_maxResults' - The maximum number of results to return.
--
-- 'attachmentType', 'listAttachments_attachmentType' - The type of attachment.
newListAttachments ::
  ListAttachments
newListAttachments =
  ListAttachments'
    { coreNetworkId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      state = Prelude.Nothing,
      edgeLocation = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      attachmentType = Prelude.Nothing
    }

-- | The ID of a core network.
listAttachments_coreNetworkId :: Lens.Lens' ListAttachments (Prelude.Maybe Prelude.Text)
listAttachments_coreNetworkId = Lens.lens (\ListAttachments' {coreNetworkId} -> coreNetworkId) (\s@ListAttachments' {} a -> s {coreNetworkId = a} :: ListAttachments)

-- | The token for the next page of results.
listAttachments_nextToken :: Lens.Lens' ListAttachments (Prelude.Maybe Prelude.Text)
listAttachments_nextToken = Lens.lens (\ListAttachments' {nextToken} -> nextToken) (\s@ListAttachments' {} a -> s {nextToken = a} :: ListAttachments)

-- | The state of the attachment.
listAttachments_state :: Lens.Lens' ListAttachments (Prelude.Maybe AttachmentState)
listAttachments_state = Lens.lens (\ListAttachments' {state} -> state) (\s@ListAttachments' {} a -> s {state = a} :: ListAttachments)

-- | The Region where the edge is located.
listAttachments_edgeLocation :: Lens.Lens' ListAttachments (Prelude.Maybe Prelude.Text)
listAttachments_edgeLocation = Lens.lens (\ListAttachments' {edgeLocation} -> edgeLocation) (\s@ListAttachments' {} a -> s {edgeLocation = a} :: ListAttachments)

-- | The maximum number of results to return.
listAttachments_maxResults :: Lens.Lens' ListAttachments (Prelude.Maybe Prelude.Natural)
listAttachments_maxResults = Lens.lens (\ListAttachments' {maxResults} -> maxResults) (\s@ListAttachments' {} a -> s {maxResults = a} :: ListAttachments)

-- | The type of attachment.
listAttachments_attachmentType :: Lens.Lens' ListAttachments (Prelude.Maybe AttachmentType)
listAttachments_attachmentType = Lens.lens (\ListAttachments' {attachmentType} -> attachmentType) (\s@ListAttachments' {} a -> s {attachmentType = a} :: ListAttachments)

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
      Prelude.Just Prelude.$
        rq
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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Attachments" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttachments where
  hashWithSalt _salt ListAttachments' {..} =
    _salt `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` attachmentType

instance Prelude.NFData ListAttachments where
  rnf ListAttachments' {..} =
    Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf attachmentType

instance Core.ToHeaders ListAttachments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListAttachments where
  toPath = Prelude.const "/attachments"

instance Core.ToQuery ListAttachments where
  toQuery ListAttachments' {..} =
    Prelude.mconcat
      [ "coreNetworkId" Core.=: coreNetworkId,
        "nextToken" Core.=: nextToken,
        "state" Core.=: state,
        "edgeLocation" Core.=: edgeLocation,
        "maxResults" Core.=: maxResults,
        "attachmentType" Core.=: attachmentType
      ]

-- | /See:/ 'newListAttachmentsResponse' smart constructor.
data ListAttachmentsResponse = ListAttachmentsResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Describes the list of attachments.
    attachments :: Prelude.Maybe [Attachment],
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
-- 'nextToken', 'listAttachmentsResponse_nextToken' - The token for the next page of results.
--
-- 'attachments', 'listAttachmentsResponse_attachments' - Describes the list of attachments.
--
-- 'httpStatus', 'listAttachmentsResponse_httpStatus' - The response's http status code.
newListAttachmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAttachmentsResponse
newListAttachmentsResponse pHttpStatus_ =
  ListAttachmentsResponse'
    { nextToken =
        Prelude.Nothing,
      attachments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next page of results.
listAttachmentsResponse_nextToken :: Lens.Lens' ListAttachmentsResponse (Prelude.Maybe Prelude.Text)
listAttachmentsResponse_nextToken = Lens.lens (\ListAttachmentsResponse' {nextToken} -> nextToken) (\s@ListAttachmentsResponse' {} a -> s {nextToken = a} :: ListAttachmentsResponse)

-- | Describes the list of attachments.
listAttachmentsResponse_attachments :: Lens.Lens' ListAttachmentsResponse (Prelude.Maybe [Attachment])
listAttachmentsResponse_attachments = Lens.lens (\ListAttachmentsResponse' {attachments} -> attachments) (\s@ListAttachmentsResponse' {} a -> s {attachments = a} :: ListAttachmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAttachmentsResponse_httpStatus :: Lens.Lens' ListAttachmentsResponse Prelude.Int
listAttachmentsResponse_httpStatus = Lens.lens (\ListAttachmentsResponse' {httpStatus} -> httpStatus) (\s@ListAttachmentsResponse' {} a -> s {httpStatus = a} :: ListAttachmentsResponse)

instance Prelude.NFData ListAttachmentsResponse where
  rnf ListAttachmentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf attachments
      `Prelude.seq` Prelude.rnf httpStatus
