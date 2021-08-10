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
-- Module      : Network.AWS.WorkDocs.DescribeComments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all the comments for the specified document version.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeComments
  ( -- * Creating a Request
    DescribeComments (..),
    newDescribeComments,

    -- * Request Lenses
    describeComments_authenticationToken,
    describeComments_limit,
    describeComments_marker,
    describeComments_documentId,
    describeComments_versionId,

    -- * Destructuring the Response
    DescribeCommentsResponse (..),
    newDescribeCommentsResponse,

    -- * Response Lenses
    describeCommentsResponse_comments,
    describeCommentsResponse_marker,
    describeCommentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeComments' smart constructor.
data DescribeComments = DescribeComments'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maximum number of items to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The marker for the next set of results. This marker was received from a
    -- previous call.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The ID of the document.
    documentId :: Prelude.Text,
    -- | The ID of the document version.
    versionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'describeComments_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'limit', 'describeComments_limit' - The maximum number of items to return.
--
-- 'marker', 'describeComments_marker' - The marker for the next set of results. This marker was received from a
-- previous call.
--
-- 'documentId', 'describeComments_documentId' - The ID of the document.
--
-- 'versionId', 'describeComments_versionId' - The ID of the document version.
newDescribeComments ::
  -- | 'documentId'
  Prelude.Text ->
  -- | 'versionId'
  Prelude.Text ->
  DescribeComments
newDescribeComments pDocumentId_ pVersionId_ =
  DescribeComments'
    { authenticationToken =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      documentId = pDocumentId_,
      versionId = pVersionId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeComments_authenticationToken :: Lens.Lens' DescribeComments (Prelude.Maybe Prelude.Text)
describeComments_authenticationToken = Lens.lens (\DescribeComments' {authenticationToken} -> authenticationToken) (\s@DescribeComments' {} a -> s {authenticationToken = a} :: DescribeComments) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of items to return.
describeComments_limit :: Lens.Lens' DescribeComments (Prelude.Maybe Prelude.Natural)
describeComments_limit = Lens.lens (\DescribeComments' {limit} -> limit) (\s@DescribeComments' {} a -> s {limit = a} :: DescribeComments)

-- | The marker for the next set of results. This marker was received from a
-- previous call.
describeComments_marker :: Lens.Lens' DescribeComments (Prelude.Maybe Prelude.Text)
describeComments_marker = Lens.lens (\DescribeComments' {marker} -> marker) (\s@DescribeComments' {} a -> s {marker = a} :: DescribeComments)

-- | The ID of the document.
describeComments_documentId :: Lens.Lens' DescribeComments Prelude.Text
describeComments_documentId = Lens.lens (\DescribeComments' {documentId} -> documentId) (\s@DescribeComments' {} a -> s {documentId = a} :: DescribeComments)

-- | The ID of the document version.
describeComments_versionId :: Lens.Lens' DescribeComments Prelude.Text
describeComments_versionId = Lens.lens (\DescribeComments' {versionId} -> versionId) (\s@DescribeComments' {} a -> s {versionId = a} :: DescribeComments)

instance Core.AWSPager DescribeComments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCommentsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCommentsResponse_comments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeComments_marker
          Lens..~ rs
          Lens.^? describeCommentsResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest DescribeComments where
  type
    AWSResponse DescribeComments =
      DescribeCommentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCommentsResponse'
            Prelude.<$> (x Core..?> "Comments" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeComments

instance Prelude.NFData DescribeComments

instance Core.ToHeaders DescribeComments where
  toHeaders DescribeComments' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath DescribeComments where
  toPath DescribeComments' {..} =
    Prelude.mconcat
      [ "/api/v1/documents/",
        Core.toBS documentId,
        "/versions/",
        Core.toBS versionId,
        "/comments"
      ]

instance Core.ToQuery DescribeComments where
  toQuery DescribeComments' {..} =
    Prelude.mconcat
      ["limit" Core.=: limit, "marker" Core.=: marker]

-- | /See:/ 'newDescribeCommentsResponse' smart constructor.
data DescribeCommentsResponse = DescribeCommentsResponse'
  { -- | The list of comments for the specified document version.
    comments :: Prelude.Maybe [Comment],
    -- | The marker for the next set of results. This marker was received from a
    -- previous call.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCommentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comments', 'describeCommentsResponse_comments' - The list of comments for the specified document version.
--
-- 'marker', 'describeCommentsResponse_marker' - The marker for the next set of results. This marker was received from a
-- previous call.
--
-- 'httpStatus', 'describeCommentsResponse_httpStatus' - The response's http status code.
newDescribeCommentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCommentsResponse
newDescribeCommentsResponse pHttpStatus_ =
  DescribeCommentsResponse'
    { comments =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of comments for the specified document version.
describeCommentsResponse_comments :: Lens.Lens' DescribeCommentsResponse (Prelude.Maybe [Comment])
describeCommentsResponse_comments = Lens.lens (\DescribeCommentsResponse' {comments} -> comments) (\s@DescribeCommentsResponse' {} a -> s {comments = a} :: DescribeCommentsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The marker for the next set of results. This marker was received from a
-- previous call.
describeCommentsResponse_marker :: Lens.Lens' DescribeCommentsResponse (Prelude.Maybe Prelude.Text)
describeCommentsResponse_marker = Lens.lens (\DescribeCommentsResponse' {marker} -> marker) (\s@DescribeCommentsResponse' {} a -> s {marker = a} :: DescribeCommentsResponse)

-- | The response's http status code.
describeCommentsResponse_httpStatus :: Lens.Lens' DescribeCommentsResponse Prelude.Int
describeCommentsResponse_httpStatus = Lens.lens (\DescribeCommentsResponse' {httpStatus} -> httpStatus) (\s@DescribeCommentsResponse' {} a -> s {httpStatus = a} :: DescribeCommentsResponse)

instance Prelude.NFData DescribeCommentsResponse
