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
-- Module      : Network.AWS.WorkDocs.DescribeDocumentVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the document versions for the specified document.
--
-- By default, only active versions are returned.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeDocumentVersions
  ( -- * Creating a Request
    DescribeDocumentVersions (..),
    newDescribeDocumentVersions,

    -- * Request Lenses
    describeDocumentVersions_include,
    describeDocumentVersions_fields,
    describeDocumentVersions_authenticationToken,
    describeDocumentVersions_limit,
    describeDocumentVersions_marker,
    describeDocumentVersions_documentId,

    -- * Destructuring the Response
    DescribeDocumentVersionsResponse (..),
    newDescribeDocumentVersionsResponse,

    -- * Response Lenses
    describeDocumentVersionsResponse_documentVersions,
    describeDocumentVersionsResponse_marker,
    describeDocumentVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeDocumentVersions' smart constructor.
data DescribeDocumentVersions = DescribeDocumentVersions'
  { -- | A comma-separated list of values. Specify \"INITIALIZED\" to include
    -- incomplete versions.
    include :: Prelude.Maybe Prelude.Text,
    -- | Specify \"SOURCE\" to include initialized versions and a URL for the
    -- source document.
    fields :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maximum number of versions to return with this call.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The ID of the document.
    documentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDocumentVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'include', 'describeDocumentVersions_include' - A comma-separated list of values. Specify \"INITIALIZED\" to include
-- incomplete versions.
--
-- 'fields', 'describeDocumentVersions_fields' - Specify \"SOURCE\" to include initialized versions and a URL for the
-- source document.
--
-- 'authenticationToken', 'describeDocumentVersions_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'limit', 'describeDocumentVersions_limit' - The maximum number of versions to return with this call.
--
-- 'marker', 'describeDocumentVersions_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'documentId', 'describeDocumentVersions_documentId' - The ID of the document.
newDescribeDocumentVersions ::
  -- | 'documentId'
  Prelude.Text ->
  DescribeDocumentVersions
newDescribeDocumentVersions pDocumentId_ =
  DescribeDocumentVersions'
    { include =
        Prelude.Nothing,
      fields = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      documentId = pDocumentId_
    }

-- | A comma-separated list of values. Specify \"INITIALIZED\" to include
-- incomplete versions.
describeDocumentVersions_include :: Lens.Lens' DescribeDocumentVersions (Prelude.Maybe Prelude.Text)
describeDocumentVersions_include = Lens.lens (\DescribeDocumentVersions' {include} -> include) (\s@DescribeDocumentVersions' {} a -> s {include = a} :: DescribeDocumentVersions)

-- | Specify \"SOURCE\" to include initialized versions and a URL for the
-- source document.
describeDocumentVersions_fields :: Lens.Lens' DescribeDocumentVersions (Prelude.Maybe Prelude.Text)
describeDocumentVersions_fields = Lens.lens (\DescribeDocumentVersions' {fields} -> fields) (\s@DescribeDocumentVersions' {} a -> s {fields = a} :: DescribeDocumentVersions)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeDocumentVersions_authenticationToken :: Lens.Lens' DescribeDocumentVersions (Prelude.Maybe Prelude.Text)
describeDocumentVersions_authenticationToken = Lens.lens (\DescribeDocumentVersions' {authenticationToken} -> authenticationToken) (\s@DescribeDocumentVersions' {} a -> s {authenticationToken = a} :: DescribeDocumentVersions) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of versions to return with this call.
describeDocumentVersions_limit :: Lens.Lens' DescribeDocumentVersions (Prelude.Maybe Prelude.Natural)
describeDocumentVersions_limit = Lens.lens (\DescribeDocumentVersions' {limit} -> limit) (\s@DescribeDocumentVersions' {} a -> s {limit = a} :: DescribeDocumentVersions)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeDocumentVersions_marker :: Lens.Lens' DescribeDocumentVersions (Prelude.Maybe Prelude.Text)
describeDocumentVersions_marker = Lens.lens (\DescribeDocumentVersions' {marker} -> marker) (\s@DescribeDocumentVersions' {} a -> s {marker = a} :: DescribeDocumentVersions)

-- | The ID of the document.
describeDocumentVersions_documentId :: Lens.Lens' DescribeDocumentVersions Prelude.Text
describeDocumentVersions_documentId = Lens.lens (\DescribeDocumentVersions' {documentId} -> documentId) (\s@DescribeDocumentVersions' {} a -> s {documentId = a} :: DescribeDocumentVersions)

instance Core.AWSPager DescribeDocumentVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDocumentVersionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDocumentVersionsResponse_documentVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDocumentVersions_marker
          Lens..~ rs
          Lens.^? describeDocumentVersionsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDocumentVersions where
  type
    AWSResponse DescribeDocumentVersions =
      DescribeDocumentVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentVersionsResponse'
            Prelude.<$> ( x Core..?> "DocumentVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDocumentVersions

instance Prelude.NFData DescribeDocumentVersions

instance Core.ToHeaders DescribeDocumentVersions where
  toHeaders DescribeDocumentVersions' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath DescribeDocumentVersions where
  toPath DescribeDocumentVersions' {..} =
    Prelude.mconcat
      [ "/api/v1/documents/",
        Core.toBS documentId,
        "/versions"
      ]

instance Core.ToQuery DescribeDocumentVersions where
  toQuery DescribeDocumentVersions' {..} =
    Prelude.mconcat
      [ "include" Core.=: include,
        "fields" Core.=: fields,
        "limit" Core.=: limit,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeDocumentVersionsResponse' smart constructor.
data DescribeDocumentVersionsResponse = DescribeDocumentVersionsResponse'
  { -- | The document versions.
    documentVersions :: Prelude.Maybe [DocumentVersionMetadata],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDocumentVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentVersions', 'describeDocumentVersionsResponse_documentVersions' - The document versions.
--
-- 'marker', 'describeDocumentVersionsResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'httpStatus', 'describeDocumentVersionsResponse_httpStatus' - The response's http status code.
newDescribeDocumentVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDocumentVersionsResponse
newDescribeDocumentVersionsResponse pHttpStatus_ =
  DescribeDocumentVersionsResponse'
    { documentVersions =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The document versions.
describeDocumentVersionsResponse_documentVersions :: Lens.Lens' DescribeDocumentVersionsResponse (Prelude.Maybe [DocumentVersionMetadata])
describeDocumentVersionsResponse_documentVersions = Lens.lens (\DescribeDocumentVersionsResponse' {documentVersions} -> documentVersions) (\s@DescribeDocumentVersionsResponse' {} a -> s {documentVersions = a} :: DescribeDocumentVersionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeDocumentVersionsResponse_marker :: Lens.Lens' DescribeDocumentVersionsResponse (Prelude.Maybe Prelude.Text)
describeDocumentVersionsResponse_marker = Lens.lens (\DescribeDocumentVersionsResponse' {marker} -> marker) (\s@DescribeDocumentVersionsResponse' {} a -> s {marker = a} :: DescribeDocumentVersionsResponse)

-- | The response's http status code.
describeDocumentVersionsResponse_httpStatus :: Lens.Lens' DescribeDocumentVersionsResponse Prelude.Int
describeDocumentVersionsResponse_httpStatus = Lens.lens (\DescribeDocumentVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeDocumentVersionsResponse' {} a -> s {httpStatus = a} :: DescribeDocumentVersionsResponse)

instance
  Prelude.NFData
    DescribeDocumentVersionsResponse
