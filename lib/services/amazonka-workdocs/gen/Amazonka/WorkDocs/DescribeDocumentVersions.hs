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
-- Module      : Amazonka.WorkDocs.DescribeDocumentVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.WorkDocs.DescribeDocumentVersions
  ( -- * Creating a Request
    DescribeDocumentVersions (..),
    newDescribeDocumentVersions,

    -- * Request Lenses
    describeDocumentVersions_authenticationToken,
    describeDocumentVersions_fields,
    describeDocumentVersions_include,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDescribeDocumentVersions' smart constructor.
data DescribeDocumentVersions = DescribeDocumentVersions'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specify \"SOURCE\" to include initialized versions and a URL for the
    -- source document.
    fields :: Prelude.Maybe Prelude.Text,
    -- | A comma-separated list of values. Specify \"INITIALIZED\" to include
    -- incomplete versions.
    include :: Prelude.Maybe Prelude.Text,
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
-- 'authenticationToken', 'describeDocumentVersions_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'fields', 'describeDocumentVersions_fields' - Specify \"SOURCE\" to include initialized versions and a URL for the
-- source document.
--
-- 'include', 'describeDocumentVersions_include' - A comma-separated list of values. Specify \"INITIALIZED\" to include
-- incomplete versions.
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
    { authenticationToken =
        Prelude.Nothing,
      fields = Prelude.Nothing,
      include = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      documentId = pDocumentId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeDocumentVersions_authenticationToken :: Lens.Lens' DescribeDocumentVersions (Prelude.Maybe Prelude.Text)
describeDocumentVersions_authenticationToken = Lens.lens (\DescribeDocumentVersions' {authenticationToken} -> authenticationToken) (\s@DescribeDocumentVersions' {} a -> s {authenticationToken = a} :: DescribeDocumentVersions) Prelude.. Lens.mapping Data._Sensitive

-- | Specify \"SOURCE\" to include initialized versions and a URL for the
-- source document.
describeDocumentVersions_fields :: Lens.Lens' DescribeDocumentVersions (Prelude.Maybe Prelude.Text)
describeDocumentVersions_fields = Lens.lens (\DescribeDocumentVersions' {fields} -> fields) (\s@DescribeDocumentVersions' {} a -> s {fields = a} :: DescribeDocumentVersions)

-- | A comma-separated list of values. Specify \"INITIALIZED\" to include
-- incomplete versions.
describeDocumentVersions_include :: Lens.Lens' DescribeDocumentVersions (Prelude.Maybe Prelude.Text)
describeDocumentVersions_include = Lens.lens (\DescribeDocumentVersions' {include} -> include) (\s@DescribeDocumentVersions' {} a -> s {include = a} :: DescribeDocumentVersions)

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentVersionsResponse'
            Prelude.<$> ( x Data..?> "DocumentVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDocumentVersions where
  hashWithSalt _salt DescribeDocumentVersions' {..} =
    _salt `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` include
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` documentId

instance Prelude.NFData DescribeDocumentVersions where
  rnf DescribeDocumentVersions' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf include
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf documentId

instance Data.ToHeaders DescribeDocumentVersions where
  toHeaders DescribeDocumentVersions' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DescribeDocumentVersions where
  toPath DescribeDocumentVersions' {..} =
    Prelude.mconcat
      [ "/api/v1/documents/",
        Data.toBS documentId,
        "/versions"
      ]

instance Data.ToQuery DescribeDocumentVersions where
  toQuery DescribeDocumentVersions' {..} =
    Prelude.mconcat
      [ "fields" Data.=: fields,
        "include" Data.=: include,
        "limit" Data.=: limit,
        "marker" Data.=: marker
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
describeDocumentVersionsResponse_documentVersions = Lens.lens (\DescribeDocumentVersionsResponse' {documentVersions} -> documentVersions) (\s@DescribeDocumentVersionsResponse' {} a -> s {documentVersions = a} :: DescribeDocumentVersionsResponse) Prelude.. Lens.mapping Lens.coerced

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
  where
  rnf DescribeDocumentVersionsResponse' {..} =
    Prelude.rnf documentVersions
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
