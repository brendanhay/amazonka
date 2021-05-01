{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkDocs.GetDocumentPath
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the path information (the hierarchy from the root folder) for
-- the requested document.
--
-- By default, Amazon WorkDocs returns a maximum of 100 levels upwards from
-- the requested document and only includes the IDs of the parent folders
-- in the path. You can limit the maximum number of levels. You can also
-- request the names of the parent folders.
module Network.AWS.WorkDocs.GetDocumentPath
  ( -- * Creating a Request
    GetDocumentPath (..),
    newGetDocumentPath,

    -- * Request Lenses
    getDocumentPath_fields,
    getDocumentPath_authenticationToken,
    getDocumentPath_limit,
    getDocumentPath_marker,
    getDocumentPath_documentId,

    -- * Destructuring the Response
    GetDocumentPathResponse (..),
    newGetDocumentPathResponse,

    -- * Response Lenses
    getDocumentPathResponse_path,
    getDocumentPathResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newGetDocumentPath' smart constructor.
data GetDocumentPath = GetDocumentPath'
  { -- | A comma-separated list of values. Specify @NAME@ to include the names of
    -- the parent folders.
    fields :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The maximum number of levels in the hierarchy to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | This value is not supported.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The ID of the document.
    documentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fields', 'getDocumentPath_fields' - A comma-separated list of values. Specify @NAME@ to include the names of
-- the parent folders.
--
-- 'authenticationToken', 'getDocumentPath_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'limit', 'getDocumentPath_limit' - The maximum number of levels in the hierarchy to return.
--
-- 'marker', 'getDocumentPath_marker' - This value is not supported.
--
-- 'documentId', 'getDocumentPath_documentId' - The ID of the document.
newGetDocumentPath ::
  -- | 'documentId'
  Prelude.Text ->
  GetDocumentPath
newGetDocumentPath pDocumentId_ =
  GetDocumentPath'
    { fields = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      documentId = pDocumentId_
    }

-- | A comma-separated list of values. Specify @NAME@ to include the names of
-- the parent folders.
getDocumentPath_fields :: Lens.Lens' GetDocumentPath (Prelude.Maybe Prelude.Text)
getDocumentPath_fields = Lens.lens (\GetDocumentPath' {fields} -> fields) (\s@GetDocumentPath' {} a -> s {fields = a} :: GetDocumentPath)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
getDocumentPath_authenticationToken :: Lens.Lens' GetDocumentPath (Prelude.Maybe Prelude.Text)
getDocumentPath_authenticationToken = Lens.lens (\GetDocumentPath' {authenticationToken} -> authenticationToken) (\s@GetDocumentPath' {} a -> s {authenticationToken = a} :: GetDocumentPath) Prelude.. Lens.mapping Prelude._Sensitive

-- | The maximum number of levels in the hierarchy to return.
getDocumentPath_limit :: Lens.Lens' GetDocumentPath (Prelude.Maybe Prelude.Natural)
getDocumentPath_limit = Lens.lens (\GetDocumentPath' {limit} -> limit) (\s@GetDocumentPath' {} a -> s {limit = a} :: GetDocumentPath)

-- | This value is not supported.
getDocumentPath_marker :: Lens.Lens' GetDocumentPath (Prelude.Maybe Prelude.Text)
getDocumentPath_marker = Lens.lens (\GetDocumentPath' {marker} -> marker) (\s@GetDocumentPath' {} a -> s {marker = a} :: GetDocumentPath)

-- | The ID of the document.
getDocumentPath_documentId :: Lens.Lens' GetDocumentPath Prelude.Text
getDocumentPath_documentId = Lens.lens (\GetDocumentPath' {documentId} -> documentId) (\s@GetDocumentPath' {} a -> s {documentId = a} :: GetDocumentPath)

instance Prelude.AWSRequest GetDocumentPath where
  type Rs GetDocumentPath = GetDocumentPathResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentPathResponse'
            Prelude.<$> (x Prelude..?> "Path")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDocumentPath

instance Prelude.NFData GetDocumentPath

instance Prelude.ToHeaders GetDocumentPath where
  toHeaders GetDocumentPath' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath GetDocumentPath where
  toPath GetDocumentPath' {..} =
    Prelude.mconcat
      [ "/api/v1/documents/",
        Prelude.toBS documentId,
        "/path"
      ]

instance Prelude.ToQuery GetDocumentPath where
  toQuery GetDocumentPath' {..} =
    Prelude.mconcat
      [ "fields" Prelude.=: fields,
        "limit" Prelude.=: limit,
        "marker" Prelude.=: marker
      ]

-- | /See:/ 'newGetDocumentPathResponse' smart constructor.
data GetDocumentPathResponse = GetDocumentPathResponse'
  { -- | The path information.
    path :: Prelude.Maybe ResourcePath,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentPathResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'getDocumentPathResponse_path' - The path information.
--
-- 'httpStatus', 'getDocumentPathResponse_httpStatus' - The response's http status code.
newGetDocumentPathResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDocumentPathResponse
newGetDocumentPathResponse pHttpStatus_ =
  GetDocumentPathResponse'
    { path = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The path information.
getDocumentPathResponse_path :: Lens.Lens' GetDocumentPathResponse (Prelude.Maybe ResourcePath)
getDocumentPathResponse_path = Lens.lens (\GetDocumentPathResponse' {path} -> path) (\s@GetDocumentPathResponse' {} a -> s {path = a} :: GetDocumentPathResponse)

-- | The response's http status code.
getDocumentPathResponse_httpStatus :: Lens.Lens' GetDocumentPathResponse Prelude.Int
getDocumentPathResponse_httpStatus = Lens.lens (\GetDocumentPathResponse' {httpStatus} -> httpStatus) (\s@GetDocumentPathResponse' {} a -> s {httpStatus = a} :: GetDocumentPathResponse)

instance Prelude.NFData GetDocumentPathResponse
