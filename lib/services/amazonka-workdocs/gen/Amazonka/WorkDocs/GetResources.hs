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
-- Module      : Amazonka.WorkDocs.GetResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a collection of resources, including folders and documents.
-- The only @CollectionType@ supported is @SHARED_WITH_ME@.
module Amazonka.WorkDocs.GetResources
  ( -- * Creating a Request
    GetResources (..),
    newGetResources,

    -- * Request Lenses
    getResources_authenticationToken,
    getResources_userId,
    getResources_marker,
    getResources_limit,
    getResources_collectionType,

    -- * Destructuring the Response
    GetResourcesResponse (..),
    newGetResourcesResponse,

    -- * Response Lenses
    getResourcesResponse_folders,
    getResourcesResponse_documents,
    getResourcesResponse_marker,
    getResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newGetResources' smart constructor.
data GetResources = GetResources'
  { -- | The Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The user ID for the resource collection. This is a required field for
    -- accessing the API operation using IAM credentials.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The marker for the next set of results. This marker was received from a
    -- previous call.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of resources to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The collection type.
    collectionType :: Prelude.Maybe ResourceCollectionType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'getResources_authenticationToken' - The Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'userId', 'getResources_userId' - The user ID for the resource collection. This is a required field for
-- accessing the API operation using IAM credentials.
--
-- 'marker', 'getResources_marker' - The marker for the next set of results. This marker was received from a
-- previous call.
--
-- 'limit', 'getResources_limit' - The maximum number of resources to return.
--
-- 'collectionType', 'getResources_collectionType' - The collection type.
newGetResources ::
  GetResources
newGetResources =
  GetResources'
    { authenticationToken =
        Prelude.Nothing,
      userId = Prelude.Nothing,
      marker = Prelude.Nothing,
      limit = Prelude.Nothing,
      collectionType = Prelude.Nothing
    }

-- | The Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
getResources_authenticationToken :: Lens.Lens' GetResources (Prelude.Maybe Prelude.Text)
getResources_authenticationToken = Lens.lens (\GetResources' {authenticationToken} -> authenticationToken) (\s@GetResources' {} a -> s {authenticationToken = a} :: GetResources) Prelude.. Lens.mapping Core._Sensitive

-- | The user ID for the resource collection. This is a required field for
-- accessing the API operation using IAM credentials.
getResources_userId :: Lens.Lens' GetResources (Prelude.Maybe Prelude.Text)
getResources_userId = Lens.lens (\GetResources' {userId} -> userId) (\s@GetResources' {} a -> s {userId = a} :: GetResources)

-- | The marker for the next set of results. This marker was received from a
-- previous call.
getResources_marker :: Lens.Lens' GetResources (Prelude.Maybe Prelude.Text)
getResources_marker = Lens.lens (\GetResources' {marker} -> marker) (\s@GetResources' {} a -> s {marker = a} :: GetResources)

-- | The maximum number of resources to return.
getResources_limit :: Lens.Lens' GetResources (Prelude.Maybe Prelude.Natural)
getResources_limit = Lens.lens (\GetResources' {limit} -> limit) (\s@GetResources' {} a -> s {limit = a} :: GetResources)

-- | The collection type.
getResources_collectionType :: Lens.Lens' GetResources (Prelude.Maybe ResourceCollectionType)
getResources_collectionType = Lens.lens (\GetResources' {collectionType} -> collectionType) (\s@GetResources' {} a -> s {collectionType = a} :: GetResources)

instance Core.AWSRequest GetResources where
  type AWSResponse GetResources = GetResourcesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcesResponse'
            Prelude.<$> (x Core..?> "Folders" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Documents" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResources

instance Prelude.NFData GetResources

instance Core.ToHeaders GetResources where
  toHeaders GetResources' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath GetResources where
  toPath = Prelude.const "/api/v1/resources"

instance Core.ToQuery GetResources where
  toQuery GetResources' {..} =
    Prelude.mconcat
      [ "userId" Core.=: userId,
        "marker" Core.=: marker,
        "limit" Core.=: limit,
        "collectionType" Core.=: collectionType
      ]

-- | /See:/ 'newGetResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { -- | The folders in the specified folder.
    folders :: Prelude.Maybe [FolderMetadata],
    -- | The documents in the specified collection.
    documents :: Prelude.Maybe [DocumentMetadata],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'folders', 'getResourcesResponse_folders' - The folders in the specified folder.
--
-- 'documents', 'getResourcesResponse_documents' - The documents in the specified collection.
--
-- 'marker', 'getResourcesResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'httpStatus', 'getResourcesResponse_httpStatus' - The response's http status code.
newGetResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcesResponse
newGetResourcesResponse pHttpStatus_ =
  GetResourcesResponse'
    { folders = Prelude.Nothing,
      documents = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The folders in the specified folder.
getResourcesResponse_folders :: Lens.Lens' GetResourcesResponse (Prelude.Maybe [FolderMetadata])
getResourcesResponse_folders = Lens.lens (\GetResourcesResponse' {folders} -> folders) (\s@GetResourcesResponse' {} a -> s {folders = a} :: GetResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The documents in the specified collection.
getResourcesResponse_documents :: Lens.Lens' GetResourcesResponse (Prelude.Maybe [DocumentMetadata])
getResourcesResponse_documents = Lens.lens (\GetResourcesResponse' {documents} -> documents) (\s@GetResourcesResponse' {} a -> s {documents = a} :: GetResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
getResourcesResponse_marker :: Lens.Lens' GetResourcesResponse (Prelude.Maybe Prelude.Text)
getResourcesResponse_marker = Lens.lens (\GetResourcesResponse' {marker} -> marker) (\s@GetResourcesResponse' {} a -> s {marker = a} :: GetResourcesResponse)

-- | The response's http status code.
getResourcesResponse_httpStatus :: Lens.Lens' GetResourcesResponse Prelude.Int
getResourcesResponse_httpStatus = Lens.lens (\GetResourcesResponse' {httpStatus} -> httpStatus) (\s@GetResourcesResponse' {} a -> s {httpStatus = a} :: GetResourcesResponse)

instance Prelude.NFData GetResourcesResponse
