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
-- Module      : Network.AWS.WorkDocs.GetFolderPath
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the path information (the hierarchy from the root folder) for
-- the specified folder.
--
-- By default, Amazon WorkDocs returns a maximum of 100 levels upwards from
-- the requested folder and only includes the IDs of the parent folders in
-- the path. You can limit the maximum number of levels. You can also
-- request the parent folder names.
module Network.AWS.WorkDocs.GetFolderPath
  ( -- * Creating a Request
    GetFolderPath (..),
    newGetFolderPath,

    -- * Request Lenses
    getFolderPath_fields,
    getFolderPath_authenticationToken,
    getFolderPath_limit,
    getFolderPath_marker,
    getFolderPath_folderId,

    -- * Destructuring the Response
    GetFolderPathResponse (..),
    newGetFolderPathResponse,

    -- * Response Lenses
    getFolderPathResponse_path,
    getFolderPathResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newGetFolderPath' smart constructor.
data GetFolderPath = GetFolderPath'
  { -- | A comma-separated list of values. Specify \"NAME\" to include the names
    -- of the parent folders.
    fields :: Core.Maybe Core.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The maximum number of levels in the hierarchy to return.
    limit :: Core.Maybe Core.Natural,
    -- | This value is not supported.
    marker :: Core.Maybe Core.Text,
    -- | The ID of the folder.
    folderId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetFolderPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fields', 'getFolderPath_fields' - A comma-separated list of values. Specify \"NAME\" to include the names
-- of the parent folders.
--
-- 'authenticationToken', 'getFolderPath_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'limit', 'getFolderPath_limit' - The maximum number of levels in the hierarchy to return.
--
-- 'marker', 'getFolderPath_marker' - This value is not supported.
--
-- 'folderId', 'getFolderPath_folderId' - The ID of the folder.
newGetFolderPath ::
  -- | 'folderId'
  Core.Text ->
  GetFolderPath
newGetFolderPath pFolderId_ =
  GetFolderPath'
    { fields = Core.Nothing,
      authenticationToken = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing,
      folderId = pFolderId_
    }

-- | A comma-separated list of values. Specify \"NAME\" to include the names
-- of the parent folders.
getFolderPath_fields :: Lens.Lens' GetFolderPath (Core.Maybe Core.Text)
getFolderPath_fields = Lens.lens (\GetFolderPath' {fields} -> fields) (\s@GetFolderPath' {} a -> s {fields = a} :: GetFolderPath)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
getFolderPath_authenticationToken :: Lens.Lens' GetFolderPath (Core.Maybe Core.Text)
getFolderPath_authenticationToken = Lens.lens (\GetFolderPath' {authenticationToken} -> authenticationToken) (\s@GetFolderPath' {} a -> s {authenticationToken = a} :: GetFolderPath) Core.. Lens.mapping Core._Sensitive

-- | The maximum number of levels in the hierarchy to return.
getFolderPath_limit :: Lens.Lens' GetFolderPath (Core.Maybe Core.Natural)
getFolderPath_limit = Lens.lens (\GetFolderPath' {limit} -> limit) (\s@GetFolderPath' {} a -> s {limit = a} :: GetFolderPath)

-- | This value is not supported.
getFolderPath_marker :: Lens.Lens' GetFolderPath (Core.Maybe Core.Text)
getFolderPath_marker = Lens.lens (\GetFolderPath' {marker} -> marker) (\s@GetFolderPath' {} a -> s {marker = a} :: GetFolderPath)

-- | The ID of the folder.
getFolderPath_folderId :: Lens.Lens' GetFolderPath Core.Text
getFolderPath_folderId = Lens.lens (\GetFolderPath' {folderId} -> folderId) (\s@GetFolderPath' {} a -> s {folderId = a} :: GetFolderPath)

instance Core.AWSRequest GetFolderPath where
  type
    AWSResponse GetFolderPath =
      GetFolderPathResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFolderPathResponse'
            Core.<$> (x Core..?> "Path")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetFolderPath

instance Core.NFData GetFolderPath

instance Core.ToHeaders GetFolderPath where
  toHeaders GetFolderPath' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath GetFolderPath where
  toPath GetFolderPath' {..} =
    Core.mconcat
      ["/api/v1/folders/", Core.toBS folderId, "/path"]

instance Core.ToQuery GetFolderPath where
  toQuery GetFolderPath' {..} =
    Core.mconcat
      [ "fields" Core.=: fields,
        "limit" Core.=: limit,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newGetFolderPathResponse' smart constructor.
data GetFolderPathResponse = GetFolderPathResponse'
  { -- | The path information.
    path :: Core.Maybe ResourcePath,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetFolderPathResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'getFolderPathResponse_path' - The path information.
--
-- 'httpStatus', 'getFolderPathResponse_httpStatus' - The response's http status code.
newGetFolderPathResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetFolderPathResponse
newGetFolderPathResponse pHttpStatus_ =
  GetFolderPathResponse'
    { path = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The path information.
getFolderPathResponse_path :: Lens.Lens' GetFolderPathResponse (Core.Maybe ResourcePath)
getFolderPathResponse_path = Lens.lens (\GetFolderPathResponse' {path} -> path) (\s@GetFolderPathResponse' {} a -> s {path = a} :: GetFolderPathResponse)

-- | The response's http status code.
getFolderPathResponse_httpStatus :: Lens.Lens' GetFolderPathResponse Core.Int
getFolderPathResponse_httpStatus = Lens.lens (\GetFolderPathResponse' {httpStatus} -> httpStatus) (\s@GetFolderPathResponse' {} a -> s {httpStatus = a} :: GetFolderPathResponse)

instance Core.NFData GetFolderPathResponse
