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
-- Module      : Network.AWS.WorkDocs.DescribeRootFolders
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current user\'s special folders; the @RootFolder@ and the
-- @RecycleBin@. @RootFolder@ is the root of user\'s files and folders and
-- @RecycleBin@ is the root of recycled items. This is not a valid action
-- for SigV4 (administrative API) clients.
--
-- This action requires an authentication token. To get an authentication
-- token, register an application with Amazon WorkDocs. For more
-- information, see
-- <https://docs.aws.amazon.com/workdocs/latest/developerguide/wd-auth-user.html Authentication and Access Control for User Applications>
-- in the /Amazon WorkDocs Developer Guide/.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeRootFolders
  ( -- * Creating a Request
    DescribeRootFolders (..),
    newDescribeRootFolders,

    -- * Request Lenses
    describeRootFolders_marker,
    describeRootFolders_limit,
    describeRootFolders_authenticationToken,

    -- * Destructuring the Response
    DescribeRootFoldersResponse (..),
    newDescribeRootFoldersResponse,

    -- * Response Lenses
    describeRootFoldersResponse_folders,
    describeRootFoldersResponse_marker,
    describeRootFoldersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeRootFolders' smart constructor.
data DescribeRootFolders = DescribeRootFolders'
  { -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Amazon WorkDocs authentication token.
    authenticationToken :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRootFolders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeRootFolders_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'limit', 'describeRootFolders_limit' - The maximum number of items to return.
--
-- 'authenticationToken', 'describeRootFolders_authenticationToken' - Amazon WorkDocs authentication token.
newDescribeRootFolders ::
  -- | 'authenticationToken'
  Prelude.Text ->
  DescribeRootFolders
newDescribeRootFolders pAuthenticationToken_ =
  DescribeRootFolders'
    { marker = Prelude.Nothing,
      limit = Prelude.Nothing,
      authenticationToken =
        Core._Sensitive Lens.# pAuthenticationToken_
    }

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeRootFolders_marker :: Lens.Lens' DescribeRootFolders (Prelude.Maybe Prelude.Text)
describeRootFolders_marker = Lens.lens (\DescribeRootFolders' {marker} -> marker) (\s@DescribeRootFolders' {} a -> s {marker = a} :: DescribeRootFolders)

-- | The maximum number of items to return.
describeRootFolders_limit :: Lens.Lens' DescribeRootFolders (Prelude.Maybe Prelude.Natural)
describeRootFolders_limit = Lens.lens (\DescribeRootFolders' {limit} -> limit) (\s@DescribeRootFolders' {} a -> s {limit = a} :: DescribeRootFolders)

-- | Amazon WorkDocs authentication token.
describeRootFolders_authenticationToken :: Lens.Lens' DescribeRootFolders Prelude.Text
describeRootFolders_authenticationToken = Lens.lens (\DescribeRootFolders' {authenticationToken} -> authenticationToken) (\s@DescribeRootFolders' {} a -> s {authenticationToken = a} :: DescribeRootFolders) Prelude.. Core._Sensitive

instance Core.AWSPager DescribeRootFolders where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRootFoldersResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeRootFoldersResponse_folders
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeRootFolders_marker
          Lens..~ rs
          Lens.^? describeRootFoldersResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeRootFolders where
  type
    AWSResponse DescribeRootFolders =
      DescribeRootFoldersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRootFoldersResponse'
            Prelude.<$> (x Core..?> "Folders" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRootFolders

instance Prelude.NFData DescribeRootFolders

instance Core.ToHeaders DescribeRootFolders where
  toHeaders DescribeRootFolders' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath DescribeRootFolders where
  toPath = Prelude.const "/api/v1/me/root"

instance Core.ToQuery DescribeRootFolders where
  toQuery DescribeRootFolders' {..} =
    Prelude.mconcat
      ["marker" Core.=: marker, "limit" Core.=: limit]

-- | /See:/ 'newDescribeRootFoldersResponse' smart constructor.
data DescribeRootFoldersResponse = DescribeRootFoldersResponse'
  { -- | The user\'s special folders.
    folders :: Prelude.Maybe [FolderMetadata],
    -- | The marker for the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRootFoldersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'folders', 'describeRootFoldersResponse_folders' - The user\'s special folders.
--
-- 'marker', 'describeRootFoldersResponse_marker' - The marker for the next set of results.
--
-- 'httpStatus', 'describeRootFoldersResponse_httpStatus' - The response's http status code.
newDescribeRootFoldersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRootFoldersResponse
newDescribeRootFoldersResponse pHttpStatus_ =
  DescribeRootFoldersResponse'
    { folders =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user\'s special folders.
describeRootFoldersResponse_folders :: Lens.Lens' DescribeRootFoldersResponse (Prelude.Maybe [FolderMetadata])
describeRootFoldersResponse_folders = Lens.lens (\DescribeRootFoldersResponse' {folders} -> folders) (\s@DescribeRootFoldersResponse' {} a -> s {folders = a} :: DescribeRootFoldersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The marker for the next set of results.
describeRootFoldersResponse_marker :: Lens.Lens' DescribeRootFoldersResponse (Prelude.Maybe Prelude.Text)
describeRootFoldersResponse_marker = Lens.lens (\DescribeRootFoldersResponse' {marker} -> marker) (\s@DescribeRootFoldersResponse' {} a -> s {marker = a} :: DescribeRootFoldersResponse)

-- | The response's http status code.
describeRootFoldersResponse_httpStatus :: Lens.Lens' DescribeRootFoldersResponse Prelude.Int
describeRootFoldersResponse_httpStatus = Lens.lens (\DescribeRootFoldersResponse' {httpStatus} -> httpStatus) (\s@DescribeRootFoldersResponse' {} a -> s {httpStatus = a} :: DescribeRootFoldersResponse)

instance Prelude.NFData DescribeRootFoldersResponse
