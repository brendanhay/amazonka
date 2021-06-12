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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions that the owner of an image has granted to
-- other AWS accounts for an image.
module Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions
  ( -- * Creating a Request
    DescribeWorkspaceImagePermissions (..),
    newDescribeWorkspaceImagePermissions,

    -- * Request Lenses
    describeWorkspaceImagePermissions_nextToken,
    describeWorkspaceImagePermissions_maxResults,
    describeWorkspaceImagePermissions_imageId,

    -- * Destructuring the Response
    DescribeWorkspaceImagePermissionsResponse (..),
    newDescribeWorkspaceImagePermissionsResponse,

    -- * Response Lenses
    describeWorkspaceImagePermissionsResponse_imagePermissions,
    describeWorkspaceImagePermissionsResponse_nextToken,
    describeWorkspaceImagePermissionsResponse_imageId,
    describeWorkspaceImagePermissionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspaceImagePermissions' smart constructor.
data DescribeWorkspaceImagePermissions = DescribeWorkspaceImagePermissions'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the image.
    imageId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkspaceImagePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeWorkspaceImagePermissions_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'maxResults', 'describeWorkspaceImagePermissions_maxResults' - The maximum number of items to return.
--
-- 'imageId', 'describeWorkspaceImagePermissions_imageId' - The identifier of the image.
newDescribeWorkspaceImagePermissions ::
  -- | 'imageId'
  Core.Text ->
  DescribeWorkspaceImagePermissions
newDescribeWorkspaceImagePermissions pImageId_ =
  DescribeWorkspaceImagePermissions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      imageId = pImageId_
    }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeWorkspaceImagePermissions_nextToken :: Lens.Lens' DescribeWorkspaceImagePermissions (Core.Maybe Core.Text)
describeWorkspaceImagePermissions_nextToken = Lens.lens (\DescribeWorkspaceImagePermissions' {nextToken} -> nextToken) (\s@DescribeWorkspaceImagePermissions' {} a -> s {nextToken = a} :: DescribeWorkspaceImagePermissions)

-- | The maximum number of items to return.
describeWorkspaceImagePermissions_maxResults :: Lens.Lens' DescribeWorkspaceImagePermissions (Core.Maybe Core.Natural)
describeWorkspaceImagePermissions_maxResults = Lens.lens (\DescribeWorkspaceImagePermissions' {maxResults} -> maxResults) (\s@DescribeWorkspaceImagePermissions' {} a -> s {maxResults = a} :: DescribeWorkspaceImagePermissions)

-- | The identifier of the image.
describeWorkspaceImagePermissions_imageId :: Lens.Lens' DescribeWorkspaceImagePermissions Core.Text
describeWorkspaceImagePermissions_imageId = Lens.lens (\DescribeWorkspaceImagePermissions' {imageId} -> imageId) (\s@DescribeWorkspaceImagePermissions' {} a -> s {imageId = a} :: DescribeWorkspaceImagePermissions)

instance
  Core.AWSRequest
    DescribeWorkspaceImagePermissions
  where
  type
    AWSResponse DescribeWorkspaceImagePermissions =
      DescribeWorkspaceImagePermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceImagePermissionsResponse'
            Core.<$> (x Core..?> "ImagePermissions" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ImageId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeWorkspaceImagePermissions

instance
  Core.NFData
    DescribeWorkspaceImagePermissions

instance
  Core.ToHeaders
    DescribeWorkspaceImagePermissions
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeWorkspaceImagePermissions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeWorkspaceImagePermissions
  where
  toJSON DescribeWorkspaceImagePermissions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("ImageId" Core..= imageId)
          ]
      )

instance
  Core.ToPath
    DescribeWorkspaceImagePermissions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeWorkspaceImagePermissions
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeWorkspaceImagePermissionsResponse' smart constructor.
data DescribeWorkspaceImagePermissionsResponse = DescribeWorkspaceImagePermissionsResponse'
  { -- | The identifiers of the AWS accounts that the image has been shared with.
    imagePermissions :: Core.Maybe [ImagePermission],
    -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Core.Maybe Core.Text,
    -- | The identifier of the image.
    imageId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkspaceImagePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imagePermissions', 'describeWorkspaceImagePermissionsResponse_imagePermissions' - The identifiers of the AWS accounts that the image has been shared with.
--
-- 'nextToken', 'describeWorkspaceImagePermissionsResponse_nextToken' - The token to use to retrieve the next set of results, or null if no more
-- results are available.
--
-- 'imageId', 'describeWorkspaceImagePermissionsResponse_imageId' - The identifier of the image.
--
-- 'httpStatus', 'describeWorkspaceImagePermissionsResponse_httpStatus' - The response's http status code.
newDescribeWorkspaceImagePermissionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeWorkspaceImagePermissionsResponse
newDescribeWorkspaceImagePermissionsResponse
  pHttpStatus_ =
    DescribeWorkspaceImagePermissionsResponse'
      { imagePermissions =
          Core.Nothing,
        nextToken = Core.Nothing,
        imageId = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identifiers of the AWS accounts that the image has been shared with.
describeWorkspaceImagePermissionsResponse_imagePermissions :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Core.Maybe [ImagePermission])
describeWorkspaceImagePermissionsResponse_imagePermissions = Lens.lens (\DescribeWorkspaceImagePermissionsResponse' {imagePermissions} -> imagePermissions) (\s@DescribeWorkspaceImagePermissionsResponse' {} a -> s {imagePermissions = a} :: DescribeWorkspaceImagePermissionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeWorkspaceImagePermissionsResponse_nextToken :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Core.Maybe Core.Text)
describeWorkspaceImagePermissionsResponse_nextToken = Lens.lens (\DescribeWorkspaceImagePermissionsResponse' {nextToken} -> nextToken) (\s@DescribeWorkspaceImagePermissionsResponse' {} a -> s {nextToken = a} :: DescribeWorkspaceImagePermissionsResponse)

-- | The identifier of the image.
describeWorkspaceImagePermissionsResponse_imageId :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Core.Maybe Core.Text)
describeWorkspaceImagePermissionsResponse_imageId = Lens.lens (\DescribeWorkspaceImagePermissionsResponse' {imageId} -> imageId) (\s@DescribeWorkspaceImagePermissionsResponse' {} a -> s {imageId = a} :: DescribeWorkspaceImagePermissionsResponse)

-- | The response's http status code.
describeWorkspaceImagePermissionsResponse_httpStatus :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse Core.Int
describeWorkspaceImagePermissionsResponse_httpStatus = Lens.lens (\DescribeWorkspaceImagePermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspaceImagePermissionsResponse' {} a -> s {httpStatus = a} :: DescribeWorkspaceImagePermissionsResponse)

instance
  Core.NFData
    DescribeWorkspaceImagePermissionsResponse
