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
-- Module      : Network.AWS.AppStream.DescribeImagePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the permissions for shared AWS account
-- IDs on a private image that you own.
module Network.AWS.AppStream.DescribeImagePermissions
  ( -- * Creating a Request
    DescribeImagePermissions (..),
    newDescribeImagePermissions,

    -- * Request Lenses
    describeImagePermissions_nextToken,
    describeImagePermissions_maxResults,
    describeImagePermissions_sharedAwsAccountIds,
    describeImagePermissions_name,

    -- * Destructuring the Response
    DescribeImagePermissionsResponse (..),
    newDescribeImagePermissionsResponse,

    -- * Response Lenses
    describeImagePermissionsResponse_sharedImagePermissionsList,
    describeImagePermissionsResponse_nextToken,
    describeImagePermissionsResponse_name,
    describeImagePermissionsResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeImagePermissions' smart constructor.
data DescribeImagePermissions = DescribeImagePermissions'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The 12-digit identifier of one or more AWS accounts with which the image
    -- is shared.
    sharedAwsAccountIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The name of the private image for which to describe permissions. The
    -- image must be one that you own.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImagePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImagePermissions_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'maxResults', 'describeImagePermissions_maxResults' - The maximum size of each page of results.
--
-- 'sharedAwsAccountIds', 'describeImagePermissions_sharedAwsAccountIds' - The 12-digit identifier of one or more AWS accounts with which the image
-- is shared.
--
-- 'name', 'describeImagePermissions_name' - The name of the private image for which to describe permissions. The
-- image must be one that you own.
newDescribeImagePermissions ::
  -- | 'name'
  Core.Text ->
  DescribeImagePermissions
newDescribeImagePermissions pName_ =
  DescribeImagePermissions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      sharedAwsAccountIds = Core.Nothing,
      name = pName_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeImagePermissions_nextToken :: Lens.Lens' DescribeImagePermissions (Core.Maybe Core.Text)
describeImagePermissions_nextToken = Lens.lens (\DescribeImagePermissions' {nextToken} -> nextToken) (\s@DescribeImagePermissions' {} a -> s {nextToken = a} :: DescribeImagePermissions)

-- | The maximum size of each page of results.
describeImagePermissions_maxResults :: Lens.Lens' DescribeImagePermissions (Core.Maybe Core.Natural)
describeImagePermissions_maxResults = Lens.lens (\DescribeImagePermissions' {maxResults} -> maxResults) (\s@DescribeImagePermissions' {} a -> s {maxResults = a} :: DescribeImagePermissions)

-- | The 12-digit identifier of one or more AWS accounts with which the image
-- is shared.
describeImagePermissions_sharedAwsAccountIds :: Lens.Lens' DescribeImagePermissions (Core.Maybe (Core.NonEmpty Core.Text))
describeImagePermissions_sharedAwsAccountIds = Lens.lens (\DescribeImagePermissions' {sharedAwsAccountIds} -> sharedAwsAccountIds) (\s@DescribeImagePermissions' {} a -> s {sharedAwsAccountIds = a} :: DescribeImagePermissions) Core.. Lens.mapping Lens._Coerce

-- | The name of the private image for which to describe permissions. The
-- image must be one that you own.
describeImagePermissions_name :: Lens.Lens' DescribeImagePermissions Core.Text
describeImagePermissions_name = Lens.lens (\DescribeImagePermissions' {name} -> name) (\s@DescribeImagePermissions' {} a -> s {name = a} :: DescribeImagePermissions)

instance Core.AWSRequest DescribeImagePermissions where
  type
    AWSResponse DescribeImagePermissions =
      DescribeImagePermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImagePermissionsResponse'
            Core.<$> ( x Core..?> "SharedImagePermissionsList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeImagePermissions

instance Core.NFData DescribeImagePermissions

instance Core.ToHeaders DescribeImagePermissions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DescribeImagePermissions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeImagePermissions where
  toJSON DescribeImagePermissions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SharedAwsAccountIds" Core..=)
              Core.<$> sharedAwsAccountIds,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath DescribeImagePermissions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeImagePermissions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeImagePermissionsResponse' smart constructor.
data DescribeImagePermissionsResponse = DescribeImagePermissionsResponse'
  { -- | The permissions for a private image that you own.
    sharedImagePermissionsList :: Core.Maybe [SharedImagePermissions],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the private image.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImagePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharedImagePermissionsList', 'describeImagePermissionsResponse_sharedImagePermissionsList' - The permissions for a private image that you own.
--
-- 'nextToken', 'describeImagePermissionsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'name', 'describeImagePermissionsResponse_name' - The name of the private image.
--
-- 'httpStatus', 'describeImagePermissionsResponse_httpStatus' - The response's http status code.
newDescribeImagePermissionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeImagePermissionsResponse
newDescribeImagePermissionsResponse pHttpStatus_ =
  DescribeImagePermissionsResponse'
    { sharedImagePermissionsList =
        Core.Nothing,
      nextToken = Core.Nothing,
      name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The permissions for a private image that you own.
describeImagePermissionsResponse_sharedImagePermissionsList :: Lens.Lens' DescribeImagePermissionsResponse (Core.Maybe [SharedImagePermissions])
describeImagePermissionsResponse_sharedImagePermissionsList = Lens.lens (\DescribeImagePermissionsResponse' {sharedImagePermissionsList} -> sharedImagePermissionsList) (\s@DescribeImagePermissionsResponse' {} a -> s {sharedImagePermissionsList = a} :: DescribeImagePermissionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeImagePermissionsResponse_nextToken :: Lens.Lens' DescribeImagePermissionsResponse (Core.Maybe Core.Text)
describeImagePermissionsResponse_nextToken = Lens.lens (\DescribeImagePermissionsResponse' {nextToken} -> nextToken) (\s@DescribeImagePermissionsResponse' {} a -> s {nextToken = a} :: DescribeImagePermissionsResponse)

-- | The name of the private image.
describeImagePermissionsResponse_name :: Lens.Lens' DescribeImagePermissionsResponse (Core.Maybe Core.Text)
describeImagePermissionsResponse_name = Lens.lens (\DescribeImagePermissionsResponse' {name} -> name) (\s@DescribeImagePermissionsResponse' {} a -> s {name = a} :: DescribeImagePermissionsResponse)

-- | The response's http status code.
describeImagePermissionsResponse_httpStatus :: Lens.Lens' DescribeImagePermissionsResponse Core.Int
describeImagePermissionsResponse_httpStatus = Lens.lens (\DescribeImagePermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeImagePermissionsResponse' {} a -> s {httpStatus = a} :: DescribeImagePermissionsResponse)

instance Core.NFData DescribeImagePermissionsResponse
