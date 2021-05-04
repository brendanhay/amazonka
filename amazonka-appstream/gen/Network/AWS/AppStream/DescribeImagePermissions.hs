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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeImagePermissions' smart constructor.
data DescribeImagePermissions = DescribeImagePermissions'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The 12-digit identifier of one or more AWS accounts with which the image
    -- is shared.
    sharedAwsAccountIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the private image for which to describe permissions. The
    -- image must be one that you own.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeImagePermissions
newDescribeImagePermissions pName_ =
  DescribeImagePermissions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sharedAwsAccountIds = Prelude.Nothing,
      name = pName_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeImagePermissions_nextToken :: Lens.Lens' DescribeImagePermissions (Prelude.Maybe Prelude.Text)
describeImagePermissions_nextToken = Lens.lens (\DescribeImagePermissions' {nextToken} -> nextToken) (\s@DescribeImagePermissions' {} a -> s {nextToken = a} :: DescribeImagePermissions)

-- | The maximum size of each page of results.
describeImagePermissions_maxResults :: Lens.Lens' DescribeImagePermissions (Prelude.Maybe Prelude.Natural)
describeImagePermissions_maxResults = Lens.lens (\DescribeImagePermissions' {maxResults} -> maxResults) (\s@DescribeImagePermissions' {} a -> s {maxResults = a} :: DescribeImagePermissions)

-- | The 12-digit identifier of one or more AWS accounts with which the image
-- is shared.
describeImagePermissions_sharedAwsAccountIds :: Lens.Lens' DescribeImagePermissions (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeImagePermissions_sharedAwsAccountIds = Lens.lens (\DescribeImagePermissions' {sharedAwsAccountIds} -> sharedAwsAccountIds) (\s@DescribeImagePermissions' {} a -> s {sharedAwsAccountIds = a} :: DescribeImagePermissions) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the private image for which to describe permissions. The
-- image must be one that you own.
describeImagePermissions_name :: Lens.Lens' DescribeImagePermissions Prelude.Text
describeImagePermissions_name = Lens.lens (\DescribeImagePermissions' {name} -> name) (\s@DescribeImagePermissions' {} a -> s {name = a} :: DescribeImagePermissions)

instance Prelude.AWSRequest DescribeImagePermissions where
  type
    Rs DescribeImagePermissions =
      DescribeImagePermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImagePermissionsResponse'
            Prelude.<$> ( x Prelude..?> "SharedImagePermissionsList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImagePermissions

instance Prelude.NFData DescribeImagePermissions

instance Prelude.ToHeaders DescribeImagePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.DescribeImagePermissions" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeImagePermissions where
  toJSON DescribeImagePermissions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("SharedAwsAccountIds" Prelude..=)
              Prelude.<$> sharedAwsAccountIds,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath DescribeImagePermissions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeImagePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImagePermissionsResponse' smart constructor.
data DescribeImagePermissionsResponse = DescribeImagePermissionsResponse'
  { -- | The permissions for a private image that you own.
    sharedImagePermissionsList :: Prelude.Maybe [SharedImagePermissions],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the private image.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeImagePermissionsResponse
newDescribeImagePermissionsResponse pHttpStatus_ =
  DescribeImagePermissionsResponse'
    { sharedImagePermissionsList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The permissions for a private image that you own.
describeImagePermissionsResponse_sharedImagePermissionsList :: Lens.Lens' DescribeImagePermissionsResponse (Prelude.Maybe [SharedImagePermissions])
describeImagePermissionsResponse_sharedImagePermissionsList = Lens.lens (\DescribeImagePermissionsResponse' {sharedImagePermissionsList} -> sharedImagePermissionsList) (\s@DescribeImagePermissionsResponse' {} a -> s {sharedImagePermissionsList = a} :: DescribeImagePermissionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeImagePermissionsResponse_nextToken :: Lens.Lens' DescribeImagePermissionsResponse (Prelude.Maybe Prelude.Text)
describeImagePermissionsResponse_nextToken = Lens.lens (\DescribeImagePermissionsResponse' {nextToken} -> nextToken) (\s@DescribeImagePermissionsResponse' {} a -> s {nextToken = a} :: DescribeImagePermissionsResponse)

-- | The name of the private image.
describeImagePermissionsResponse_name :: Lens.Lens' DescribeImagePermissionsResponse (Prelude.Maybe Prelude.Text)
describeImagePermissionsResponse_name = Lens.lens (\DescribeImagePermissionsResponse' {name} -> name) (\s@DescribeImagePermissionsResponse' {} a -> s {name = a} :: DescribeImagePermissionsResponse)

-- | The response's http status code.
describeImagePermissionsResponse_httpStatus :: Lens.Lens' DescribeImagePermissionsResponse Prelude.Int
describeImagePermissionsResponse_httpStatus = Lens.lens (\DescribeImagePermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeImagePermissionsResponse' {} a -> s {httpStatus = a} :: DescribeImagePermissionsResponse)

instance
  Prelude.NFData
    DescribeImagePermissionsResponse
