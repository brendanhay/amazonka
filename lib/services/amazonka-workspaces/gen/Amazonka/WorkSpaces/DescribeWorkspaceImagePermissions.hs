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
-- Module      : Amazonka.WorkSpaces.DescribeWorkspaceImagePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions that the owner of an image has granted to
-- other Amazon Web Services accounts for an image.
module Amazonka.WorkSpaces.DescribeWorkspaceImagePermissions
  ( -- * Creating a Request
    DescribeWorkspaceImagePermissions (..),
    newDescribeWorkspaceImagePermissions,

    -- * Request Lenses
    describeWorkspaceImagePermissions_maxResults,
    describeWorkspaceImagePermissions_nextToken,
    describeWorkspaceImagePermissions_imageId,

    -- * Destructuring the Response
    DescribeWorkspaceImagePermissionsResponse (..),
    newDescribeWorkspaceImagePermissionsResponse,

    -- * Response Lenses
    describeWorkspaceImagePermissionsResponse_imageId,
    describeWorkspaceImagePermissionsResponse_imagePermissions,
    describeWorkspaceImagePermissionsResponse_nextToken,
    describeWorkspaceImagePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspaceImagePermissions' smart constructor.
data DescribeWorkspaceImagePermissions = DescribeWorkspaceImagePermissions'
  { -- | The maximum number of items to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the image.
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaceImagePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeWorkspaceImagePermissions_maxResults' - The maximum number of items to return.
--
-- 'nextToken', 'describeWorkspaceImagePermissions_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'imageId', 'describeWorkspaceImagePermissions_imageId' - The identifier of the image.
newDescribeWorkspaceImagePermissions ::
  -- | 'imageId'
  Prelude.Text ->
  DescribeWorkspaceImagePermissions
newDescribeWorkspaceImagePermissions pImageId_ =
  DescribeWorkspaceImagePermissions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      imageId = pImageId_
    }

-- | The maximum number of items to return.
describeWorkspaceImagePermissions_maxResults :: Lens.Lens' DescribeWorkspaceImagePermissions (Prelude.Maybe Prelude.Natural)
describeWorkspaceImagePermissions_maxResults = Lens.lens (\DescribeWorkspaceImagePermissions' {maxResults} -> maxResults) (\s@DescribeWorkspaceImagePermissions' {} a -> s {maxResults = a} :: DescribeWorkspaceImagePermissions)

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeWorkspaceImagePermissions_nextToken :: Lens.Lens' DescribeWorkspaceImagePermissions (Prelude.Maybe Prelude.Text)
describeWorkspaceImagePermissions_nextToken = Lens.lens (\DescribeWorkspaceImagePermissions' {nextToken} -> nextToken) (\s@DescribeWorkspaceImagePermissions' {} a -> s {nextToken = a} :: DescribeWorkspaceImagePermissions)

-- | The identifier of the image.
describeWorkspaceImagePermissions_imageId :: Lens.Lens' DescribeWorkspaceImagePermissions Prelude.Text
describeWorkspaceImagePermissions_imageId = Lens.lens (\DescribeWorkspaceImagePermissions' {imageId} -> imageId) (\s@DescribeWorkspaceImagePermissions' {} a -> s {imageId = a} :: DescribeWorkspaceImagePermissions)

instance
  Core.AWSRequest
    DescribeWorkspaceImagePermissions
  where
  type
    AWSResponse DescribeWorkspaceImagePermissions =
      DescribeWorkspaceImagePermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceImagePermissionsResponse'
            Prelude.<$> (x Data..?> "ImageId")
            Prelude.<*> ( x
                            Data..?> "ImagePermissions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeWorkspaceImagePermissions
  where
  hashWithSalt
    _salt
    DescribeWorkspaceImagePermissions' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` imageId

instance
  Prelude.NFData
    DescribeWorkspaceImagePermissions
  where
  rnf DescribeWorkspaceImagePermissions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf imageId

instance
  Data.ToHeaders
    DescribeWorkspaceImagePermissions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DescribeWorkspaceImagePermissions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeWorkspaceImagePermissions
  where
  toJSON DescribeWorkspaceImagePermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ImageId" Data..= imageId)
          ]
      )

instance
  Data.ToPath
    DescribeWorkspaceImagePermissions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeWorkspaceImagePermissions
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkspaceImagePermissionsResponse' smart constructor.
data DescribeWorkspaceImagePermissionsResponse = DescribeWorkspaceImagePermissionsResponse'
  { -- | The identifier of the image.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the Amazon Web Services accounts that the image has
    -- been shared with.
    imagePermissions :: Prelude.Maybe [ImagePermission],
    -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaceImagePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'describeWorkspaceImagePermissionsResponse_imageId' - The identifier of the image.
--
-- 'imagePermissions', 'describeWorkspaceImagePermissionsResponse_imagePermissions' - The identifiers of the Amazon Web Services accounts that the image has
-- been shared with.
--
-- 'nextToken', 'describeWorkspaceImagePermissionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'httpStatus', 'describeWorkspaceImagePermissionsResponse_httpStatus' - The response's http status code.
newDescribeWorkspaceImagePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWorkspaceImagePermissionsResponse
newDescribeWorkspaceImagePermissionsResponse
  pHttpStatus_ =
    DescribeWorkspaceImagePermissionsResponse'
      { imageId =
          Prelude.Nothing,
        imagePermissions =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identifier of the image.
describeWorkspaceImagePermissionsResponse_imageId :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Prelude.Maybe Prelude.Text)
describeWorkspaceImagePermissionsResponse_imageId = Lens.lens (\DescribeWorkspaceImagePermissionsResponse' {imageId} -> imageId) (\s@DescribeWorkspaceImagePermissionsResponse' {} a -> s {imageId = a} :: DescribeWorkspaceImagePermissionsResponse)

-- | The identifiers of the Amazon Web Services accounts that the image has
-- been shared with.
describeWorkspaceImagePermissionsResponse_imagePermissions :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Prelude.Maybe [ImagePermission])
describeWorkspaceImagePermissionsResponse_imagePermissions = Lens.lens (\DescribeWorkspaceImagePermissionsResponse' {imagePermissions} -> imagePermissions) (\s@DescribeWorkspaceImagePermissionsResponse' {} a -> s {imagePermissions = a} :: DescribeWorkspaceImagePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeWorkspaceImagePermissionsResponse_nextToken :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Prelude.Maybe Prelude.Text)
describeWorkspaceImagePermissionsResponse_nextToken = Lens.lens (\DescribeWorkspaceImagePermissionsResponse' {nextToken} -> nextToken) (\s@DescribeWorkspaceImagePermissionsResponse' {} a -> s {nextToken = a} :: DescribeWorkspaceImagePermissionsResponse)

-- | The response's http status code.
describeWorkspaceImagePermissionsResponse_httpStatus :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse Prelude.Int
describeWorkspaceImagePermissionsResponse_httpStatus = Lens.lens (\DescribeWorkspaceImagePermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspaceImagePermissionsResponse' {} a -> s {httpStatus = a} :: DescribeWorkspaceImagePermissionsResponse)

instance
  Prelude.NFData
    DescribeWorkspaceImagePermissionsResponse
  where
  rnf DescribeWorkspaceImagePermissionsResponse' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf imagePermissions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
