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
-- Module      : Network.AWS.EFS.DescribeFileSystems
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of a specific Amazon EFS file system if either
-- the file system @CreationToken@ or the @FileSystemId@ is provided.
-- Otherwise, it returns descriptions of all file systems owned by the
-- caller\'s AWS account in the AWS Region of the endpoint that you\'re
-- calling.
--
-- When retrieving all file system descriptions, you can optionally specify
-- the @MaxItems@ parameter to limit the number of descriptions in a
-- response. Currently, this number is automatically set to 10. If more
-- file system descriptions remain, Amazon EFS returns a @NextMarker@, an
-- opaque token, in the response. In this case, you should send a
-- subsequent request with the @Marker@ request parameter set to the value
-- of @NextMarker@.
--
-- To retrieve a list of your file system descriptions, this operation is
-- used in an iterative process, where @DescribeFileSystems@ is called
-- first without the @Marker@ and then the operation continues to call it
-- with the @Marker@ parameter set to the value of the @NextMarker@ from
-- the previous response until the response has no @NextMarker@.
--
-- The order of file systems returned in the response of one
-- @DescribeFileSystems@ call and the order of file systems returned across
-- the responses of a multi-call iteration is unspecified.
--
-- This operation requires permissions for the
-- @elasticfilesystem:DescribeFileSystems@ action.
--
-- This operation returns paginated results.
module Network.AWS.EFS.DescribeFileSystems
  ( -- * Creating a Request
    DescribeFileSystems (..),
    newDescribeFileSystems,

    -- * Request Lenses
    describeFileSystems_creationToken,
    describeFileSystems_fileSystemId,
    describeFileSystems_maxItems,
    describeFileSystems_marker,

    -- * Destructuring the Response
    DescribeFileSystemsResponse (..),
    newDescribeFileSystemsResponse,

    -- * Response Lenses
    describeFileSystemsResponse_nextMarker,
    describeFileSystemsResponse_fileSystems,
    describeFileSystemsResponse_marker,
    describeFileSystemsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeFileSystems' smart constructor.
data DescribeFileSystems = DescribeFileSystems'
  { -- | (Optional) Restricts the list to the file system with this creation
    -- token (String). You specify a creation token when you create an Amazon
    -- EFS file system.
    creationToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) ID of the file system whose description you want to retrieve
    -- (String).
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Specifies the maximum number of file systems to return in the
    -- response (integer). This number is automatically set to 100. The
    -- response is paginated at 100 per page if you have more than 100 file
    -- systems.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) Opaque pagination token returned from a previous
    -- @DescribeFileSystems@ operation (String). If present, specifies to
    -- continue the list from where the returning call had left off.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFileSystems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationToken', 'describeFileSystems_creationToken' - (Optional) Restricts the list to the file system with this creation
-- token (String). You specify a creation token when you create an Amazon
-- EFS file system.
--
-- 'fileSystemId', 'describeFileSystems_fileSystemId' - (Optional) ID of the file system whose description you want to retrieve
-- (String).
--
-- 'maxItems', 'describeFileSystems_maxItems' - (Optional) Specifies the maximum number of file systems to return in the
-- response (integer). This number is automatically set to 100. The
-- response is paginated at 100 per page if you have more than 100 file
-- systems.
--
-- 'marker', 'describeFileSystems_marker' - (Optional) Opaque pagination token returned from a previous
-- @DescribeFileSystems@ operation (String). If present, specifies to
-- continue the list from where the returning call had left off.
newDescribeFileSystems ::
  DescribeFileSystems
newDescribeFileSystems =
  DescribeFileSystems'
    { creationToken =
        Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | (Optional) Restricts the list to the file system with this creation
-- token (String). You specify a creation token when you create an Amazon
-- EFS file system.
describeFileSystems_creationToken :: Lens.Lens' DescribeFileSystems (Prelude.Maybe Prelude.Text)
describeFileSystems_creationToken = Lens.lens (\DescribeFileSystems' {creationToken} -> creationToken) (\s@DescribeFileSystems' {} a -> s {creationToken = a} :: DescribeFileSystems)

-- | (Optional) ID of the file system whose description you want to retrieve
-- (String).
describeFileSystems_fileSystemId :: Lens.Lens' DescribeFileSystems (Prelude.Maybe Prelude.Text)
describeFileSystems_fileSystemId = Lens.lens (\DescribeFileSystems' {fileSystemId} -> fileSystemId) (\s@DescribeFileSystems' {} a -> s {fileSystemId = a} :: DescribeFileSystems)

-- | (Optional) Specifies the maximum number of file systems to return in the
-- response (integer). This number is automatically set to 100. The
-- response is paginated at 100 per page if you have more than 100 file
-- systems.
describeFileSystems_maxItems :: Lens.Lens' DescribeFileSystems (Prelude.Maybe Prelude.Natural)
describeFileSystems_maxItems = Lens.lens (\DescribeFileSystems' {maxItems} -> maxItems) (\s@DescribeFileSystems' {} a -> s {maxItems = a} :: DescribeFileSystems)

-- | (Optional) Opaque pagination token returned from a previous
-- @DescribeFileSystems@ operation (String). If present, specifies to
-- continue the list from where the returning call had left off.
describeFileSystems_marker :: Lens.Lens' DescribeFileSystems (Prelude.Maybe Prelude.Text)
describeFileSystems_marker = Lens.lens (\DescribeFileSystems' {marker} -> marker) (\s@DescribeFileSystems' {} a -> s {marker = a} :: DescribeFileSystems)

instance Core.AWSPager DescribeFileSystems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFileSystemsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFileSystemsResponse_fileSystems
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeFileSystems_marker
          Lens..~ rs
          Lens.^? describeFileSystemsResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeFileSystems where
  type
    AWSResponse DescribeFileSystems =
      DescribeFileSystemsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFileSystemsResponse'
            Prelude.<$> (x Core..?> "NextMarker")
            Prelude.<*> (x Core..?> "FileSystems" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFileSystems

instance Prelude.NFData DescribeFileSystems

instance Core.ToHeaders DescribeFileSystems where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeFileSystems where
  toPath = Prelude.const "/2015-02-01/file-systems"

instance Core.ToQuery DescribeFileSystems where
  toQuery DescribeFileSystems' {..} =
    Prelude.mconcat
      [ "CreationToken" Core.=: creationToken,
        "FileSystemId" Core.=: fileSystemId,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeFileSystemsResponse' smart constructor.
data DescribeFileSystemsResponse = DescribeFileSystemsResponse'
  { -- | Present if there are more file systems than returned in the response
    -- (String). You can use the @NextMarker@ in the subsequent request to
    -- fetch the descriptions.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | An array of file system descriptions.
    fileSystems :: Prelude.Maybe [FileSystemDescription],
    -- | Present if provided by caller in the request (String).
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFileSystemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'describeFileSystemsResponse_nextMarker' - Present if there are more file systems than returned in the response
-- (String). You can use the @NextMarker@ in the subsequent request to
-- fetch the descriptions.
--
-- 'fileSystems', 'describeFileSystemsResponse_fileSystems' - An array of file system descriptions.
--
-- 'marker', 'describeFileSystemsResponse_marker' - Present if provided by caller in the request (String).
--
-- 'httpStatus', 'describeFileSystemsResponse_httpStatus' - The response's http status code.
newDescribeFileSystemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFileSystemsResponse
newDescribeFileSystemsResponse pHttpStatus_ =
  DescribeFileSystemsResponse'
    { nextMarker =
        Prelude.Nothing,
      fileSystems = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Present if there are more file systems than returned in the response
-- (String). You can use the @NextMarker@ in the subsequent request to
-- fetch the descriptions.
describeFileSystemsResponse_nextMarker :: Lens.Lens' DescribeFileSystemsResponse (Prelude.Maybe Prelude.Text)
describeFileSystemsResponse_nextMarker = Lens.lens (\DescribeFileSystemsResponse' {nextMarker} -> nextMarker) (\s@DescribeFileSystemsResponse' {} a -> s {nextMarker = a} :: DescribeFileSystemsResponse)

-- | An array of file system descriptions.
describeFileSystemsResponse_fileSystems :: Lens.Lens' DescribeFileSystemsResponse (Prelude.Maybe [FileSystemDescription])
describeFileSystemsResponse_fileSystems = Lens.lens (\DescribeFileSystemsResponse' {fileSystems} -> fileSystems) (\s@DescribeFileSystemsResponse' {} a -> s {fileSystems = a} :: DescribeFileSystemsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Present if provided by caller in the request (String).
describeFileSystemsResponse_marker :: Lens.Lens' DescribeFileSystemsResponse (Prelude.Maybe Prelude.Text)
describeFileSystemsResponse_marker = Lens.lens (\DescribeFileSystemsResponse' {marker} -> marker) (\s@DescribeFileSystemsResponse' {} a -> s {marker = a} :: DescribeFileSystemsResponse)

-- | The response's http status code.
describeFileSystemsResponse_httpStatus :: Lens.Lens' DescribeFileSystemsResponse Prelude.Int
describeFileSystemsResponse_httpStatus = Lens.lens (\DescribeFileSystemsResponse' {httpStatus} -> httpStatus) (\s@DescribeFileSystemsResponse' {} a -> s {httpStatus = a} :: DescribeFileSystemsResponse)

instance Prelude.NFData DescribeFileSystemsResponse
