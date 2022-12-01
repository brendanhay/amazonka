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
-- Module      : Amazonka.EFS.DescribeAccessPoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of a specific Amazon EFS access point if the
-- @AccessPointId@ is provided. If you provide an EFS @FileSystemId@, it
-- returns descriptions of all access points for that file system. You can
-- provide either an @AccessPointId@ or a @FileSystemId@ in the request,
-- but not both.
--
-- This operation requires permissions for the
-- @elasticfilesystem:DescribeAccessPoints@ action.
module Amazonka.EFS.DescribeAccessPoints
  ( -- * Creating a Request
    DescribeAccessPoints (..),
    newDescribeAccessPoints,

    -- * Request Lenses
    describeAccessPoints_nextToken,
    describeAccessPoints_fileSystemId,
    describeAccessPoints_accessPointId,
    describeAccessPoints_maxResults,

    -- * Destructuring the Response
    DescribeAccessPointsResponse (..),
    newDescribeAccessPointsResponse,

    -- * Response Lenses
    describeAccessPointsResponse_nextToken,
    describeAccessPointsResponse_accessPoints,
    describeAccessPointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccessPoints' smart constructor.
data DescribeAccessPoints = DescribeAccessPoints'
  { -- | @NextToken@ is present if the response is paginated. You can use
    -- @NextMarker@ in the subsequent request to fetch the next page of access
    -- point descriptions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) If you provide a @FileSystemId@, EFS returns all access
    -- points for that file system; mutually exclusive with @AccessPointId@.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Specifies an EFS access point to describe in the response;
    -- mutually exclusive with @FileSystemId@.
    accessPointId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) When retrieving all access points for a file system, you can
    -- optionally specify the @MaxItems@ parameter to limit the number of
    -- objects returned in a response. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccessPoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAccessPoints_nextToken' - @NextToken@ is present if the response is paginated. You can use
-- @NextMarker@ in the subsequent request to fetch the next page of access
-- point descriptions.
--
-- 'fileSystemId', 'describeAccessPoints_fileSystemId' - (Optional) If you provide a @FileSystemId@, EFS returns all access
-- points for that file system; mutually exclusive with @AccessPointId@.
--
-- 'accessPointId', 'describeAccessPoints_accessPointId' - (Optional) Specifies an EFS access point to describe in the response;
-- mutually exclusive with @FileSystemId@.
--
-- 'maxResults', 'describeAccessPoints_maxResults' - (Optional) When retrieving all access points for a file system, you can
-- optionally specify the @MaxItems@ parameter to limit the number of
-- objects returned in a response. The default value is 100.
newDescribeAccessPoints ::
  DescribeAccessPoints
newDescribeAccessPoints =
  DescribeAccessPoints'
    { nextToken = Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      accessPointId = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | @NextToken@ is present if the response is paginated. You can use
-- @NextMarker@ in the subsequent request to fetch the next page of access
-- point descriptions.
describeAccessPoints_nextToken :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Text)
describeAccessPoints_nextToken = Lens.lens (\DescribeAccessPoints' {nextToken} -> nextToken) (\s@DescribeAccessPoints' {} a -> s {nextToken = a} :: DescribeAccessPoints)

-- | (Optional) If you provide a @FileSystemId@, EFS returns all access
-- points for that file system; mutually exclusive with @AccessPointId@.
describeAccessPoints_fileSystemId :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Text)
describeAccessPoints_fileSystemId = Lens.lens (\DescribeAccessPoints' {fileSystemId} -> fileSystemId) (\s@DescribeAccessPoints' {} a -> s {fileSystemId = a} :: DescribeAccessPoints)

-- | (Optional) Specifies an EFS access point to describe in the response;
-- mutually exclusive with @FileSystemId@.
describeAccessPoints_accessPointId :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Text)
describeAccessPoints_accessPointId = Lens.lens (\DescribeAccessPoints' {accessPointId} -> accessPointId) (\s@DescribeAccessPoints' {} a -> s {accessPointId = a} :: DescribeAccessPoints)

-- | (Optional) When retrieving all access points for a file system, you can
-- optionally specify the @MaxItems@ parameter to limit the number of
-- objects returned in a response. The default value is 100.
describeAccessPoints_maxResults :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Natural)
describeAccessPoints_maxResults = Lens.lens (\DescribeAccessPoints' {maxResults} -> maxResults) (\s@DescribeAccessPoints' {} a -> s {maxResults = a} :: DescribeAccessPoints)

instance Core.AWSRequest DescribeAccessPoints where
  type
    AWSResponse DescribeAccessPoints =
      DescribeAccessPointsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccessPointsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "AccessPoints" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccessPoints where
  hashWithSalt _salt DescribeAccessPoints' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` accessPointId
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeAccessPoints where
  rnf DescribeAccessPoints' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf accessPointId
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeAccessPoints where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeAccessPoints where
  toPath = Prelude.const "/2015-02-01/access-points"

instance Core.ToQuery DescribeAccessPoints where
  toQuery DescribeAccessPoints' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "FileSystemId" Core.=: fileSystemId,
        "AccessPointId" Core.=: accessPointId,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeAccessPointsResponse' smart constructor.
data DescribeAccessPointsResponse = DescribeAccessPointsResponse'
  { -- | Present if there are more access points than returned in the response.
    -- You can use the NextMarker in the subsequent request to fetch the
    -- additional descriptions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of access point descriptions.
    accessPoints :: Prelude.Maybe [AccessPointDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccessPointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAccessPointsResponse_nextToken' - Present if there are more access points than returned in the response.
-- You can use the NextMarker in the subsequent request to fetch the
-- additional descriptions.
--
-- 'accessPoints', 'describeAccessPointsResponse_accessPoints' - An array of access point descriptions.
--
-- 'httpStatus', 'describeAccessPointsResponse_httpStatus' - The response's http status code.
newDescribeAccessPointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccessPointsResponse
newDescribeAccessPointsResponse pHttpStatus_ =
  DescribeAccessPointsResponse'
    { nextToken =
        Prelude.Nothing,
      accessPoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Present if there are more access points than returned in the response.
-- You can use the NextMarker in the subsequent request to fetch the
-- additional descriptions.
describeAccessPointsResponse_nextToken :: Lens.Lens' DescribeAccessPointsResponse (Prelude.Maybe Prelude.Text)
describeAccessPointsResponse_nextToken = Lens.lens (\DescribeAccessPointsResponse' {nextToken} -> nextToken) (\s@DescribeAccessPointsResponse' {} a -> s {nextToken = a} :: DescribeAccessPointsResponse)

-- | An array of access point descriptions.
describeAccessPointsResponse_accessPoints :: Lens.Lens' DescribeAccessPointsResponse (Prelude.Maybe [AccessPointDescription])
describeAccessPointsResponse_accessPoints = Lens.lens (\DescribeAccessPointsResponse' {accessPoints} -> accessPoints) (\s@DescribeAccessPointsResponse' {} a -> s {accessPoints = a} :: DescribeAccessPointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAccessPointsResponse_httpStatus :: Lens.Lens' DescribeAccessPointsResponse Prelude.Int
describeAccessPointsResponse_httpStatus = Lens.lens (\DescribeAccessPointsResponse' {httpStatus} -> httpStatus) (\s@DescribeAccessPointsResponse' {} a -> s {httpStatus = a} :: DescribeAccessPointsResponse)

instance Prelude.NFData DescribeAccessPointsResponse where
  rnf DescribeAccessPointsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accessPoints
      `Prelude.seq` Prelude.rnf httpStatus
