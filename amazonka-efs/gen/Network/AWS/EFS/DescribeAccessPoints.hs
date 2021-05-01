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
-- Module      : Network.AWS.EFS.DescribeAccessPoints
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EFS.DescribeAccessPoints
  ( -- * Creating a Request
    DescribeAccessPoints (..),
    newDescribeAccessPoints,

    -- * Request Lenses
    describeAccessPoints_nextToken,
    describeAccessPoints_maxResults,
    describeAccessPoints_accessPointId,
    describeAccessPoints_fileSystemId,

    -- * Destructuring the Response
    DescribeAccessPointsResponse (..),
    newDescribeAccessPointsResponse,

    -- * Response Lenses
    describeAccessPointsResponse_nextToken,
    describeAccessPointsResponse_accessPoints,
    describeAccessPointsResponse_httpStatus,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAccessPoints' smart constructor.
data DescribeAccessPoints = DescribeAccessPoints'
  { -- | @NextToken@ is present if the response is paginated. You can use
    -- @NextMarker@ in the subsequent request to fetch the next page of access
    -- point descriptions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) When retrieving all access points for a file system, you can
    -- optionally specify the @MaxItems@ parameter to limit the number of
    -- objects returned in a response. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) Specifies an EFS access point to describe in the response;
    -- mutually exclusive with @FileSystemId@.
    accessPointId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) If you provide a @FileSystemId@, EFS returns all access
    -- points for that file system; mutually exclusive with @AccessPointId@.
    fileSystemId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'maxResults', 'describeAccessPoints_maxResults' - (Optional) When retrieving all access points for a file system, you can
-- optionally specify the @MaxItems@ parameter to limit the number of
-- objects returned in a response. The default value is 100.
--
-- 'accessPointId', 'describeAccessPoints_accessPointId' - (Optional) Specifies an EFS access point to describe in the response;
-- mutually exclusive with @FileSystemId@.
--
-- 'fileSystemId', 'describeAccessPoints_fileSystemId' - (Optional) If you provide a @FileSystemId@, EFS returns all access
-- points for that file system; mutually exclusive with @AccessPointId@.
newDescribeAccessPoints ::
  DescribeAccessPoints
newDescribeAccessPoints =
  DescribeAccessPoints'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      accessPointId = Prelude.Nothing,
      fileSystemId = Prelude.Nothing
    }

-- | @NextToken@ is present if the response is paginated. You can use
-- @NextMarker@ in the subsequent request to fetch the next page of access
-- point descriptions.
describeAccessPoints_nextToken :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Text)
describeAccessPoints_nextToken = Lens.lens (\DescribeAccessPoints' {nextToken} -> nextToken) (\s@DescribeAccessPoints' {} a -> s {nextToken = a} :: DescribeAccessPoints)

-- | (Optional) When retrieving all access points for a file system, you can
-- optionally specify the @MaxItems@ parameter to limit the number of
-- objects returned in a response. The default value is 100.
describeAccessPoints_maxResults :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Natural)
describeAccessPoints_maxResults = Lens.lens (\DescribeAccessPoints' {maxResults} -> maxResults) (\s@DescribeAccessPoints' {} a -> s {maxResults = a} :: DescribeAccessPoints)

-- | (Optional) Specifies an EFS access point to describe in the response;
-- mutually exclusive with @FileSystemId@.
describeAccessPoints_accessPointId :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Text)
describeAccessPoints_accessPointId = Lens.lens (\DescribeAccessPoints' {accessPointId} -> accessPointId) (\s@DescribeAccessPoints' {} a -> s {accessPointId = a} :: DescribeAccessPoints)

-- | (Optional) If you provide a @FileSystemId@, EFS returns all access
-- points for that file system; mutually exclusive with @AccessPointId@.
describeAccessPoints_fileSystemId :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Text)
describeAccessPoints_fileSystemId = Lens.lens (\DescribeAccessPoints' {fileSystemId} -> fileSystemId) (\s@DescribeAccessPoints' {} a -> s {fileSystemId = a} :: DescribeAccessPoints)

instance Prelude.AWSRequest DescribeAccessPoints where
  type
    Rs DescribeAccessPoints =
      DescribeAccessPointsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccessPointsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "AccessPoints"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccessPoints

instance Prelude.NFData DescribeAccessPoints

instance Prelude.ToHeaders DescribeAccessPoints where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeAccessPoints where
  toPath = Prelude.const "/2015-02-01/access-points"

instance Prelude.ToQuery DescribeAccessPoints where
  toQuery DescribeAccessPoints' {..} =
    Prelude.mconcat
      [ "NextToken" Prelude.=: nextToken,
        "MaxResults" Prelude.=: maxResults,
        "AccessPointId" Prelude.=: accessPointId,
        "FileSystemId" Prelude.=: fileSystemId
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeAccessPointsResponse_accessPoints = Lens.lens (\DescribeAccessPointsResponse' {accessPoints} -> accessPoints) (\s@DescribeAccessPointsResponse' {} a -> s {accessPoints = a} :: DescribeAccessPointsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeAccessPointsResponse_httpStatus :: Lens.Lens' DescribeAccessPointsResponse Prelude.Int
describeAccessPointsResponse_httpStatus = Lens.lens (\DescribeAccessPointsResponse' {httpStatus} -> httpStatus) (\s@DescribeAccessPointsResponse' {} a -> s {httpStatus = a} :: DescribeAccessPointsResponse)

instance Prelude.NFData DescribeAccessPointsResponse
