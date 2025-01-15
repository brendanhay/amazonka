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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeAccessPoints_accessPointId,
    describeAccessPoints_fileSystemId,
    describeAccessPoints_maxResults,
    describeAccessPoints_nextToken,

    -- * Destructuring the Response
    DescribeAccessPointsResponse (..),
    newDescribeAccessPointsResponse,

    -- * Response Lenses
    describeAccessPointsResponse_accessPoints,
    describeAccessPointsResponse_nextToken,
    describeAccessPointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccessPoints' smart constructor.
data DescribeAccessPoints = DescribeAccessPoints'
  { -- | (Optional) Specifies an EFS access point to describe in the response;
    -- mutually exclusive with @FileSystemId@.
    accessPointId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) If you provide a @FileSystemId@, EFS returns all access
    -- points for that file system; mutually exclusive with @AccessPointId@.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) When retrieving all access points for a file system, you can
    -- optionally specify the @MaxItems@ parameter to limit the number of
    -- objects returned in a response. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | @NextToken@ is present if the response is paginated. You can use
    -- @NextMarker@ in the subsequent request to fetch the next page of access
    -- point descriptions.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'accessPointId', 'describeAccessPoints_accessPointId' - (Optional) Specifies an EFS access point to describe in the response;
-- mutually exclusive with @FileSystemId@.
--
-- 'fileSystemId', 'describeAccessPoints_fileSystemId' - (Optional) If you provide a @FileSystemId@, EFS returns all access
-- points for that file system; mutually exclusive with @AccessPointId@.
--
-- 'maxResults', 'describeAccessPoints_maxResults' - (Optional) When retrieving all access points for a file system, you can
-- optionally specify the @MaxItems@ parameter to limit the number of
-- objects returned in a response. The default value is 100.
--
-- 'nextToken', 'describeAccessPoints_nextToken' - @NextToken@ is present if the response is paginated. You can use
-- @NextMarker@ in the subsequent request to fetch the next page of access
-- point descriptions.
newDescribeAccessPoints ::
  DescribeAccessPoints
newDescribeAccessPoints =
  DescribeAccessPoints'
    { accessPointId =
        Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | (Optional) Specifies an EFS access point to describe in the response;
-- mutually exclusive with @FileSystemId@.
describeAccessPoints_accessPointId :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Text)
describeAccessPoints_accessPointId = Lens.lens (\DescribeAccessPoints' {accessPointId} -> accessPointId) (\s@DescribeAccessPoints' {} a -> s {accessPointId = a} :: DescribeAccessPoints)

-- | (Optional) If you provide a @FileSystemId@, EFS returns all access
-- points for that file system; mutually exclusive with @AccessPointId@.
describeAccessPoints_fileSystemId :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Text)
describeAccessPoints_fileSystemId = Lens.lens (\DescribeAccessPoints' {fileSystemId} -> fileSystemId) (\s@DescribeAccessPoints' {} a -> s {fileSystemId = a} :: DescribeAccessPoints)

-- | (Optional) When retrieving all access points for a file system, you can
-- optionally specify the @MaxItems@ parameter to limit the number of
-- objects returned in a response. The default value is 100.
describeAccessPoints_maxResults :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Natural)
describeAccessPoints_maxResults = Lens.lens (\DescribeAccessPoints' {maxResults} -> maxResults) (\s@DescribeAccessPoints' {} a -> s {maxResults = a} :: DescribeAccessPoints)

-- | @NextToken@ is present if the response is paginated. You can use
-- @NextMarker@ in the subsequent request to fetch the next page of access
-- point descriptions.
describeAccessPoints_nextToken :: Lens.Lens' DescribeAccessPoints (Prelude.Maybe Prelude.Text)
describeAccessPoints_nextToken = Lens.lens (\DescribeAccessPoints' {nextToken} -> nextToken) (\s@DescribeAccessPoints' {} a -> s {nextToken = a} :: DescribeAccessPoints)

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
            Prelude.<$> (x Data..?> "AccessPoints" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccessPoints where
  hashWithSalt _salt DescribeAccessPoints' {..} =
    _salt
      `Prelude.hashWithSalt` accessPointId
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeAccessPoints where
  rnf DescribeAccessPoints' {..} =
    Prelude.rnf accessPointId `Prelude.seq`
      Prelude.rnf fileSystemId `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken

instance Data.ToHeaders DescribeAccessPoints where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAccessPoints where
  toPath = Prelude.const "/2015-02-01/access-points"

instance Data.ToQuery DescribeAccessPoints where
  toQuery DescribeAccessPoints' {..} =
    Prelude.mconcat
      [ "AccessPointId" Data.=: accessPointId,
        "FileSystemId" Data.=: fileSystemId,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeAccessPointsResponse' smart constructor.
data DescribeAccessPointsResponse = DescribeAccessPointsResponse'
  { -- | An array of access point descriptions.
    accessPoints :: Prelude.Maybe [AccessPointDescription],
    -- | Present if there are more access points than returned in the response.
    -- You can use the NextMarker in the subsequent request to fetch the
    -- additional descriptions.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'accessPoints', 'describeAccessPointsResponse_accessPoints' - An array of access point descriptions.
--
-- 'nextToken', 'describeAccessPointsResponse_nextToken' - Present if there are more access points than returned in the response.
-- You can use the NextMarker in the subsequent request to fetch the
-- additional descriptions.
--
-- 'httpStatus', 'describeAccessPointsResponse_httpStatus' - The response's http status code.
newDescribeAccessPointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccessPointsResponse
newDescribeAccessPointsResponse pHttpStatus_ =
  DescribeAccessPointsResponse'
    { accessPoints =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of access point descriptions.
describeAccessPointsResponse_accessPoints :: Lens.Lens' DescribeAccessPointsResponse (Prelude.Maybe [AccessPointDescription])
describeAccessPointsResponse_accessPoints = Lens.lens (\DescribeAccessPointsResponse' {accessPoints} -> accessPoints) (\s@DescribeAccessPointsResponse' {} a -> s {accessPoints = a} :: DescribeAccessPointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Present if there are more access points than returned in the response.
-- You can use the NextMarker in the subsequent request to fetch the
-- additional descriptions.
describeAccessPointsResponse_nextToken :: Lens.Lens' DescribeAccessPointsResponse (Prelude.Maybe Prelude.Text)
describeAccessPointsResponse_nextToken = Lens.lens (\DescribeAccessPointsResponse' {nextToken} -> nextToken) (\s@DescribeAccessPointsResponse' {} a -> s {nextToken = a} :: DescribeAccessPointsResponse)

-- | The response's http status code.
describeAccessPointsResponse_httpStatus :: Lens.Lens' DescribeAccessPointsResponse Prelude.Int
describeAccessPointsResponse_httpStatus = Lens.lens (\DescribeAccessPointsResponse' {httpStatus} -> httpStatus) (\s@DescribeAccessPointsResponse' {} a -> s {httpStatus = a} :: DescribeAccessPointsResponse)

instance Prelude.NFData DescribeAccessPointsResponse where
  rnf DescribeAccessPointsResponse' {..} =
    Prelude.rnf accessPoints `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
