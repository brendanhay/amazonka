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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of application versions.
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
  ( -- * Creating a Request
    DescribeApplicationVersions (..),
    newDescribeApplicationVersions,

    -- * Request Lenses
    describeApplicationVersions_nextToken,
    describeApplicationVersions_versionLabels,
    describeApplicationVersions_applicationName,
    describeApplicationVersions_maxRecords,

    -- * Destructuring the Response
    DescribeApplicationVersionsResponse (..),
    newDescribeApplicationVersionsResponse,

    -- * Response Lenses
    describeApplicationVersionsResponse_nextToken,
    describeApplicationVersionsResponse_applicationVersions,
    describeApplicationVersionsResponse_httpStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe application versions.
--
-- /See:/ 'newDescribeApplicationVersions' smart constructor.
data DescribeApplicationVersions = DescribeApplicationVersions'
  { -- | For a paginated request. Specify a token from a previous response page
    -- to retrieve the next response page. All other parameter values must be
    -- identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specify a version label to show a specific application version.
    versionLabels :: Prelude.Maybe [Prelude.Text],
    -- | Specify an application name to show only application versions for that
    -- application.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | For a paginated request. Specify a maximum number of application
    -- versions to include in each response.
    --
    -- If no @MaxRecords@ is specified, all available application versions are
    -- retrieved in a single response.
    maxRecords :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeApplicationVersions_nextToken' - For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- 'versionLabels', 'describeApplicationVersions_versionLabels' - Specify a version label to show a specific application version.
--
-- 'applicationName', 'describeApplicationVersions_applicationName' - Specify an application name to show only application versions for that
-- application.
--
-- 'maxRecords', 'describeApplicationVersions_maxRecords' - For a paginated request. Specify a maximum number of application
-- versions to include in each response.
--
-- If no @MaxRecords@ is specified, all available application versions are
-- retrieved in a single response.
newDescribeApplicationVersions ::
  DescribeApplicationVersions
newDescribeApplicationVersions =
  DescribeApplicationVersions'
    { nextToken =
        Prelude.Nothing,
      versionLabels = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
describeApplicationVersions_nextToken :: Lens.Lens' DescribeApplicationVersions (Prelude.Maybe Prelude.Text)
describeApplicationVersions_nextToken = Lens.lens (\DescribeApplicationVersions' {nextToken} -> nextToken) (\s@DescribeApplicationVersions' {} a -> s {nextToken = a} :: DescribeApplicationVersions)

-- | Specify a version label to show a specific application version.
describeApplicationVersions_versionLabels :: Lens.Lens' DescribeApplicationVersions (Prelude.Maybe [Prelude.Text])
describeApplicationVersions_versionLabels = Lens.lens (\DescribeApplicationVersions' {versionLabels} -> versionLabels) (\s@DescribeApplicationVersions' {} a -> s {versionLabels = a} :: DescribeApplicationVersions) Prelude.. Lens.mapping Prelude._Coerce

-- | Specify an application name to show only application versions for that
-- application.
describeApplicationVersions_applicationName :: Lens.Lens' DescribeApplicationVersions (Prelude.Maybe Prelude.Text)
describeApplicationVersions_applicationName = Lens.lens (\DescribeApplicationVersions' {applicationName} -> applicationName) (\s@DescribeApplicationVersions' {} a -> s {applicationName = a} :: DescribeApplicationVersions)

-- | For a paginated request. Specify a maximum number of application
-- versions to include in each response.
--
-- If no @MaxRecords@ is specified, all available application versions are
-- retrieved in a single response.
describeApplicationVersions_maxRecords :: Lens.Lens' DescribeApplicationVersions (Prelude.Maybe Prelude.Natural)
describeApplicationVersions_maxRecords = Lens.lens (\DescribeApplicationVersions' {maxRecords} -> maxRecords) (\s@DescribeApplicationVersions' {} a -> s {maxRecords = a} :: DescribeApplicationVersions)

instance Pager.AWSPager DescribeApplicationVersions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeApplicationVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeApplicationVersionsResponse_applicationVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeApplicationVersions_nextToken
          Lens..~ rs
          Lens.^? describeApplicationVersionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeApplicationVersions
  where
  type
    Rs DescribeApplicationVersions =
      DescribeApplicationVersionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeApplicationVersionsResult"
      ( \s h x ->
          DescribeApplicationVersionsResponse'
            Prelude.<$> (x Prelude..@? "NextToken")
            Prelude.<*> ( x Prelude..@? "ApplicationVersions"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeApplicationVersions

instance Prelude.NFData DescribeApplicationVersions

instance
  Prelude.ToHeaders
    DescribeApplicationVersions
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeApplicationVersions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeApplicationVersions where
  toQuery DescribeApplicationVersions' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeApplicationVersions" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "VersionLabels"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> versionLabels
            ),
        "ApplicationName" Prelude.=: applicationName,
        "MaxRecords" Prelude.=: maxRecords
      ]

-- | Result message wrapping a list of application version descriptions.
--
-- /See:/ 'newDescribeApplicationVersionsResponse' smart constructor.
data DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse'
  { -- | In a paginated request, the token that you can pass in a subsequent
    -- request to get the next response page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of @ApplicationVersionDescription@ objects sorted in order of
    -- creation.
    applicationVersions :: Prelude.Maybe [ApplicationVersionDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeApplicationVersionsResponse_nextToken' - In a paginated request, the token that you can pass in a subsequent
-- request to get the next response page.
--
-- 'applicationVersions', 'describeApplicationVersionsResponse_applicationVersions' - List of @ApplicationVersionDescription@ objects sorted in order of
-- creation.
--
-- 'httpStatus', 'describeApplicationVersionsResponse_httpStatus' - The response's http status code.
newDescribeApplicationVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeApplicationVersionsResponse
newDescribeApplicationVersionsResponse pHttpStatus_ =
  DescribeApplicationVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      applicationVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | In a paginated request, the token that you can pass in a subsequent
-- request to get the next response page.
describeApplicationVersionsResponse_nextToken :: Lens.Lens' DescribeApplicationVersionsResponse (Prelude.Maybe Prelude.Text)
describeApplicationVersionsResponse_nextToken = Lens.lens (\DescribeApplicationVersionsResponse' {nextToken} -> nextToken) (\s@DescribeApplicationVersionsResponse' {} a -> s {nextToken = a} :: DescribeApplicationVersionsResponse)

-- | List of @ApplicationVersionDescription@ objects sorted in order of
-- creation.
describeApplicationVersionsResponse_applicationVersions :: Lens.Lens' DescribeApplicationVersionsResponse (Prelude.Maybe [ApplicationVersionDescription])
describeApplicationVersionsResponse_applicationVersions = Lens.lens (\DescribeApplicationVersionsResponse' {applicationVersions} -> applicationVersions) (\s@DescribeApplicationVersionsResponse' {} a -> s {applicationVersions = a} :: DescribeApplicationVersionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeApplicationVersionsResponse_httpStatus :: Lens.Lens' DescribeApplicationVersionsResponse Prelude.Int
describeApplicationVersionsResponse_httpStatus = Lens.lens (\DescribeApplicationVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationVersionsResponse' {} a -> s {httpStatus = a} :: DescribeApplicationVersionsResponse)

instance
  Prelude.NFData
    DescribeApplicationVersionsResponse
