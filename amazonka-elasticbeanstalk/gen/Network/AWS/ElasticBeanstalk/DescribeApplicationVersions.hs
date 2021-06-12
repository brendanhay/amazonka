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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
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
    nextToken :: Core.Maybe Core.Text,
    -- | Specify a version label to show a specific application version.
    versionLabels :: Core.Maybe [Core.Text],
    -- | Specify an application name to show only application versions for that
    -- application.
    applicationName :: Core.Maybe Core.Text,
    -- | For a paginated request. Specify a maximum number of application
    -- versions to include in each response.
    --
    -- If no @MaxRecords@ is specified, all available application versions are
    -- retrieved in a single response.
    maxRecords :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      versionLabels = Core.Nothing,
      applicationName = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
describeApplicationVersions_nextToken :: Lens.Lens' DescribeApplicationVersions (Core.Maybe Core.Text)
describeApplicationVersions_nextToken = Lens.lens (\DescribeApplicationVersions' {nextToken} -> nextToken) (\s@DescribeApplicationVersions' {} a -> s {nextToken = a} :: DescribeApplicationVersions)

-- | Specify a version label to show a specific application version.
describeApplicationVersions_versionLabels :: Lens.Lens' DescribeApplicationVersions (Core.Maybe [Core.Text])
describeApplicationVersions_versionLabels = Lens.lens (\DescribeApplicationVersions' {versionLabels} -> versionLabels) (\s@DescribeApplicationVersions' {} a -> s {versionLabels = a} :: DescribeApplicationVersions) Core.. Lens.mapping Lens._Coerce

-- | Specify an application name to show only application versions for that
-- application.
describeApplicationVersions_applicationName :: Lens.Lens' DescribeApplicationVersions (Core.Maybe Core.Text)
describeApplicationVersions_applicationName = Lens.lens (\DescribeApplicationVersions' {applicationName} -> applicationName) (\s@DescribeApplicationVersions' {} a -> s {applicationName = a} :: DescribeApplicationVersions)

-- | For a paginated request. Specify a maximum number of application
-- versions to include in each response.
--
-- If no @MaxRecords@ is specified, all available application versions are
-- retrieved in a single response.
describeApplicationVersions_maxRecords :: Lens.Lens' DescribeApplicationVersions (Core.Maybe Core.Natural)
describeApplicationVersions_maxRecords = Lens.lens (\DescribeApplicationVersions' {maxRecords} -> maxRecords) (\s@DescribeApplicationVersions' {} a -> s {maxRecords = a} :: DescribeApplicationVersions)

instance Core.AWSPager DescribeApplicationVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeApplicationVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeApplicationVersionsResponse_applicationVersions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeApplicationVersions_nextToken
          Lens..~ rs
          Lens.^? describeApplicationVersionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeApplicationVersions where
  type
    AWSResponse DescribeApplicationVersions =
      DescribeApplicationVersionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeApplicationVersionsResult"
      ( \s h x ->
          DescribeApplicationVersionsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "ApplicationVersions"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeApplicationVersions

instance Core.NFData DescribeApplicationVersions

instance Core.ToHeaders DescribeApplicationVersions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeApplicationVersions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeApplicationVersions where
  toQuery DescribeApplicationVersions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeApplicationVersions" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "VersionLabels"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> versionLabels),
        "ApplicationName" Core.=: applicationName,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Result message wrapping a list of application version descriptions.
--
-- /See:/ 'newDescribeApplicationVersionsResponse' smart constructor.
data DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse'
  { -- | In a paginated request, the token that you can pass in a subsequent
    -- request to get the next response page.
    nextToken :: Core.Maybe Core.Text,
    -- | List of @ApplicationVersionDescription@ objects sorted in order of
    -- creation.
    applicationVersions :: Core.Maybe [ApplicationVersionDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeApplicationVersionsResponse
newDescribeApplicationVersionsResponse pHttpStatus_ =
  DescribeApplicationVersionsResponse'
    { nextToken =
        Core.Nothing,
      applicationVersions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | In a paginated request, the token that you can pass in a subsequent
-- request to get the next response page.
describeApplicationVersionsResponse_nextToken :: Lens.Lens' DescribeApplicationVersionsResponse (Core.Maybe Core.Text)
describeApplicationVersionsResponse_nextToken = Lens.lens (\DescribeApplicationVersionsResponse' {nextToken} -> nextToken) (\s@DescribeApplicationVersionsResponse' {} a -> s {nextToken = a} :: DescribeApplicationVersionsResponse)

-- | List of @ApplicationVersionDescription@ objects sorted in order of
-- creation.
describeApplicationVersionsResponse_applicationVersions :: Lens.Lens' DescribeApplicationVersionsResponse (Core.Maybe [ApplicationVersionDescription])
describeApplicationVersionsResponse_applicationVersions = Lens.lens (\DescribeApplicationVersionsResponse' {applicationVersions} -> applicationVersions) (\s@DescribeApplicationVersionsResponse' {} a -> s {applicationVersions = a} :: DescribeApplicationVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeApplicationVersionsResponse_httpStatus :: Lens.Lens' DescribeApplicationVersionsResponse Core.Int
describeApplicationVersionsResponse_httpStatus = Lens.lens (\DescribeApplicationVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationVersionsResponse' {} a -> s {httpStatus = a} :: DescribeApplicationVersionsResponse)

instance
  Core.NFData
    DescribeApplicationVersionsResponse
