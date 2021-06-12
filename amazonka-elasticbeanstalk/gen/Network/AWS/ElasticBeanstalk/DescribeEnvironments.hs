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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions for existing environments.
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeEnvironments
  ( -- * Creating a Request
    DescribeEnvironments (..),
    newDescribeEnvironments,

    -- * Request Lenses
    describeEnvironments_nextToken,
    describeEnvironments_environmentNames,
    describeEnvironments_environmentIds,
    describeEnvironments_versionLabel,
    describeEnvironments_includeDeleted,
    describeEnvironments_includedDeletedBackTo,
    describeEnvironments_applicationName,
    describeEnvironments_maxRecords,

    -- * Destructuring the Response
    EnvironmentDescriptionsMessage (..),
    newEnvironmentDescriptionsMessage,

    -- * Response Lenses
    environmentDescriptionsMessage_nextToken,
    environmentDescriptionsMessage_environments,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe one or more environments.
--
-- /See:/ 'newDescribeEnvironments' smart constructor.
data DescribeEnvironments = DescribeEnvironments'
  { -- | For a paginated request. Specify a token from a previous response page
    -- to retrieve the next response page. All other parameter values must be
    -- identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Core.Maybe Core.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that have the specified names.
    environmentNames :: Core.Maybe [Core.Text],
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that have the specified IDs.
    environmentIds :: Core.Maybe [Core.Text],
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that are associated with this application version.
    versionLabel :: Core.Maybe Core.Text,
    -- | Indicates whether to include deleted environments:
    --
    -- @true@: Environments that have been deleted after
    -- @IncludedDeletedBackTo@ are displayed.
    --
    -- @false@: Do not include deleted environments.
    includeDeleted :: Core.Maybe Core.Bool,
    -- | If specified when @IncludeDeleted@ is set to @true@, then environments
    -- deleted after this date are displayed.
    includedDeletedBackTo :: Core.Maybe Core.ISO8601,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that are associated with this application.
    applicationName :: Core.Maybe Core.Text,
    -- | For a paginated request. Specify a maximum number of environments to
    -- include in each response.
    --
    -- If no @MaxRecords@ is specified, all available environments are
    -- retrieved in a single response.
    maxRecords :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEnvironments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEnvironments_nextToken' - For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- 'environmentNames', 'describeEnvironments_environmentNames' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
--
-- 'environmentIds', 'describeEnvironments_environmentIds' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
--
-- 'versionLabel', 'describeEnvironments_versionLabel' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
--
-- 'includeDeleted', 'describeEnvironments_includeDeleted' - Indicates whether to include deleted environments:
--
-- @true@: Environments that have been deleted after
-- @IncludedDeletedBackTo@ are displayed.
--
-- @false@: Do not include deleted environments.
--
-- 'includedDeletedBackTo', 'describeEnvironments_includedDeletedBackTo' - If specified when @IncludeDeleted@ is set to @true@, then environments
-- deleted after this date are displayed.
--
-- 'applicationName', 'describeEnvironments_applicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
--
-- 'maxRecords', 'describeEnvironments_maxRecords' - For a paginated request. Specify a maximum number of environments to
-- include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are
-- retrieved in a single response.
newDescribeEnvironments ::
  DescribeEnvironments
newDescribeEnvironments =
  DescribeEnvironments'
    { nextToken = Core.Nothing,
      environmentNames = Core.Nothing,
      environmentIds = Core.Nothing,
      versionLabel = Core.Nothing,
      includeDeleted = Core.Nothing,
      includedDeletedBackTo = Core.Nothing,
      applicationName = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
describeEnvironments_nextToken :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.Text)
describeEnvironments_nextToken = Lens.lens (\DescribeEnvironments' {nextToken} -> nextToken) (\s@DescribeEnvironments' {} a -> s {nextToken = a} :: DescribeEnvironments)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
describeEnvironments_environmentNames :: Lens.Lens' DescribeEnvironments (Core.Maybe [Core.Text])
describeEnvironments_environmentNames = Lens.lens (\DescribeEnvironments' {environmentNames} -> environmentNames) (\s@DescribeEnvironments' {} a -> s {environmentNames = a} :: DescribeEnvironments) Core.. Lens.mapping Lens._Coerce

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
describeEnvironments_environmentIds :: Lens.Lens' DescribeEnvironments (Core.Maybe [Core.Text])
describeEnvironments_environmentIds = Lens.lens (\DescribeEnvironments' {environmentIds} -> environmentIds) (\s@DescribeEnvironments' {} a -> s {environmentIds = a} :: DescribeEnvironments) Core.. Lens.mapping Lens._Coerce

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
describeEnvironments_versionLabel :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.Text)
describeEnvironments_versionLabel = Lens.lens (\DescribeEnvironments' {versionLabel} -> versionLabel) (\s@DescribeEnvironments' {} a -> s {versionLabel = a} :: DescribeEnvironments)

-- | Indicates whether to include deleted environments:
--
-- @true@: Environments that have been deleted after
-- @IncludedDeletedBackTo@ are displayed.
--
-- @false@: Do not include deleted environments.
describeEnvironments_includeDeleted :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.Bool)
describeEnvironments_includeDeleted = Lens.lens (\DescribeEnvironments' {includeDeleted} -> includeDeleted) (\s@DescribeEnvironments' {} a -> s {includeDeleted = a} :: DescribeEnvironments)

-- | If specified when @IncludeDeleted@ is set to @true@, then environments
-- deleted after this date are displayed.
describeEnvironments_includedDeletedBackTo :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.UTCTime)
describeEnvironments_includedDeletedBackTo = Lens.lens (\DescribeEnvironments' {includedDeletedBackTo} -> includedDeletedBackTo) (\s@DescribeEnvironments' {} a -> s {includedDeletedBackTo = a} :: DescribeEnvironments) Core.. Lens.mapping Core._Time

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
describeEnvironments_applicationName :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.Text)
describeEnvironments_applicationName = Lens.lens (\DescribeEnvironments' {applicationName} -> applicationName) (\s@DescribeEnvironments' {} a -> s {applicationName = a} :: DescribeEnvironments)

-- | For a paginated request. Specify a maximum number of environments to
-- include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are
-- retrieved in a single response.
describeEnvironments_maxRecords :: Lens.Lens' DescribeEnvironments (Core.Maybe Core.Natural)
describeEnvironments_maxRecords = Lens.lens (\DescribeEnvironments' {maxRecords} -> maxRecords) (\s@DescribeEnvironments' {} a -> s {maxRecords = a} :: DescribeEnvironments)

instance Core.AWSPager DescribeEnvironments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? environmentDescriptionsMessage_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? environmentDescriptionsMessage_environments
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEnvironments_nextToken
          Lens..~ rs
          Lens.^? environmentDescriptionsMessage_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeEnvironments where
  type
    AWSResponse DescribeEnvironments =
      EnvironmentDescriptionsMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentsResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable DescribeEnvironments

instance Core.NFData DescribeEnvironments

instance Core.ToHeaders DescribeEnvironments where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeEnvironments where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEnvironments where
  toQuery DescribeEnvironments' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeEnvironments" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "EnvironmentNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> environmentNames
            ),
        "EnvironmentIds"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> environmentIds),
        "VersionLabel" Core.=: versionLabel,
        "IncludeDeleted" Core.=: includeDeleted,
        "IncludedDeletedBackTo"
          Core.=: includedDeletedBackTo,
        "ApplicationName" Core.=: applicationName,
        "MaxRecords" Core.=: maxRecords
      ]
