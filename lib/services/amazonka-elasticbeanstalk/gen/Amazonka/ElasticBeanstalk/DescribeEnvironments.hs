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
-- Module      : Amazonka.ElasticBeanstalk.DescribeEnvironments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions for existing environments.
--
-- This operation returns paginated results.
module Amazonka.ElasticBeanstalk.DescribeEnvironments
  ( -- * Creating a Request
    DescribeEnvironments (..),
    newDescribeEnvironments,

    -- * Request Lenses
    describeEnvironments_nextToken,
    describeEnvironments_includeDeleted,
    describeEnvironments_maxRecords,
    describeEnvironments_environmentIds,
    describeEnvironments_includedDeletedBackTo,
    describeEnvironments_environmentNames,
    describeEnvironments_versionLabel,
    describeEnvironments_applicationName,

    -- * Destructuring the Response
    EnvironmentDescriptionsMessage (..),
    newEnvironmentDescriptionsMessage,

    -- * Response Lenses
    environmentDescriptionsMessage_nextToken,
    environmentDescriptionsMessage_environments,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to describe one or more environments.
--
-- /See:/ 'newDescribeEnvironments' smart constructor.
data DescribeEnvironments = DescribeEnvironments'
  { -- | For a paginated request. Specify a token from a previous response page
    -- to retrieve the next response page. All other parameter values must be
    -- identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to include deleted environments:
    --
    -- @true@: Environments that have been deleted after
    -- @IncludedDeletedBackTo@ are displayed.
    --
    -- @false@: Do not include deleted environments.
    includeDeleted :: Prelude.Maybe Prelude.Bool,
    -- | For a paginated request. Specify a maximum number of environments to
    -- include in each response.
    --
    -- If no @MaxRecords@ is specified, all available environments are
    -- retrieved in a single response.
    maxRecords :: Prelude.Maybe Prelude.Natural,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that have the specified IDs.
    environmentIds :: Prelude.Maybe [Prelude.Text],
    -- | If specified when @IncludeDeleted@ is set to @true@, then environments
    -- deleted after this date are displayed.
    includedDeletedBackTo :: Prelude.Maybe Core.ISO8601,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that have the specified names.
    environmentNames :: Prelude.Maybe [Prelude.Text],
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that are associated with this application version.
    versionLabel :: Prelude.Maybe Prelude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that are associated with this application.
    applicationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'includeDeleted', 'describeEnvironments_includeDeleted' - Indicates whether to include deleted environments:
--
-- @true@: Environments that have been deleted after
-- @IncludedDeletedBackTo@ are displayed.
--
-- @false@: Do not include deleted environments.
--
-- 'maxRecords', 'describeEnvironments_maxRecords' - For a paginated request. Specify a maximum number of environments to
-- include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are
-- retrieved in a single response.
--
-- 'environmentIds', 'describeEnvironments_environmentIds' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
--
-- 'includedDeletedBackTo', 'describeEnvironments_includedDeletedBackTo' - If specified when @IncludeDeleted@ is set to @true@, then environments
-- deleted after this date are displayed.
--
-- 'environmentNames', 'describeEnvironments_environmentNames' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
--
-- 'versionLabel', 'describeEnvironments_versionLabel' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
--
-- 'applicationName', 'describeEnvironments_applicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
newDescribeEnvironments ::
  DescribeEnvironments
newDescribeEnvironments =
  DescribeEnvironments'
    { nextToken = Prelude.Nothing,
      includeDeleted = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      environmentIds = Prelude.Nothing,
      includedDeletedBackTo = Prelude.Nothing,
      environmentNames = Prelude.Nothing,
      versionLabel = Prelude.Nothing,
      applicationName = Prelude.Nothing
    }

-- | For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
describeEnvironments_nextToken :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.Text)
describeEnvironments_nextToken = Lens.lens (\DescribeEnvironments' {nextToken} -> nextToken) (\s@DescribeEnvironments' {} a -> s {nextToken = a} :: DescribeEnvironments)

-- | Indicates whether to include deleted environments:
--
-- @true@: Environments that have been deleted after
-- @IncludedDeletedBackTo@ are displayed.
--
-- @false@: Do not include deleted environments.
describeEnvironments_includeDeleted :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.Bool)
describeEnvironments_includeDeleted = Lens.lens (\DescribeEnvironments' {includeDeleted} -> includeDeleted) (\s@DescribeEnvironments' {} a -> s {includeDeleted = a} :: DescribeEnvironments)

-- | For a paginated request. Specify a maximum number of environments to
-- include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are
-- retrieved in a single response.
describeEnvironments_maxRecords :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.Natural)
describeEnvironments_maxRecords = Lens.lens (\DescribeEnvironments' {maxRecords} -> maxRecords) (\s@DescribeEnvironments' {} a -> s {maxRecords = a} :: DescribeEnvironments)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
describeEnvironments_environmentIds :: Lens.Lens' DescribeEnvironments (Prelude.Maybe [Prelude.Text])
describeEnvironments_environmentIds = Lens.lens (\DescribeEnvironments' {environmentIds} -> environmentIds) (\s@DescribeEnvironments' {} a -> s {environmentIds = a} :: DescribeEnvironments) Prelude.. Lens.mapping Lens.coerced

-- | If specified when @IncludeDeleted@ is set to @true@, then environments
-- deleted after this date are displayed.
describeEnvironments_includedDeletedBackTo :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.UTCTime)
describeEnvironments_includedDeletedBackTo = Lens.lens (\DescribeEnvironments' {includedDeletedBackTo} -> includedDeletedBackTo) (\s@DescribeEnvironments' {} a -> s {includedDeletedBackTo = a} :: DescribeEnvironments) Prelude.. Lens.mapping Core._Time

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
describeEnvironments_environmentNames :: Lens.Lens' DescribeEnvironments (Prelude.Maybe [Prelude.Text])
describeEnvironments_environmentNames = Lens.lens (\DescribeEnvironments' {environmentNames} -> environmentNames) (\s@DescribeEnvironments' {} a -> s {environmentNames = a} :: DescribeEnvironments) Prelude.. Lens.mapping Lens.coerced

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
describeEnvironments_versionLabel :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.Text)
describeEnvironments_versionLabel = Lens.lens (\DescribeEnvironments' {versionLabel} -> versionLabel) (\s@DescribeEnvironments' {} a -> s {versionLabel = a} :: DescribeEnvironments)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
describeEnvironments_applicationName :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.Text)
describeEnvironments_applicationName = Lens.lens (\DescribeEnvironments' {applicationName} -> applicationName) (\s@DescribeEnvironments' {} a -> s {applicationName = a} :: DescribeEnvironments)

instance Core.AWSPager DescribeEnvironments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? environmentDescriptionsMessage_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? environmentDescriptionsMessage_environments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEnvironments_nextToken
          Lens..~ rs
          Lens.^? environmentDescriptionsMessage_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeEnvironments where
  type
    AWSResponse DescribeEnvironments =
      EnvironmentDescriptionsMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentsResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable DescribeEnvironments where
  hashWithSalt _salt DescribeEnvironments' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` includeDeleted
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` environmentIds
      `Prelude.hashWithSalt` includedDeletedBackTo
      `Prelude.hashWithSalt` environmentNames
      `Prelude.hashWithSalt` versionLabel
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData DescribeEnvironments where
  rnf DescribeEnvironments' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf includeDeleted
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf environmentIds
      `Prelude.seq` Prelude.rnf includedDeletedBackTo
      `Prelude.seq` Prelude.rnf environmentNames
      `Prelude.seq` Prelude.rnf versionLabel
      `Prelude.seq` Prelude.rnf applicationName

instance Core.ToHeaders DescribeEnvironments where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeEnvironments where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEnvironments where
  toQuery DescribeEnvironments' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeEnvironments" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "IncludeDeleted" Core.=: includeDeleted,
        "MaxRecords" Core.=: maxRecords,
        "EnvironmentIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> environmentIds
            ),
        "IncludedDeletedBackTo"
          Core.=: includedDeletedBackTo,
        "EnvironmentNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> environmentNames
            ),
        "VersionLabel" Core.=: versionLabel,
        "ApplicationName" Core.=: applicationName
      ]
