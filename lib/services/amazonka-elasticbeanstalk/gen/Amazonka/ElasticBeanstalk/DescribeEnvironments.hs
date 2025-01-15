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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeEnvironments_applicationName,
    describeEnvironments_environmentIds,
    describeEnvironments_environmentNames,
    describeEnvironments_includeDeleted,
    describeEnvironments_includedDeletedBackTo,
    describeEnvironments_maxRecords,
    describeEnvironments_nextToken,
    describeEnvironments_versionLabel,

    -- * Destructuring the Response
    EnvironmentDescriptionsMessage (..),
    newEnvironmentDescriptionsMessage,

    -- * Response Lenses
    environmentDescriptionsMessage_environments,
    environmentDescriptionsMessage_nextToken,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to describe one or more environments.
--
-- /See:/ 'newDescribeEnvironments' smart constructor.
data DescribeEnvironments = DescribeEnvironments'
  { -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that are associated with this application.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that have the specified IDs.
    environmentIds :: Prelude.Maybe [Prelude.Text],
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that have the specified names.
    environmentNames :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether to include deleted environments:
    --
    -- @true@: Environments that have been deleted after
    -- @IncludedDeletedBackTo@ are displayed.
    --
    -- @false@: Do not include deleted environments.
    includeDeleted :: Prelude.Maybe Prelude.Bool,
    -- | If specified when @IncludeDeleted@ is set to @true@, then environments
    -- deleted after this date are displayed.
    includedDeletedBackTo :: Prelude.Maybe Data.ISO8601,
    -- | For a paginated request. Specify a maximum number of environments to
    -- include in each response.
    --
    -- If no @MaxRecords@ is specified, all available environments are
    -- retrieved in a single response.
    maxRecords :: Prelude.Maybe Prelude.Natural,
    -- | For a paginated request. Specify a token from a previous response page
    -- to retrieve the next response page. All other parameter values must be
    -- identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
    -- to include only those that are associated with this application version.
    versionLabel :: Prelude.Maybe Prelude.Text
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
-- 'applicationName', 'describeEnvironments_applicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
--
-- 'environmentIds', 'describeEnvironments_environmentIds' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
--
-- 'environmentNames', 'describeEnvironments_environmentNames' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
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
-- 'maxRecords', 'describeEnvironments_maxRecords' - For a paginated request. Specify a maximum number of environments to
-- include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are
-- retrieved in a single response.
--
-- 'nextToken', 'describeEnvironments_nextToken' - For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- 'versionLabel', 'describeEnvironments_versionLabel' - If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
newDescribeEnvironments ::
  DescribeEnvironments
newDescribeEnvironments =
  DescribeEnvironments'
    { applicationName =
        Prelude.Nothing,
      environmentIds = Prelude.Nothing,
      environmentNames = Prelude.Nothing,
      includeDeleted = Prelude.Nothing,
      includedDeletedBackTo = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      versionLabel = Prelude.Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application.
describeEnvironments_applicationName :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.Text)
describeEnvironments_applicationName = Lens.lens (\DescribeEnvironments' {applicationName} -> applicationName) (\s@DescribeEnvironments' {} a -> s {applicationName = a} :: DescribeEnvironments)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified IDs.
describeEnvironments_environmentIds :: Lens.Lens' DescribeEnvironments (Prelude.Maybe [Prelude.Text])
describeEnvironments_environmentIds = Lens.lens (\DescribeEnvironments' {environmentIds} -> environmentIds) (\s@DescribeEnvironments' {} a -> s {environmentIds = a} :: DescribeEnvironments) Prelude.. Lens.mapping Lens.coerced

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that have the specified names.
describeEnvironments_environmentNames :: Lens.Lens' DescribeEnvironments (Prelude.Maybe [Prelude.Text])
describeEnvironments_environmentNames = Lens.lens (\DescribeEnvironments' {environmentNames} -> environmentNames) (\s@DescribeEnvironments' {} a -> s {environmentNames = a} :: DescribeEnvironments) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether to include deleted environments:
--
-- @true@: Environments that have been deleted after
-- @IncludedDeletedBackTo@ are displayed.
--
-- @false@: Do not include deleted environments.
describeEnvironments_includeDeleted :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.Bool)
describeEnvironments_includeDeleted = Lens.lens (\DescribeEnvironments' {includeDeleted} -> includeDeleted) (\s@DescribeEnvironments' {} a -> s {includeDeleted = a} :: DescribeEnvironments)

-- | If specified when @IncludeDeleted@ is set to @true@, then environments
-- deleted after this date are displayed.
describeEnvironments_includedDeletedBackTo :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.UTCTime)
describeEnvironments_includedDeletedBackTo = Lens.lens (\DescribeEnvironments' {includedDeletedBackTo} -> includedDeletedBackTo) (\s@DescribeEnvironments' {} a -> s {includedDeletedBackTo = a} :: DescribeEnvironments) Prelude.. Lens.mapping Data._Time

-- | For a paginated request. Specify a maximum number of environments to
-- include in each response.
--
-- If no @MaxRecords@ is specified, all available environments are
-- retrieved in a single response.
describeEnvironments_maxRecords :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.Natural)
describeEnvironments_maxRecords = Lens.lens (\DescribeEnvironments' {maxRecords} -> maxRecords) (\s@DescribeEnvironments' {} a -> s {maxRecords = a} :: DescribeEnvironments)

-- | For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
describeEnvironments_nextToken :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.Text)
describeEnvironments_nextToken = Lens.lens (\DescribeEnvironments' {nextToken} -> nextToken) (\s@DescribeEnvironments' {} a -> s {nextToken = a} :: DescribeEnvironments)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those that are associated with this application version.
describeEnvironments_versionLabel :: Lens.Lens' DescribeEnvironments (Prelude.Maybe Prelude.Text)
describeEnvironments_versionLabel = Lens.lens (\DescribeEnvironments' {versionLabel} -> versionLabel) (\s@DescribeEnvironments' {} a -> s {versionLabel = a} :: DescribeEnvironments)

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
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable DescribeEnvironments where
  hashWithSalt _salt DescribeEnvironments' {..} =
    _salt
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` environmentIds
      `Prelude.hashWithSalt` environmentNames
      `Prelude.hashWithSalt` includeDeleted
      `Prelude.hashWithSalt` includedDeletedBackTo
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` versionLabel

instance Prelude.NFData DescribeEnvironments where
  rnf DescribeEnvironments' {..} =
    Prelude.rnf applicationName `Prelude.seq`
      Prelude.rnf environmentIds `Prelude.seq`
        Prelude.rnf environmentNames `Prelude.seq`
          Prelude.rnf includeDeleted `Prelude.seq`
            Prelude.rnf includedDeletedBackTo `Prelude.seq`
              Prelude.rnf maxRecords `Prelude.seq`
                Prelude.rnf nextToken `Prelude.seq`
                  Prelude.rnf versionLabel

instance Data.ToHeaders DescribeEnvironments where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeEnvironments where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEnvironments where
  toQuery DescribeEnvironments' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeEnvironments" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ApplicationName" Data.=: applicationName,
        "EnvironmentIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> environmentIds
            ),
        "EnvironmentNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> environmentNames
            ),
        "IncludeDeleted" Data.=: includeDeleted,
        "IncludedDeletedBackTo"
          Data.=: includedDeletedBackTo,
        "MaxRecords" Data.=: maxRecords,
        "NextToken" Data.=: nextToken,
        "VersionLabel" Data.=: versionLabel
      ]
