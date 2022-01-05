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
-- Module      : Amazonka.ElasticBeanstalk.ListPlatformBranches
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform branches available for your account in an AWS Region.
-- Provides summary information about each platform branch.
--
-- For definitions of platform branch and other platform-related terms, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/platforms-glossary.html AWS Elastic Beanstalk Platforms Glossary>.
module Amazonka.ElasticBeanstalk.ListPlatformBranches
  ( -- * Creating a Request
    ListPlatformBranches (..),
    newListPlatformBranches,

    -- * Request Lenses
    listPlatformBranches_filters,
    listPlatformBranches_nextToken,
    listPlatformBranches_maxRecords,

    -- * Destructuring the Response
    ListPlatformBranchesResponse (..),
    newListPlatformBranchesResponse,

    -- * Response Lenses
    listPlatformBranchesResponse_platformBranchSummaryList,
    listPlatformBranchesResponse_nextToken,
    listPlatformBranchesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPlatformBranches' smart constructor.
data ListPlatformBranches = ListPlatformBranches'
  { -- | Criteria for restricting the resulting list of platform branches. The
    -- filter is evaluated as a logical conjunction (AND) of the separate
    -- @SearchFilter@ terms.
    --
    -- The following list shows valid attribute values for each of the
    -- @SearchFilter@ terms. Most operators take a single value. The @in@ and
    -- @not_in@ operators can take multiple values.
    --
    -- -   @Attribute = BranchName@:
    --
    --     -   @Operator@: @=@ | @!=@ | @begins_with@ | @ends_with@ |
    --         @contains@ | @in@ | @not_in@
    --
    -- -   @Attribute = LifecycleState@:
    --
    --     -   @Operator@: @=@ | @!=@ | @in@ | @not_in@
    --
    --     -   @Values@: @beta@ | @supported@ | @deprecated@ | @retired@
    --
    -- -   @Attribute = PlatformName@:
    --
    --     -   @Operator@: @=@ | @!=@ | @begins_with@ | @ends_with@ |
    --         @contains@ | @in@ | @not_in@
    --
    -- -   @Attribute = TierType@:
    --
    --     -   @Operator@: @=@ | @!=@
    --
    --     -   @Values@: @WebServer\/Standard@ | @Worker\/SQS\/HTTP@
    --
    -- Array size: limited to 10 @SearchFilter@ objects.
    --
    -- Within each @SearchFilter@ item, the @Values@ array is limited to 10
    -- items.
    filters :: Prelude.Maybe [SearchFilter],
    -- | For a paginated request. Specify a token from a previous response page
    -- to retrieve the next response page. All other parameter values must be
    -- identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of platform branch values returned in one call.
    maxRecords :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlatformBranches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listPlatformBranches_filters' - Criteria for restricting the resulting list of platform branches. The
-- filter is evaluated as a logical conjunction (AND) of the separate
-- @SearchFilter@ terms.
--
-- The following list shows valid attribute values for each of the
-- @SearchFilter@ terms. Most operators take a single value. The @in@ and
-- @not_in@ operators can take multiple values.
--
-- -   @Attribute = BranchName@:
--
--     -   @Operator@: @=@ | @!=@ | @begins_with@ | @ends_with@ |
--         @contains@ | @in@ | @not_in@
--
-- -   @Attribute = LifecycleState@:
--
--     -   @Operator@: @=@ | @!=@ | @in@ | @not_in@
--
--     -   @Values@: @beta@ | @supported@ | @deprecated@ | @retired@
--
-- -   @Attribute = PlatformName@:
--
--     -   @Operator@: @=@ | @!=@ | @begins_with@ | @ends_with@ |
--         @contains@ | @in@ | @not_in@
--
-- -   @Attribute = TierType@:
--
--     -   @Operator@: @=@ | @!=@
--
--     -   @Values@: @WebServer\/Standard@ | @Worker\/SQS\/HTTP@
--
-- Array size: limited to 10 @SearchFilter@ objects.
--
-- Within each @SearchFilter@ item, the @Values@ array is limited to 10
-- items.
--
-- 'nextToken', 'listPlatformBranches_nextToken' - For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- 'maxRecords', 'listPlatformBranches_maxRecords' - The maximum number of platform branch values returned in one call.
newListPlatformBranches ::
  ListPlatformBranches
newListPlatformBranches =
  ListPlatformBranches'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | Criteria for restricting the resulting list of platform branches. The
-- filter is evaluated as a logical conjunction (AND) of the separate
-- @SearchFilter@ terms.
--
-- The following list shows valid attribute values for each of the
-- @SearchFilter@ terms. Most operators take a single value. The @in@ and
-- @not_in@ operators can take multiple values.
--
-- -   @Attribute = BranchName@:
--
--     -   @Operator@: @=@ | @!=@ | @begins_with@ | @ends_with@ |
--         @contains@ | @in@ | @not_in@
--
-- -   @Attribute = LifecycleState@:
--
--     -   @Operator@: @=@ | @!=@ | @in@ | @not_in@
--
--     -   @Values@: @beta@ | @supported@ | @deprecated@ | @retired@
--
-- -   @Attribute = PlatformName@:
--
--     -   @Operator@: @=@ | @!=@ | @begins_with@ | @ends_with@ |
--         @contains@ | @in@ | @not_in@
--
-- -   @Attribute = TierType@:
--
--     -   @Operator@: @=@ | @!=@
--
--     -   @Values@: @WebServer\/Standard@ | @Worker\/SQS\/HTTP@
--
-- Array size: limited to 10 @SearchFilter@ objects.
--
-- Within each @SearchFilter@ item, the @Values@ array is limited to 10
-- items.
listPlatformBranches_filters :: Lens.Lens' ListPlatformBranches (Prelude.Maybe [SearchFilter])
listPlatformBranches_filters = Lens.lens (\ListPlatformBranches' {filters} -> filters) (\s@ListPlatformBranches' {} a -> s {filters = a} :: ListPlatformBranches) Prelude.. Lens.mapping Lens.coerced

-- | For a paginated request. Specify a token from a previous response page
-- to retrieve the next response page. All other parameter values must be
-- identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
listPlatformBranches_nextToken :: Lens.Lens' ListPlatformBranches (Prelude.Maybe Prelude.Text)
listPlatformBranches_nextToken = Lens.lens (\ListPlatformBranches' {nextToken} -> nextToken) (\s@ListPlatformBranches' {} a -> s {nextToken = a} :: ListPlatformBranches)

-- | The maximum number of platform branch values returned in one call.
listPlatformBranches_maxRecords :: Lens.Lens' ListPlatformBranches (Prelude.Maybe Prelude.Natural)
listPlatformBranches_maxRecords = Lens.lens (\ListPlatformBranches' {maxRecords} -> maxRecords) (\s@ListPlatformBranches' {} a -> s {maxRecords = a} :: ListPlatformBranches)

instance Core.AWSRequest ListPlatformBranches where
  type
    AWSResponse ListPlatformBranches =
      ListPlatformBranchesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListPlatformBranchesResult"
      ( \s h x ->
          ListPlatformBranchesResponse'
            Prelude.<$> ( x Core..@? "PlatformBranchSummaryList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPlatformBranches where
  hashWithSalt _salt ListPlatformBranches' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData ListPlatformBranches where
  rnf ListPlatformBranches' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxRecords

instance Core.ToHeaders ListPlatformBranches where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListPlatformBranches where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPlatformBranches where
  toQuery ListPlatformBranches' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListPlatformBranches" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> filters),
        "NextToken" Core.=: nextToken,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newListPlatformBranchesResponse' smart constructor.
data ListPlatformBranchesResponse = ListPlatformBranchesResponse'
  { -- | Summary information about the platform branches.
    platformBranchSummaryList :: Prelude.Maybe [PlatformBranchSummary],
    -- | In a paginated request, if this value isn\'t @null@, it\'s the token
    -- that you can pass in a subsequent request to get the next response page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlatformBranchesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformBranchSummaryList', 'listPlatformBranchesResponse_platformBranchSummaryList' - Summary information about the platform branches.
--
-- 'nextToken', 'listPlatformBranchesResponse_nextToken' - In a paginated request, if this value isn\'t @null@, it\'s the token
-- that you can pass in a subsequent request to get the next response page.
--
-- 'httpStatus', 'listPlatformBranchesResponse_httpStatus' - The response's http status code.
newListPlatformBranchesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPlatformBranchesResponse
newListPlatformBranchesResponse pHttpStatus_ =
  ListPlatformBranchesResponse'
    { platformBranchSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information about the platform branches.
listPlatformBranchesResponse_platformBranchSummaryList :: Lens.Lens' ListPlatformBranchesResponse (Prelude.Maybe [PlatformBranchSummary])
listPlatformBranchesResponse_platformBranchSummaryList = Lens.lens (\ListPlatformBranchesResponse' {platformBranchSummaryList} -> platformBranchSummaryList) (\s@ListPlatformBranchesResponse' {} a -> s {platformBranchSummaryList = a} :: ListPlatformBranchesResponse) Prelude.. Lens.mapping Lens.coerced

-- | In a paginated request, if this value isn\'t @null@, it\'s the token
-- that you can pass in a subsequent request to get the next response page.
listPlatformBranchesResponse_nextToken :: Lens.Lens' ListPlatformBranchesResponse (Prelude.Maybe Prelude.Text)
listPlatformBranchesResponse_nextToken = Lens.lens (\ListPlatformBranchesResponse' {nextToken} -> nextToken) (\s@ListPlatformBranchesResponse' {} a -> s {nextToken = a} :: ListPlatformBranchesResponse)

-- | The response's http status code.
listPlatformBranchesResponse_httpStatus :: Lens.Lens' ListPlatformBranchesResponse Prelude.Int
listPlatformBranchesResponse_httpStatus = Lens.lens (\ListPlatformBranchesResponse' {httpStatus} -> httpStatus) (\s@ListPlatformBranchesResponse' {} a -> s {httpStatus = a} :: ListPlatformBranchesResponse)

instance Prelude.NFData ListPlatformBranchesResponse where
  rnf ListPlatformBranchesResponse' {..} =
    Prelude.rnf platformBranchSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
