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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists an environment\'s completed and failed managed actions.
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
  ( -- * Creating a Request
    DescribeEnvironmentManagedActionHistory (..),
    newDescribeEnvironmentManagedActionHistory,

    -- * Request Lenses
    describeEnvironmentManagedActionHistory_nextToken,
    describeEnvironmentManagedActionHistory_environmentId,
    describeEnvironmentManagedActionHistory_environmentName,
    describeEnvironmentManagedActionHistory_maxItems,

    -- * Destructuring the Response
    DescribeEnvironmentManagedActionHistoryResponse (..),
    newDescribeEnvironmentManagedActionHistoryResponse,

    -- * Response Lenses
    describeEnvironmentManagedActionHistoryResponse_nextToken,
    describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems,
    describeEnvironmentManagedActionHistoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to list completed and failed managed actions.
--
-- /See:/ 'newDescribeEnvironmentManagedActionHistory' smart constructor.
data DescribeEnvironmentManagedActionHistory = DescribeEnvironmentManagedActionHistory'
  { -- | The pagination token returned by a previous request.
    nextToken :: Core.Maybe Core.Text,
    -- | The environment ID of the target environment.
    environmentId :: Core.Maybe Core.Text,
    -- | The name of the target environment.
    environmentName :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for a single request.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEnvironmentManagedActionHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEnvironmentManagedActionHistory_nextToken' - The pagination token returned by a previous request.
--
-- 'environmentId', 'describeEnvironmentManagedActionHistory_environmentId' - The environment ID of the target environment.
--
-- 'environmentName', 'describeEnvironmentManagedActionHistory_environmentName' - The name of the target environment.
--
-- 'maxItems', 'describeEnvironmentManagedActionHistory_maxItems' - The maximum number of items to return for a single request.
newDescribeEnvironmentManagedActionHistory ::
  DescribeEnvironmentManagedActionHistory
newDescribeEnvironmentManagedActionHistory =
  DescribeEnvironmentManagedActionHistory'
    { nextToken =
        Core.Nothing,
      environmentId = Core.Nothing,
      environmentName = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The pagination token returned by a previous request.
describeEnvironmentManagedActionHistory_nextToken :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Core.Maybe Core.Text)
describeEnvironmentManagedActionHistory_nextToken = Lens.lens (\DescribeEnvironmentManagedActionHistory' {nextToken} -> nextToken) (\s@DescribeEnvironmentManagedActionHistory' {} a -> s {nextToken = a} :: DescribeEnvironmentManagedActionHistory)

-- | The environment ID of the target environment.
describeEnvironmentManagedActionHistory_environmentId :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Core.Maybe Core.Text)
describeEnvironmentManagedActionHistory_environmentId = Lens.lens (\DescribeEnvironmentManagedActionHistory' {environmentId} -> environmentId) (\s@DescribeEnvironmentManagedActionHistory' {} a -> s {environmentId = a} :: DescribeEnvironmentManagedActionHistory)

-- | The name of the target environment.
describeEnvironmentManagedActionHistory_environmentName :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Core.Maybe Core.Text)
describeEnvironmentManagedActionHistory_environmentName = Lens.lens (\DescribeEnvironmentManagedActionHistory' {environmentName} -> environmentName) (\s@DescribeEnvironmentManagedActionHistory' {} a -> s {environmentName = a} :: DescribeEnvironmentManagedActionHistory)

-- | The maximum number of items to return for a single request.
describeEnvironmentManagedActionHistory_maxItems :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Core.Maybe Core.Natural)
describeEnvironmentManagedActionHistory_maxItems = Lens.lens (\DescribeEnvironmentManagedActionHistory' {maxItems} -> maxItems) (\s@DescribeEnvironmentManagedActionHistory' {} a -> s {maxItems = a} :: DescribeEnvironmentManagedActionHistory)

instance
  Core.AWSPager
    DescribeEnvironmentManagedActionHistory
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEnvironmentManagedActionHistoryResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems
              Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEnvironmentManagedActionHistory_nextToken
          Lens..~ rs
            Lens.^? describeEnvironmentManagedActionHistoryResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeEnvironmentManagedActionHistory
  where
  type
    AWSResponse
      DescribeEnvironmentManagedActionHistory =
      DescribeEnvironmentManagedActionHistoryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentManagedActionHistoryResult"
      ( \s h x ->
          DescribeEnvironmentManagedActionHistoryResponse'
            Core.<$> (x Core..@? "NextToken")
              Core.<*> ( x Core..@? "ManagedActionHistoryItems"
                           Core..!@ Core.mempty
                           Core.>>= Core.may (Core.parseXMLList1 "member")
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeEnvironmentManagedActionHistory

instance
  Core.NFData
    DescribeEnvironmentManagedActionHistory

instance
  Core.ToHeaders
    DescribeEnvironmentManagedActionHistory
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeEnvironmentManagedActionHistory
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeEnvironmentManagedActionHistory
  where
  toQuery DescribeEnvironmentManagedActionHistory' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeEnvironmentManagedActionHistory" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "EnvironmentId" Core.=: environmentId,
        "EnvironmentName" Core.=: environmentName,
        "MaxItems" Core.=: maxItems
      ]

-- | A result message containing a list of completed and failed managed
-- actions.
--
-- /See:/ 'newDescribeEnvironmentManagedActionHistoryResponse' smart constructor.
data DescribeEnvironmentManagedActionHistoryResponse = DescribeEnvironmentManagedActionHistoryResponse'
  { -- | A pagination token that you pass to
    -- DescribeEnvironmentManagedActionHistory to get the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of completed and failed managed actions.
    managedActionHistoryItems :: Core.Maybe (Core.NonEmpty ManagedActionHistoryItem),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEnvironmentManagedActionHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEnvironmentManagedActionHistoryResponse_nextToken' - A pagination token that you pass to
-- DescribeEnvironmentManagedActionHistory to get the next page of results.
--
-- 'managedActionHistoryItems', 'describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems' - A list of completed and failed managed actions.
--
-- 'httpStatus', 'describeEnvironmentManagedActionHistoryResponse_httpStatus' - The response's http status code.
newDescribeEnvironmentManagedActionHistoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEnvironmentManagedActionHistoryResponse
newDescribeEnvironmentManagedActionHistoryResponse
  pHttpStatus_ =
    DescribeEnvironmentManagedActionHistoryResponse'
      { nextToken =
          Core.Nothing,
        managedActionHistoryItems =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A pagination token that you pass to
-- DescribeEnvironmentManagedActionHistory to get the next page of results.
describeEnvironmentManagedActionHistoryResponse_nextToken :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse (Core.Maybe Core.Text)
describeEnvironmentManagedActionHistoryResponse_nextToken = Lens.lens (\DescribeEnvironmentManagedActionHistoryResponse' {nextToken} -> nextToken) (\s@DescribeEnvironmentManagedActionHistoryResponse' {} a -> s {nextToken = a} :: DescribeEnvironmentManagedActionHistoryResponse)

-- | A list of completed and failed managed actions.
describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse (Core.Maybe (Core.NonEmpty ManagedActionHistoryItem))
describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems = Lens.lens (\DescribeEnvironmentManagedActionHistoryResponse' {managedActionHistoryItems} -> managedActionHistoryItems) (\s@DescribeEnvironmentManagedActionHistoryResponse' {} a -> s {managedActionHistoryItems = a} :: DescribeEnvironmentManagedActionHistoryResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEnvironmentManagedActionHistoryResponse_httpStatus :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse Core.Int
describeEnvironmentManagedActionHistoryResponse_httpStatus = Lens.lens (\DescribeEnvironmentManagedActionHistoryResponse' {httpStatus} -> httpStatus) (\s@DescribeEnvironmentManagedActionHistoryResponse' {} a -> s {httpStatus = a} :: DescribeEnvironmentManagedActionHistoryResponse)

instance
  Core.NFData
    DescribeEnvironmentManagedActionHistoryResponse
