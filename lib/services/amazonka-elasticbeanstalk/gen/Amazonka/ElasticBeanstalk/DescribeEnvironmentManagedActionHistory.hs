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
-- Module      : Amazonka.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists an environment\'s completed and failed managed actions.
--
-- This operation returns paginated results.
module Amazonka.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
  ( -- * Creating a Request
    DescribeEnvironmentManagedActionHistory (..),
    newDescribeEnvironmentManagedActionHistory,

    -- * Request Lenses
    describeEnvironmentManagedActionHistory_nextToken,
    describeEnvironmentManagedActionHistory_environmentName,
    describeEnvironmentManagedActionHistory_maxItems,
    describeEnvironmentManagedActionHistory_environmentId,

    -- * Destructuring the Response
    DescribeEnvironmentManagedActionHistoryResponse (..),
    newDescribeEnvironmentManagedActionHistoryResponse,

    -- * Response Lenses
    describeEnvironmentManagedActionHistoryResponse_nextToken,
    describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems,
    describeEnvironmentManagedActionHistoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to list completed and failed managed actions.
--
-- /See:/ 'newDescribeEnvironmentManagedActionHistory' smart constructor.
data DescribeEnvironmentManagedActionHistory = DescribeEnvironmentManagedActionHistory'
  { -- | The pagination token returned by a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the target environment.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for a single request.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | The environment ID of the target environment.
    environmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'environmentName', 'describeEnvironmentManagedActionHistory_environmentName' - The name of the target environment.
--
-- 'maxItems', 'describeEnvironmentManagedActionHistory_maxItems' - The maximum number of items to return for a single request.
--
-- 'environmentId', 'describeEnvironmentManagedActionHistory_environmentId' - The environment ID of the target environment.
newDescribeEnvironmentManagedActionHistory ::
  DescribeEnvironmentManagedActionHistory
newDescribeEnvironmentManagedActionHistory =
  DescribeEnvironmentManagedActionHistory'
    { nextToken =
        Prelude.Nothing,
      environmentName = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      environmentId = Prelude.Nothing
    }

-- | The pagination token returned by a previous request.
describeEnvironmentManagedActionHistory_nextToken :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Prelude.Maybe Prelude.Text)
describeEnvironmentManagedActionHistory_nextToken = Lens.lens (\DescribeEnvironmentManagedActionHistory' {nextToken} -> nextToken) (\s@DescribeEnvironmentManagedActionHistory' {} a -> s {nextToken = a} :: DescribeEnvironmentManagedActionHistory)

-- | The name of the target environment.
describeEnvironmentManagedActionHistory_environmentName :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Prelude.Maybe Prelude.Text)
describeEnvironmentManagedActionHistory_environmentName = Lens.lens (\DescribeEnvironmentManagedActionHistory' {environmentName} -> environmentName) (\s@DescribeEnvironmentManagedActionHistory' {} a -> s {environmentName = a} :: DescribeEnvironmentManagedActionHistory)

-- | The maximum number of items to return for a single request.
describeEnvironmentManagedActionHistory_maxItems :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Prelude.Maybe Prelude.Natural)
describeEnvironmentManagedActionHistory_maxItems = Lens.lens (\DescribeEnvironmentManagedActionHistory' {maxItems} -> maxItems) (\s@DescribeEnvironmentManagedActionHistory' {} a -> s {maxItems = a} :: DescribeEnvironmentManagedActionHistory)

-- | The environment ID of the target environment.
describeEnvironmentManagedActionHistory_environmentId :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Prelude.Maybe Prelude.Text)
describeEnvironmentManagedActionHistory_environmentId = Lens.lens (\DescribeEnvironmentManagedActionHistory' {environmentId} -> environmentId) (\s@DescribeEnvironmentManagedActionHistory' {} a -> s {environmentId = a} :: DescribeEnvironmentManagedActionHistory)

instance
  Core.AWSPager
    DescribeEnvironmentManagedActionHistory
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEnvironmentManagedActionHistoryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEnvironmentManagedActionHistory_nextToken
          Lens..~ rs
            Lens.^? describeEnvironmentManagedActionHistoryResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeEnvironmentManagedActionHistory
  where
  type
    AWSResponse
      DescribeEnvironmentManagedActionHistory =
      DescribeEnvironmentManagedActionHistoryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentManagedActionHistoryResult"
      ( \s h x ->
          DescribeEnvironmentManagedActionHistoryResponse'
            Prelude.<$> (x Core..@? "NextToken")
              Prelude.<*> ( x Core..@? "ManagedActionHistoryItems"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList1 "member")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEnvironmentManagedActionHistory
  where
  hashWithSalt
    _salt
    DescribeEnvironmentManagedActionHistory' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` environmentName
        `Prelude.hashWithSalt` maxItems
        `Prelude.hashWithSalt` environmentId

instance
  Prelude.NFData
    DescribeEnvironmentManagedActionHistory
  where
  rnf DescribeEnvironmentManagedActionHistory' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf environmentId

instance
  Core.ToHeaders
    DescribeEnvironmentManagedActionHistory
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeEnvironmentManagedActionHistory
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeEnvironmentManagedActionHistory
  where
  toQuery DescribeEnvironmentManagedActionHistory' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeEnvironmentManagedActionHistory" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "EnvironmentName" Core.=: environmentName,
        "MaxItems" Core.=: maxItems,
        "EnvironmentId" Core.=: environmentId
      ]

-- | A result message containing a list of completed and failed managed
-- actions.
--
-- /See:/ 'newDescribeEnvironmentManagedActionHistoryResponse' smart constructor.
data DescribeEnvironmentManagedActionHistoryResponse = DescribeEnvironmentManagedActionHistoryResponse'
  { -- | A pagination token that you pass to
    -- DescribeEnvironmentManagedActionHistory to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of completed and failed managed actions.
    managedActionHistoryItems :: Prelude.Maybe (Prelude.NonEmpty ManagedActionHistoryItem),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeEnvironmentManagedActionHistoryResponse
newDescribeEnvironmentManagedActionHistoryResponse
  pHttpStatus_ =
    DescribeEnvironmentManagedActionHistoryResponse'
      { nextToken =
          Prelude.Nothing,
        managedActionHistoryItems =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A pagination token that you pass to
-- DescribeEnvironmentManagedActionHistory to get the next page of results.
describeEnvironmentManagedActionHistoryResponse_nextToken :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse (Prelude.Maybe Prelude.Text)
describeEnvironmentManagedActionHistoryResponse_nextToken = Lens.lens (\DescribeEnvironmentManagedActionHistoryResponse' {nextToken} -> nextToken) (\s@DescribeEnvironmentManagedActionHistoryResponse' {} a -> s {nextToken = a} :: DescribeEnvironmentManagedActionHistoryResponse)

-- | A list of completed and failed managed actions.
describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse (Prelude.Maybe (Prelude.NonEmpty ManagedActionHistoryItem))
describeEnvironmentManagedActionHistoryResponse_managedActionHistoryItems = Lens.lens (\DescribeEnvironmentManagedActionHistoryResponse' {managedActionHistoryItems} -> managedActionHistoryItems) (\s@DescribeEnvironmentManagedActionHistoryResponse' {} a -> s {managedActionHistoryItems = a} :: DescribeEnvironmentManagedActionHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEnvironmentManagedActionHistoryResponse_httpStatus :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse Prelude.Int
describeEnvironmentManagedActionHistoryResponse_httpStatus = Lens.lens (\DescribeEnvironmentManagedActionHistoryResponse' {httpStatus} -> httpStatus) (\s@DescribeEnvironmentManagedActionHistoryResponse' {} a -> s {httpStatus = a} :: DescribeEnvironmentManagedActionHistoryResponse)

instance
  Prelude.NFData
    DescribeEnvironmentManagedActionHistoryResponse
  where
  rnf
    DescribeEnvironmentManagedActionHistoryResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf managedActionHistoryItems
        `Prelude.seq` Prelude.rnf httpStatus
