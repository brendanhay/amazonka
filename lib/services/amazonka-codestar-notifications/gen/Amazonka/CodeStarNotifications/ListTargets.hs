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
-- Module      : Amazonka.CodeStarNotifications.ListTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the notification rule targets for an AWS account.
--
-- This operation returns paginated results.
module Amazonka.CodeStarNotifications.ListTargets
  ( -- * Creating a Request
    ListTargets (..),
    newListTargets,

    -- * Request Lenses
    listTargets_filters,
    listTargets_nextToken,
    listTargets_maxResults,

    -- * Destructuring the Response
    ListTargetsResponse (..),
    newListTargetsResponse,

    -- * Response Lenses
    listTargetsResponse_nextToken,
    listTargetsResponse_targets,
    listTargetsResponse_httpStatus,
  )
where

import Amazonka.CodeStarNotifications.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTargets' smart constructor.
data ListTargets = ListTargets'
  { -- | The filters to use to return information by service or resource type.
    -- Valid filters include target type, target address, and target status.
    --
    -- A filter with the same name can appear more than once when used with OR
    -- statements. Filters with different names should be applied with AND
    -- statements.
    filters :: Prelude.Maybe [ListTargetsFilter],
    -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A non-negative integer used to limit the number of returned results. The
    -- maximum number of results that can be returned is 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listTargets_filters' - The filters to use to return information by service or resource type.
-- Valid filters include target type, target address, and target status.
--
-- A filter with the same name can appear more than once when used with OR
-- statements. Filters with different names should be applied with AND
-- statements.
--
-- 'nextToken', 'listTargets_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'maxResults', 'listTargets_maxResults' - A non-negative integer used to limit the number of returned results. The
-- maximum number of results that can be returned is 100.
newListTargets ::
  ListTargets
newListTargets =
  ListTargets'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The filters to use to return information by service or resource type.
-- Valid filters include target type, target address, and target status.
--
-- A filter with the same name can appear more than once when used with OR
-- statements. Filters with different names should be applied with AND
-- statements.
listTargets_filters :: Lens.Lens' ListTargets (Prelude.Maybe [ListTargetsFilter])
listTargets_filters = Lens.lens (\ListTargets' {filters} -> filters) (\s@ListTargets' {} a -> s {filters = a} :: ListTargets) Prelude.. Lens.mapping Lens.coerced

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
listTargets_nextToken :: Lens.Lens' ListTargets (Prelude.Maybe Prelude.Text)
listTargets_nextToken = Lens.lens (\ListTargets' {nextToken} -> nextToken) (\s@ListTargets' {} a -> s {nextToken = a} :: ListTargets)

-- | A non-negative integer used to limit the number of returned results. The
-- maximum number of results that can be returned is 100.
listTargets_maxResults :: Lens.Lens' ListTargets (Prelude.Maybe Prelude.Natural)
listTargets_maxResults = Lens.lens (\ListTargets' {maxResults} -> maxResults) (\s@ListTargets' {} a -> s {maxResults = a} :: ListTargets)

instance Core.AWSPager ListTargets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTargetsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTargetsResponse_targets Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTargets_nextToken
          Lens..~ rs
          Lens.^? listTargetsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTargets where
  type AWSResponse ListTargets = ListTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Targets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTargets

instance Prelude.NFData ListTargets

instance Core.ToHeaders ListTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTargets where
  toJSON ListTargets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListTargets where
  toPath = Prelude.const "/listTargets"

instance Core.ToQuery ListTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTargetsResponse' smart constructor.
data ListTargetsResponse = ListTargetsResponse'
  { -- | An enumeration token that can be used in a request to return the next
    -- batch of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of notification rule targets.
    targets :: Prelude.Maybe [TargetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTargetsResponse_nextToken' - An enumeration token that can be used in a request to return the next
-- batch of results.
--
-- 'targets', 'listTargetsResponse_targets' - The list of notification rule targets.
--
-- 'httpStatus', 'listTargetsResponse_httpStatus' - The response's http status code.
newListTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTargetsResponse
newListTargetsResponse pHttpStatus_ =
  ListTargetsResponse'
    { nextToken = Prelude.Nothing,
      targets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An enumeration token that can be used in a request to return the next
-- batch of results.
listTargetsResponse_nextToken :: Lens.Lens' ListTargetsResponse (Prelude.Maybe Prelude.Text)
listTargetsResponse_nextToken = Lens.lens (\ListTargetsResponse' {nextToken} -> nextToken) (\s@ListTargetsResponse' {} a -> s {nextToken = a} :: ListTargetsResponse)

-- | The list of notification rule targets.
listTargetsResponse_targets :: Lens.Lens' ListTargetsResponse (Prelude.Maybe [TargetSummary])
listTargetsResponse_targets = Lens.lens (\ListTargetsResponse' {targets} -> targets) (\s@ListTargetsResponse' {} a -> s {targets = a} :: ListTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTargetsResponse_httpStatus :: Lens.Lens' ListTargetsResponse Prelude.Int
listTargetsResponse_httpStatus = Lens.lens (\ListTargetsResponse' {httpStatus} -> httpStatus) (\s@ListTargetsResponse' {} a -> s {httpStatus = a} :: ListTargetsResponse)

instance Prelude.NFData ListTargetsResponse
