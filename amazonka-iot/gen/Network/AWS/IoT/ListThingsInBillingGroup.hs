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
-- Module      : Network.AWS.IoT.ListThingsInBillingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things you have added to the given billing group.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingsInBillingGroup
  ( -- * Creating a Request
    ListThingsInBillingGroup (..),
    newListThingsInBillingGroup,

    -- * Request Lenses
    listThingsInBillingGroup_nextToken,
    listThingsInBillingGroup_maxResults,
    listThingsInBillingGroup_billingGroupName,

    -- * Destructuring the Response
    ListThingsInBillingGroupResponse (..),
    newListThingsInBillingGroupResponse,

    -- * Response Lenses
    listThingsInBillingGroupResponse_nextToken,
    listThingsInBillingGroupResponse_things,
    listThingsInBillingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListThingsInBillingGroup' smart constructor.
data ListThingsInBillingGroup = ListThingsInBillingGroup'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the billing group.
    billingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingsInBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingsInBillingGroup_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listThingsInBillingGroup_maxResults' - The maximum number of results to return per request.
--
-- 'billingGroupName', 'listThingsInBillingGroup_billingGroupName' - The name of the billing group.
newListThingsInBillingGroup ::
  -- | 'billingGroupName'
  Prelude.Text ->
  ListThingsInBillingGroup
newListThingsInBillingGroup pBillingGroupName_ =
  ListThingsInBillingGroup'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      billingGroupName = pBillingGroupName_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingsInBillingGroup_nextToken :: Lens.Lens' ListThingsInBillingGroup (Prelude.Maybe Prelude.Text)
listThingsInBillingGroup_nextToken = Lens.lens (\ListThingsInBillingGroup' {nextToken} -> nextToken) (\s@ListThingsInBillingGroup' {} a -> s {nextToken = a} :: ListThingsInBillingGroup)

-- | The maximum number of results to return per request.
listThingsInBillingGroup_maxResults :: Lens.Lens' ListThingsInBillingGroup (Prelude.Maybe Prelude.Natural)
listThingsInBillingGroup_maxResults = Lens.lens (\ListThingsInBillingGroup' {maxResults} -> maxResults) (\s@ListThingsInBillingGroup' {} a -> s {maxResults = a} :: ListThingsInBillingGroup)

-- | The name of the billing group.
listThingsInBillingGroup_billingGroupName :: Lens.Lens' ListThingsInBillingGroup Prelude.Text
listThingsInBillingGroup_billingGroupName = Lens.lens (\ListThingsInBillingGroup' {billingGroupName} -> billingGroupName) (\s@ListThingsInBillingGroup' {} a -> s {billingGroupName = a} :: ListThingsInBillingGroup)

instance Core.AWSPager ListThingsInBillingGroup where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThingsInBillingGroupResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listThingsInBillingGroupResponse_things
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listThingsInBillingGroup_nextToken
          Lens..~ rs
          Lens.^? listThingsInBillingGroupResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListThingsInBillingGroup where
  type
    AWSResponse ListThingsInBillingGroup =
      ListThingsInBillingGroupResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingsInBillingGroupResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "things" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThingsInBillingGroup

instance Prelude.NFData ListThingsInBillingGroup

instance Core.ToHeaders ListThingsInBillingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListThingsInBillingGroup where
  toPath ListThingsInBillingGroup' {..} =
    Prelude.mconcat
      [ "/billing-groups/",
        Core.toBS billingGroupName,
        "/things"
      ]

instance Core.ToQuery ListThingsInBillingGroup where
  toQuery ListThingsInBillingGroup' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListThingsInBillingGroupResponse' smart constructor.
data ListThingsInBillingGroupResponse = ListThingsInBillingGroupResponse'
  { -- | The token to use to get the next set of results. Will not be returned if
    -- operation has returned all results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of things in the billing group.
    things :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingsInBillingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingsInBillingGroupResponse_nextToken' - The token to use to get the next set of results. Will not be returned if
-- operation has returned all results.
--
-- 'things', 'listThingsInBillingGroupResponse_things' - A list of things in the billing group.
--
-- 'httpStatus', 'listThingsInBillingGroupResponse_httpStatus' - The response's http status code.
newListThingsInBillingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListThingsInBillingGroupResponse
newListThingsInBillingGroupResponse pHttpStatus_ =
  ListThingsInBillingGroupResponse'
    { nextToken =
        Prelude.Nothing,
      things = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results. Will not be returned if
-- operation has returned all results.
listThingsInBillingGroupResponse_nextToken :: Lens.Lens' ListThingsInBillingGroupResponse (Prelude.Maybe Prelude.Text)
listThingsInBillingGroupResponse_nextToken = Lens.lens (\ListThingsInBillingGroupResponse' {nextToken} -> nextToken) (\s@ListThingsInBillingGroupResponse' {} a -> s {nextToken = a} :: ListThingsInBillingGroupResponse)

-- | A list of things in the billing group.
listThingsInBillingGroupResponse_things :: Lens.Lens' ListThingsInBillingGroupResponse (Prelude.Maybe [Prelude.Text])
listThingsInBillingGroupResponse_things = Lens.lens (\ListThingsInBillingGroupResponse' {things} -> things) (\s@ListThingsInBillingGroupResponse' {} a -> s {things = a} :: ListThingsInBillingGroupResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listThingsInBillingGroupResponse_httpStatus :: Lens.Lens' ListThingsInBillingGroupResponse Prelude.Int
listThingsInBillingGroupResponse_httpStatus = Lens.lens (\ListThingsInBillingGroupResponse' {httpStatus} -> httpStatus) (\s@ListThingsInBillingGroupResponse' {} a -> s {httpStatus = a} :: ListThingsInBillingGroupResponse)

instance
  Prelude.NFData
    ListThingsInBillingGroupResponse
