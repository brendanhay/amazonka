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
-- Module      : Amazonka.IoT.ListThingsInBillingGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things you have added to the given billing group.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListThingsInBillingGroup>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListThingsInBillingGroup
  ( -- * Creating a Request
    ListThingsInBillingGroup (..),
    newListThingsInBillingGroup,

    -- * Request Lenses
    listThingsInBillingGroup_maxResults,
    listThingsInBillingGroup_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListThingsInBillingGroup' smart constructor.
data ListThingsInBillingGroup = ListThingsInBillingGroup'
  { -- | The maximum number of results to return per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listThingsInBillingGroup_maxResults' - The maximum number of results to return per request.
--
-- 'nextToken', 'listThingsInBillingGroup_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'billingGroupName', 'listThingsInBillingGroup_billingGroupName' - The name of the billing group.
newListThingsInBillingGroup ::
  -- | 'billingGroupName'
  Prelude.Text ->
  ListThingsInBillingGroup
newListThingsInBillingGroup pBillingGroupName_ =
  ListThingsInBillingGroup'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      billingGroupName = pBillingGroupName_
    }

-- | The maximum number of results to return per request.
listThingsInBillingGroup_maxResults :: Lens.Lens' ListThingsInBillingGroup (Prelude.Maybe Prelude.Natural)
listThingsInBillingGroup_maxResults = Lens.lens (\ListThingsInBillingGroup' {maxResults} -> maxResults) (\s@ListThingsInBillingGroup' {} a -> s {maxResults = a} :: ListThingsInBillingGroup)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingsInBillingGroup_nextToken :: Lens.Lens' ListThingsInBillingGroup (Prelude.Maybe Prelude.Text)
listThingsInBillingGroup_nextToken = Lens.lens (\ListThingsInBillingGroup' {nextToken} -> nextToken) (\s@ListThingsInBillingGroup' {} a -> s {nextToken = a} :: ListThingsInBillingGroup)

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingsInBillingGroupResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "things" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThingsInBillingGroup where
  hashWithSalt _salt ListThingsInBillingGroup' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` billingGroupName

instance Prelude.NFData ListThingsInBillingGroup where
  rnf ListThingsInBillingGroup' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf billingGroupName

instance Data.ToHeaders ListThingsInBillingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListThingsInBillingGroup where
  toPath ListThingsInBillingGroup' {..} =
    Prelude.mconcat
      [ "/billing-groups/",
        Data.toBS billingGroupName,
        "/things"
      ]

instance Data.ToQuery ListThingsInBillingGroup where
  toQuery ListThingsInBillingGroup' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
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
listThingsInBillingGroupResponse_things = Lens.lens (\ListThingsInBillingGroupResponse' {things} -> things) (\s@ListThingsInBillingGroupResponse' {} a -> s {things = a} :: ListThingsInBillingGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listThingsInBillingGroupResponse_httpStatus :: Lens.Lens' ListThingsInBillingGroupResponse Prelude.Int
listThingsInBillingGroupResponse_httpStatus = Lens.lens (\ListThingsInBillingGroupResponse' {httpStatus} -> httpStatus) (\s@ListThingsInBillingGroupResponse' {} a -> s {httpStatus = a} :: ListThingsInBillingGroupResponse)

instance
  Prelude.NFData
    ListThingsInBillingGroupResponse
  where
  rnf ListThingsInBillingGroupResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf things
      `Prelude.seq` Prelude.rnf httpStatus
