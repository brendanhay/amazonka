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
-- Module      : Amazonka.LicenseManagerLinuxSubscriptions.ListLinuxSubscriptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Linux subscriptions that have been discovered. If you have
-- linked your organization, the returned results will include data
-- aggregated across your accounts in Organizations.
--
-- This operation returns paginated results.
module Amazonka.LicenseManagerLinuxSubscriptions.ListLinuxSubscriptions
  ( -- * Creating a Request
    ListLinuxSubscriptions (..),
    newListLinuxSubscriptions,

    -- * Request Lenses
    listLinuxSubscriptions_filters,
    listLinuxSubscriptions_maxResults,
    listLinuxSubscriptions_nextToken,

    -- * Destructuring the Response
    ListLinuxSubscriptionsResponse (..),
    newListLinuxSubscriptionsResponse,

    -- * Response Lenses
    listLinuxSubscriptionsResponse_nextToken,
    listLinuxSubscriptionsResponse_subscriptions,
    listLinuxSubscriptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerLinuxSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | NextToken length limit is half of ddb accepted limit. Increase this
-- limit if parameters in request increases.
--
-- /See:/ 'newListLinuxSubscriptions' smart constructor.
data ListLinuxSubscriptions = ListLinuxSubscriptions'
  { -- | An array of structures that you can use to filter the results to those
    -- that match one or more sets of key-value pairs that you specify. For
    -- example, you can filter by the name of @Subscription@ with an optional
    -- operator to see subscriptions that match, partially match, or don\'t
    -- match a certain subscription\'s name.
    --
    -- The valid names for this filter are:
    --
    -- -   @Subscription@
    --
    -- The valid Operators for this filter are:
    --
    -- -   @contains@
    --
    -- -   @equals@
    --
    -- -   @Notequal@
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLinuxSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listLinuxSubscriptions_filters' - An array of structures that you can use to filter the results to those
-- that match one or more sets of key-value pairs that you specify. For
-- example, you can filter by the name of @Subscription@ with an optional
-- operator to see subscriptions that match, partially match, or don\'t
-- match a certain subscription\'s name.
--
-- The valid names for this filter are:
--
-- -   @Subscription@
--
-- The valid Operators for this filter are:
--
-- -   @contains@
--
-- -   @equals@
--
-- -   @Notequal@
--
-- 'maxResults', 'listLinuxSubscriptions_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listLinuxSubscriptions_nextToken' - Token for the next set of results.
newListLinuxSubscriptions ::
  ListLinuxSubscriptions
newListLinuxSubscriptions =
  ListLinuxSubscriptions'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An array of structures that you can use to filter the results to those
-- that match one or more sets of key-value pairs that you specify. For
-- example, you can filter by the name of @Subscription@ with an optional
-- operator to see subscriptions that match, partially match, or don\'t
-- match a certain subscription\'s name.
--
-- The valid names for this filter are:
--
-- -   @Subscription@
--
-- The valid Operators for this filter are:
--
-- -   @contains@
--
-- -   @equals@
--
-- -   @Notequal@
listLinuxSubscriptions_filters :: Lens.Lens' ListLinuxSubscriptions (Prelude.Maybe [Filter])
listLinuxSubscriptions_filters = Lens.lens (\ListLinuxSubscriptions' {filters} -> filters) (\s@ListLinuxSubscriptions' {} a -> s {filters = a} :: ListLinuxSubscriptions) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listLinuxSubscriptions_maxResults :: Lens.Lens' ListLinuxSubscriptions (Prelude.Maybe Prelude.Int)
listLinuxSubscriptions_maxResults = Lens.lens (\ListLinuxSubscriptions' {maxResults} -> maxResults) (\s@ListLinuxSubscriptions' {} a -> s {maxResults = a} :: ListLinuxSubscriptions)

-- | Token for the next set of results.
listLinuxSubscriptions_nextToken :: Lens.Lens' ListLinuxSubscriptions (Prelude.Maybe Prelude.Text)
listLinuxSubscriptions_nextToken = Lens.lens (\ListLinuxSubscriptions' {nextToken} -> nextToken) (\s@ListLinuxSubscriptions' {} a -> s {nextToken = a} :: ListLinuxSubscriptions)

instance Core.AWSPager ListLinuxSubscriptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLinuxSubscriptionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLinuxSubscriptionsResponse_subscriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLinuxSubscriptions_nextToken
          Lens..~ rs
          Lens.^? listLinuxSubscriptionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLinuxSubscriptions where
  type
    AWSResponse ListLinuxSubscriptions =
      ListLinuxSubscriptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLinuxSubscriptionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Subscriptions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLinuxSubscriptions where
  hashWithSalt _salt ListLinuxSubscriptions' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLinuxSubscriptions where
  rnf ListLinuxSubscriptions' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLinuxSubscriptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLinuxSubscriptions where
  toJSON ListLinuxSubscriptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLinuxSubscriptions where
  toPath =
    Prelude.const
      "/subscription/ListLinuxSubscriptions"

instance Data.ToQuery ListLinuxSubscriptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLinuxSubscriptionsResponse' smart constructor.
data ListLinuxSubscriptionsResponse = ListLinuxSubscriptionsResponse'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array that contains subscription objects.
    subscriptions :: Prelude.Maybe [Subscription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLinuxSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLinuxSubscriptionsResponse_nextToken' - Token for the next set of results.
--
-- 'subscriptions', 'listLinuxSubscriptionsResponse_subscriptions' - An array that contains subscription objects.
--
-- 'httpStatus', 'listLinuxSubscriptionsResponse_httpStatus' - The response's http status code.
newListLinuxSubscriptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLinuxSubscriptionsResponse
newListLinuxSubscriptionsResponse pHttpStatus_ =
  ListLinuxSubscriptionsResponse'
    { nextToken =
        Prelude.Nothing,
      subscriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for the next set of results.
listLinuxSubscriptionsResponse_nextToken :: Lens.Lens' ListLinuxSubscriptionsResponse (Prelude.Maybe Prelude.Text)
listLinuxSubscriptionsResponse_nextToken = Lens.lens (\ListLinuxSubscriptionsResponse' {nextToken} -> nextToken) (\s@ListLinuxSubscriptionsResponse' {} a -> s {nextToken = a} :: ListLinuxSubscriptionsResponse)

-- | An array that contains subscription objects.
listLinuxSubscriptionsResponse_subscriptions :: Lens.Lens' ListLinuxSubscriptionsResponse (Prelude.Maybe [Subscription])
listLinuxSubscriptionsResponse_subscriptions = Lens.lens (\ListLinuxSubscriptionsResponse' {subscriptions} -> subscriptions) (\s@ListLinuxSubscriptionsResponse' {} a -> s {subscriptions = a} :: ListLinuxSubscriptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listLinuxSubscriptionsResponse_httpStatus :: Lens.Lens' ListLinuxSubscriptionsResponse Prelude.Int
listLinuxSubscriptionsResponse_httpStatus = Lens.lens (\ListLinuxSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@ListLinuxSubscriptionsResponse' {} a -> s {httpStatus = a} :: ListLinuxSubscriptionsResponse)

instance
  Prelude.NFData
    ListLinuxSubscriptionsResponse
  where
  rnf ListLinuxSubscriptionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subscriptions
      `Prelude.seq` Prelude.rnf httpStatus
