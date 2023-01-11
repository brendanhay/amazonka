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
-- Module      : Amazonka.CodeStarNotifications.ListNotificationRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the notification rules for an Amazon Web Services
-- account.
--
-- This operation returns paginated results.
module Amazonka.CodeStarNotifications.ListNotificationRules
  ( -- * Creating a Request
    ListNotificationRules (..),
    newListNotificationRules,

    -- * Request Lenses
    listNotificationRules_filters,
    listNotificationRules_maxResults,
    listNotificationRules_nextToken,

    -- * Destructuring the Response
    ListNotificationRulesResponse (..),
    newListNotificationRulesResponse,

    -- * Response Lenses
    listNotificationRulesResponse_nextToken,
    listNotificationRulesResponse_notificationRules,
    listNotificationRulesResponse_httpStatus,
  )
where

import Amazonka.CodeStarNotifications.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNotificationRules' smart constructor.
data ListNotificationRules = ListNotificationRules'
  { -- | The filters to use to return information by service or resource type.
    -- For valid values, see ListNotificationRulesFilter.
    --
    -- A filter with the same name can appear more than once when used with OR
    -- statements. Filters with different names should be applied with AND
    -- statements.
    filters :: Prelude.Maybe [ListNotificationRulesFilter],
    -- | A non-negative integer used to limit the number of returned results. The
    -- maximum number of results that can be returned is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotificationRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listNotificationRules_filters' - The filters to use to return information by service or resource type.
-- For valid values, see ListNotificationRulesFilter.
--
-- A filter with the same name can appear more than once when used with OR
-- statements. Filters with different names should be applied with AND
-- statements.
--
-- 'maxResults', 'listNotificationRules_maxResults' - A non-negative integer used to limit the number of returned results. The
-- maximum number of results that can be returned is 100.
--
-- 'nextToken', 'listNotificationRules_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
newListNotificationRules ::
  ListNotificationRules
newListNotificationRules =
  ListNotificationRules'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The filters to use to return information by service or resource type.
-- For valid values, see ListNotificationRulesFilter.
--
-- A filter with the same name can appear more than once when used with OR
-- statements. Filters with different names should be applied with AND
-- statements.
listNotificationRules_filters :: Lens.Lens' ListNotificationRules (Prelude.Maybe [ListNotificationRulesFilter])
listNotificationRules_filters = Lens.lens (\ListNotificationRules' {filters} -> filters) (\s@ListNotificationRules' {} a -> s {filters = a} :: ListNotificationRules) Prelude.. Lens.mapping Lens.coerced

-- | A non-negative integer used to limit the number of returned results. The
-- maximum number of results that can be returned is 100.
listNotificationRules_maxResults :: Lens.Lens' ListNotificationRules (Prelude.Maybe Prelude.Natural)
listNotificationRules_maxResults = Lens.lens (\ListNotificationRules' {maxResults} -> maxResults) (\s@ListNotificationRules' {} a -> s {maxResults = a} :: ListNotificationRules)

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
listNotificationRules_nextToken :: Lens.Lens' ListNotificationRules (Prelude.Maybe Prelude.Text)
listNotificationRules_nextToken = Lens.lens (\ListNotificationRules' {nextToken} -> nextToken) (\s@ListNotificationRules' {} a -> s {nextToken = a} :: ListNotificationRules)

instance Core.AWSPager ListNotificationRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNotificationRulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listNotificationRulesResponse_notificationRules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listNotificationRules_nextToken
          Lens..~ rs
          Lens.^? listNotificationRulesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListNotificationRules where
  type
    AWSResponse ListNotificationRules =
      ListNotificationRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNotificationRulesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "NotificationRules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNotificationRules where
  hashWithSalt _salt ListNotificationRules' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListNotificationRules where
  rnf ListNotificationRules' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListNotificationRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListNotificationRules where
  toJSON ListNotificationRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListNotificationRules where
  toPath = Prelude.const "/listNotificationRules"

instance Data.ToQuery ListNotificationRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNotificationRulesResponse' smart constructor.
data ListNotificationRulesResponse = ListNotificationRulesResponse'
  { -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of notification rules for the Amazon Web Services account, by
    -- Amazon Resource Name (ARN) and ID.
    notificationRules :: Prelude.Maybe [NotificationRuleSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotificationRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNotificationRulesResponse_nextToken' - An enumeration token that can be used in a request to return the next
-- batch of the results.
--
-- 'notificationRules', 'listNotificationRulesResponse_notificationRules' - The list of notification rules for the Amazon Web Services account, by
-- Amazon Resource Name (ARN) and ID.
--
-- 'httpStatus', 'listNotificationRulesResponse_httpStatus' - The response's http status code.
newListNotificationRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNotificationRulesResponse
newListNotificationRulesResponse pHttpStatus_ =
  ListNotificationRulesResponse'
    { nextToken =
        Prelude.Nothing,
      notificationRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
listNotificationRulesResponse_nextToken :: Lens.Lens' ListNotificationRulesResponse (Prelude.Maybe Prelude.Text)
listNotificationRulesResponse_nextToken = Lens.lens (\ListNotificationRulesResponse' {nextToken} -> nextToken) (\s@ListNotificationRulesResponse' {} a -> s {nextToken = a} :: ListNotificationRulesResponse)

-- | The list of notification rules for the Amazon Web Services account, by
-- Amazon Resource Name (ARN) and ID.
listNotificationRulesResponse_notificationRules :: Lens.Lens' ListNotificationRulesResponse (Prelude.Maybe [NotificationRuleSummary])
listNotificationRulesResponse_notificationRules = Lens.lens (\ListNotificationRulesResponse' {notificationRules} -> notificationRules) (\s@ListNotificationRulesResponse' {} a -> s {notificationRules = a} :: ListNotificationRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNotificationRulesResponse_httpStatus :: Lens.Lens' ListNotificationRulesResponse Prelude.Int
listNotificationRulesResponse_httpStatus = Lens.lens (\ListNotificationRulesResponse' {httpStatus} -> httpStatus) (\s@ListNotificationRulesResponse' {} a -> s {httpStatus = a} :: ListNotificationRulesResponse)

instance Prelude.NFData ListNotificationRulesResponse where
  rnf ListNotificationRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf notificationRules
      `Prelude.seq` Prelude.rnf httpStatus
