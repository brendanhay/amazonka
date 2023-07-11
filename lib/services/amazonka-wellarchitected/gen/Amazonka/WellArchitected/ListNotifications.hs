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
-- Module      : Amazonka.WellArchitected.ListNotifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List lens notifications.
module Amazonka.WellArchitected.ListNotifications
  ( -- * Creating a Request
    ListNotifications (..),
    newListNotifications,

    -- * Request Lenses
    listNotifications_maxResults,
    listNotifications_nextToken,
    listNotifications_workloadId,

    -- * Destructuring the Response
    ListNotificationsResponse (..),
    newListNotificationsResponse,

    -- * Response Lenses
    listNotificationsResponse_nextToken,
    listNotificationsResponse_notificationSummaries,
    listNotificationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newListNotifications' smart constructor.
data ListNotifications = ListNotifications'
  { -- | The maximum number of results to return for this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listNotifications_maxResults' - The maximum number of results to return for this request.
--
-- 'nextToken', 'listNotifications_nextToken' - Undocumented member.
--
-- 'workloadId', 'listNotifications_workloadId' - Undocumented member.
newListNotifications ::
  ListNotifications
newListNotifications =
  ListNotifications'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workloadId = Prelude.Nothing
    }

-- | The maximum number of results to return for this request.
listNotifications_maxResults :: Lens.Lens' ListNotifications (Prelude.Maybe Prelude.Natural)
listNotifications_maxResults = Lens.lens (\ListNotifications' {maxResults} -> maxResults) (\s@ListNotifications' {} a -> s {maxResults = a} :: ListNotifications)

-- | Undocumented member.
listNotifications_nextToken :: Lens.Lens' ListNotifications (Prelude.Maybe Prelude.Text)
listNotifications_nextToken = Lens.lens (\ListNotifications' {nextToken} -> nextToken) (\s@ListNotifications' {} a -> s {nextToken = a} :: ListNotifications)

-- | Undocumented member.
listNotifications_workloadId :: Lens.Lens' ListNotifications (Prelude.Maybe Prelude.Text)
listNotifications_workloadId = Lens.lens (\ListNotifications' {workloadId} -> workloadId) (\s@ListNotifications' {} a -> s {workloadId = a} :: ListNotifications)

instance Core.AWSRequest ListNotifications where
  type
    AWSResponse ListNotifications =
      ListNotificationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNotificationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "NotificationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNotifications where
  hashWithSalt _salt ListNotifications' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workloadId

instance Prelude.NFData ListNotifications where
  rnf ListNotifications' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workloadId

instance Data.ToHeaders ListNotifications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListNotifications where
  toJSON ListNotifications' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("WorkloadId" Data..=) Prelude.<$> workloadId
          ]
      )

instance Data.ToPath ListNotifications where
  toPath = Prelude.const "/notifications"

instance Data.ToQuery ListNotifications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNotificationsResponse' smart constructor.
data ListNotificationsResponse = ListNotificationsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of lens notification summaries in a workload.
    notificationSummaries :: Prelude.Maybe [NotificationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNotificationsResponse_nextToken' - Undocumented member.
--
-- 'notificationSummaries', 'listNotificationsResponse_notificationSummaries' - List of lens notification summaries in a workload.
--
-- 'httpStatus', 'listNotificationsResponse_httpStatus' - The response's http status code.
newListNotificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNotificationsResponse
newListNotificationsResponse pHttpStatus_ =
  ListNotificationsResponse'
    { nextToken =
        Prelude.Nothing,
      notificationSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listNotificationsResponse_nextToken :: Lens.Lens' ListNotificationsResponse (Prelude.Maybe Prelude.Text)
listNotificationsResponse_nextToken = Lens.lens (\ListNotificationsResponse' {nextToken} -> nextToken) (\s@ListNotificationsResponse' {} a -> s {nextToken = a} :: ListNotificationsResponse)

-- | List of lens notification summaries in a workload.
listNotificationsResponse_notificationSummaries :: Lens.Lens' ListNotificationsResponse (Prelude.Maybe [NotificationSummary])
listNotificationsResponse_notificationSummaries = Lens.lens (\ListNotificationsResponse' {notificationSummaries} -> notificationSummaries) (\s@ListNotificationsResponse' {} a -> s {notificationSummaries = a} :: ListNotificationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNotificationsResponse_httpStatus :: Lens.Lens' ListNotificationsResponse Prelude.Int
listNotificationsResponse_httpStatus = Lens.lens (\ListNotificationsResponse' {httpStatus} -> httpStatus) (\s@ListNotificationsResponse' {} a -> s {httpStatus = a} :: ListNotificationsResponse)

instance Prelude.NFData ListNotificationsResponse where
  rnf ListNotificationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf notificationSummaries
      `Prelude.seq` Prelude.rnf httpStatus
