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
-- Module      : Amazonka.WellArchitected.ListProfileNotifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List profile notifications.
module Amazonka.WellArchitected.ListProfileNotifications
  ( -- * Creating a Request
    ListProfileNotifications (..),
    newListProfileNotifications,

    -- * Request Lenses
    listProfileNotifications_maxResults,
    listProfileNotifications_nextToken,
    listProfileNotifications_workloadId,

    -- * Destructuring the Response
    ListProfileNotificationsResponse (..),
    newListProfileNotificationsResponse,

    -- * Response Lenses
    listProfileNotificationsResponse_nextToken,
    listProfileNotificationsResponse_notificationSummaries,
    listProfileNotificationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newListProfileNotifications' smart constructor.
data ListProfileNotifications = ListProfileNotifications'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileNotifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProfileNotifications_maxResults' - Undocumented member.
--
-- 'nextToken', 'listProfileNotifications_nextToken' - Undocumented member.
--
-- 'workloadId', 'listProfileNotifications_workloadId' - Undocumented member.
newListProfileNotifications ::
  ListProfileNotifications
newListProfileNotifications =
  ListProfileNotifications'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workloadId = Prelude.Nothing
    }

-- | Undocumented member.
listProfileNotifications_maxResults :: Lens.Lens' ListProfileNotifications (Prelude.Maybe Prelude.Natural)
listProfileNotifications_maxResults = Lens.lens (\ListProfileNotifications' {maxResults} -> maxResults) (\s@ListProfileNotifications' {} a -> s {maxResults = a} :: ListProfileNotifications)

-- | Undocumented member.
listProfileNotifications_nextToken :: Lens.Lens' ListProfileNotifications (Prelude.Maybe Prelude.Text)
listProfileNotifications_nextToken = Lens.lens (\ListProfileNotifications' {nextToken} -> nextToken) (\s@ListProfileNotifications' {} a -> s {nextToken = a} :: ListProfileNotifications)

-- | Undocumented member.
listProfileNotifications_workloadId :: Lens.Lens' ListProfileNotifications (Prelude.Maybe Prelude.Text)
listProfileNotifications_workloadId = Lens.lens (\ListProfileNotifications' {workloadId} -> workloadId) (\s@ListProfileNotifications' {} a -> s {workloadId = a} :: ListProfileNotifications)

instance Core.AWSRequest ListProfileNotifications where
  type
    AWSResponse ListProfileNotifications =
      ListProfileNotificationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProfileNotificationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "NotificationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProfileNotifications where
  hashWithSalt _salt ListProfileNotifications' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workloadId

instance Prelude.NFData ListProfileNotifications where
  rnf ListProfileNotifications' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workloadId

instance Data.ToHeaders ListProfileNotifications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProfileNotifications where
  toPath = Prelude.const "/profileNotifications/"

instance Data.ToQuery ListProfileNotifications where
  toQuery ListProfileNotifications' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "WorkloadId" Data.=: workloadId
      ]

-- | /See:/ 'newListProfileNotificationsResponse' smart constructor.
data ListProfileNotificationsResponse = ListProfileNotificationsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | Notification summaries.
    notificationSummaries :: Prelude.Maybe [ProfileNotificationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileNotificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProfileNotificationsResponse_nextToken' - Undocumented member.
--
-- 'notificationSummaries', 'listProfileNotificationsResponse_notificationSummaries' - Notification summaries.
--
-- 'httpStatus', 'listProfileNotificationsResponse_httpStatus' - The response's http status code.
newListProfileNotificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProfileNotificationsResponse
newListProfileNotificationsResponse pHttpStatus_ =
  ListProfileNotificationsResponse'
    { nextToken =
        Prelude.Nothing,
      notificationSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listProfileNotificationsResponse_nextToken :: Lens.Lens' ListProfileNotificationsResponse (Prelude.Maybe Prelude.Text)
listProfileNotificationsResponse_nextToken = Lens.lens (\ListProfileNotificationsResponse' {nextToken} -> nextToken) (\s@ListProfileNotificationsResponse' {} a -> s {nextToken = a} :: ListProfileNotificationsResponse)

-- | Notification summaries.
listProfileNotificationsResponse_notificationSummaries :: Lens.Lens' ListProfileNotificationsResponse (Prelude.Maybe [ProfileNotificationSummary])
listProfileNotificationsResponse_notificationSummaries = Lens.lens (\ListProfileNotificationsResponse' {notificationSummaries} -> notificationSummaries) (\s@ListProfileNotificationsResponse' {} a -> s {notificationSummaries = a} :: ListProfileNotificationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProfileNotificationsResponse_httpStatus :: Lens.Lens' ListProfileNotificationsResponse Prelude.Int
listProfileNotificationsResponse_httpStatus = Lens.lens (\ListProfileNotificationsResponse' {httpStatus} -> httpStatus) (\s@ListProfileNotificationsResponse' {} a -> s {httpStatus = a} :: ListProfileNotificationsResponse)

instance
  Prelude.NFData
    ListProfileNotificationsResponse
  where
  rnf ListProfileNotificationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf notificationSummaries
      `Prelude.seq` Prelude.rnf httpStatus
