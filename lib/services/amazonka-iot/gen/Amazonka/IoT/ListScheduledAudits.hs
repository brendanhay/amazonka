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
-- Module      : Amazonka.IoT.ListScheduledAudits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of your scheduled audits.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListScheduledAudits>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListScheduledAudits
  ( -- * Creating a Request
    ListScheduledAudits (..),
    newListScheduledAudits,

    -- * Request Lenses
    listScheduledAudits_maxResults,
    listScheduledAudits_nextToken,

    -- * Destructuring the Response
    ListScheduledAuditsResponse (..),
    newListScheduledAuditsResponse,

    -- * Response Lenses
    listScheduledAuditsResponse_nextToken,
    listScheduledAuditsResponse_scheduledAudits,
    listScheduledAuditsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListScheduledAudits' smart constructor.
data ListScheduledAudits = ListScheduledAudits'
  { -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScheduledAudits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listScheduledAudits_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'nextToken', 'listScheduledAudits_nextToken' - The token for the next set of results.
newListScheduledAudits ::
  ListScheduledAudits
newListScheduledAudits =
  ListScheduledAudits'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return at one time. The default is 25.
listScheduledAudits_maxResults :: Lens.Lens' ListScheduledAudits (Prelude.Maybe Prelude.Natural)
listScheduledAudits_maxResults = Lens.lens (\ListScheduledAudits' {maxResults} -> maxResults) (\s@ListScheduledAudits' {} a -> s {maxResults = a} :: ListScheduledAudits)

-- | The token for the next set of results.
listScheduledAudits_nextToken :: Lens.Lens' ListScheduledAudits (Prelude.Maybe Prelude.Text)
listScheduledAudits_nextToken = Lens.lens (\ListScheduledAudits' {nextToken} -> nextToken) (\s@ListScheduledAudits' {} a -> s {nextToken = a} :: ListScheduledAudits)

instance Core.AWSPager ListScheduledAudits where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listScheduledAuditsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listScheduledAuditsResponse_scheduledAudits
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listScheduledAudits_nextToken
          Lens..~ rs
          Lens.^? listScheduledAuditsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListScheduledAudits where
  type
    AWSResponse ListScheduledAudits =
      ListScheduledAuditsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListScheduledAuditsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "scheduledAudits"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListScheduledAudits where
  hashWithSalt _salt ListScheduledAudits' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListScheduledAudits where
  rnf ListScheduledAudits' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListScheduledAudits where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListScheduledAudits where
  toPath = Prelude.const "/audit/scheduledaudits"

instance Data.ToQuery ListScheduledAudits where
  toQuery ListScheduledAudits' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListScheduledAuditsResponse' smart constructor.
data ListScheduledAuditsResponse = ListScheduledAuditsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of scheduled audits.
    scheduledAudits :: Prelude.Maybe [ScheduledAuditMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScheduledAuditsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listScheduledAuditsResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'scheduledAudits', 'listScheduledAuditsResponse_scheduledAudits' - The list of scheduled audits.
--
-- 'httpStatus', 'listScheduledAuditsResponse_httpStatus' - The response's http status code.
newListScheduledAuditsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListScheduledAuditsResponse
newListScheduledAuditsResponse pHttpStatus_ =
  ListScheduledAuditsResponse'
    { nextToken =
        Prelude.Nothing,
      scheduledAudits = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listScheduledAuditsResponse_nextToken :: Lens.Lens' ListScheduledAuditsResponse (Prelude.Maybe Prelude.Text)
listScheduledAuditsResponse_nextToken = Lens.lens (\ListScheduledAuditsResponse' {nextToken} -> nextToken) (\s@ListScheduledAuditsResponse' {} a -> s {nextToken = a} :: ListScheduledAuditsResponse)

-- | The list of scheduled audits.
listScheduledAuditsResponse_scheduledAudits :: Lens.Lens' ListScheduledAuditsResponse (Prelude.Maybe [ScheduledAuditMetadata])
listScheduledAuditsResponse_scheduledAudits = Lens.lens (\ListScheduledAuditsResponse' {scheduledAudits} -> scheduledAudits) (\s@ListScheduledAuditsResponse' {} a -> s {scheduledAudits = a} :: ListScheduledAuditsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listScheduledAuditsResponse_httpStatus :: Lens.Lens' ListScheduledAuditsResponse Prelude.Int
listScheduledAuditsResponse_httpStatus = Lens.lens (\ListScheduledAuditsResponse' {httpStatus} -> httpStatus) (\s@ListScheduledAuditsResponse' {} a -> s {httpStatus = a} :: ListScheduledAuditsResponse)

instance Prelude.NFData ListScheduledAuditsResponse where
  rnf ListScheduledAuditsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scheduledAudits
      `Prelude.seq` Prelude.rnf httpStatus
