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
-- Module      : Amazonka.Chime.ListSipMediaApplications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the SIP media applications under the administrator\'s AWS account.
module Amazonka.Chime.ListSipMediaApplications
  ( -- * Creating a Request
    ListSipMediaApplications (..),
    newListSipMediaApplications,

    -- * Request Lenses
    listSipMediaApplications_nextToken,
    listSipMediaApplications_maxResults,

    -- * Destructuring the Response
    ListSipMediaApplicationsResponse (..),
    newListSipMediaApplicationsResponse,

    -- * Response Lenses
    listSipMediaApplicationsResponse_nextToken,
    listSipMediaApplicationsResponse_sipMediaApplications,
    listSipMediaApplicationsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSipMediaApplications' smart constructor.
data ListSipMediaApplications = ListSipMediaApplications'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call. Defaults to
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSipMediaApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSipMediaApplications_nextToken' - The token to use to retrieve the next page of results.
--
-- 'maxResults', 'listSipMediaApplications_maxResults' - The maximum number of results to return in a single call. Defaults to
-- 100.
newListSipMediaApplications ::
  ListSipMediaApplications
newListSipMediaApplications =
  ListSipMediaApplications'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to use to retrieve the next page of results.
listSipMediaApplications_nextToken :: Lens.Lens' ListSipMediaApplications (Prelude.Maybe Prelude.Text)
listSipMediaApplications_nextToken = Lens.lens (\ListSipMediaApplications' {nextToken} -> nextToken) (\s@ListSipMediaApplications' {} a -> s {nextToken = a} :: ListSipMediaApplications)

-- | The maximum number of results to return in a single call. Defaults to
-- 100.
listSipMediaApplications_maxResults :: Lens.Lens' ListSipMediaApplications (Prelude.Maybe Prelude.Natural)
listSipMediaApplications_maxResults = Lens.lens (\ListSipMediaApplications' {maxResults} -> maxResults) (\s@ListSipMediaApplications' {} a -> s {maxResults = a} :: ListSipMediaApplications)

instance Core.AWSRequest ListSipMediaApplications where
  type
    AWSResponse ListSipMediaApplications =
      ListSipMediaApplicationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSipMediaApplicationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "SipMediaApplications"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSipMediaApplications

instance Prelude.NFData ListSipMediaApplications

instance Core.ToHeaders ListSipMediaApplications where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListSipMediaApplications where
  toPath = Prelude.const "/sip-media-applications"

instance Core.ToQuery ListSipMediaApplications where
  toQuery ListSipMediaApplications' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListSipMediaApplicationsResponse' smart constructor.
data ListSipMediaApplicationsResponse = ListSipMediaApplicationsResponse'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of SIP media applications and application details.
    sipMediaApplications :: Prelude.Maybe [SipMediaApplication],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSipMediaApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSipMediaApplicationsResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'sipMediaApplications', 'listSipMediaApplicationsResponse_sipMediaApplications' - List of SIP media applications and application details.
--
-- 'httpStatus', 'listSipMediaApplicationsResponse_httpStatus' - The response's http status code.
newListSipMediaApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSipMediaApplicationsResponse
newListSipMediaApplicationsResponse pHttpStatus_ =
  ListSipMediaApplicationsResponse'
    { nextToken =
        Prelude.Nothing,
      sipMediaApplications = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results.
listSipMediaApplicationsResponse_nextToken :: Lens.Lens' ListSipMediaApplicationsResponse (Prelude.Maybe Prelude.Text)
listSipMediaApplicationsResponse_nextToken = Lens.lens (\ListSipMediaApplicationsResponse' {nextToken} -> nextToken) (\s@ListSipMediaApplicationsResponse' {} a -> s {nextToken = a} :: ListSipMediaApplicationsResponse)

-- | List of SIP media applications and application details.
listSipMediaApplicationsResponse_sipMediaApplications :: Lens.Lens' ListSipMediaApplicationsResponse (Prelude.Maybe [SipMediaApplication])
listSipMediaApplicationsResponse_sipMediaApplications = Lens.lens (\ListSipMediaApplicationsResponse' {sipMediaApplications} -> sipMediaApplications) (\s@ListSipMediaApplicationsResponse' {} a -> s {sipMediaApplications = a} :: ListSipMediaApplicationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSipMediaApplicationsResponse_httpStatus :: Lens.Lens' ListSipMediaApplicationsResponse Prelude.Int
listSipMediaApplicationsResponse_httpStatus = Lens.lens (\ListSipMediaApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListSipMediaApplicationsResponse' {} a -> s {httpStatus = a} :: ListSipMediaApplicationsResponse)

instance
  Prelude.NFData
    ListSipMediaApplicationsResponse
