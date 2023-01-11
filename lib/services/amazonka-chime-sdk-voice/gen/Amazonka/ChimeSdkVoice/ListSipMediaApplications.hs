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
-- Module      : Amazonka.ChimeSdkVoice.ListSipMediaApplications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
--
-- This operation returns paginated results.
module Amazonka.ChimeSdkVoice.ListSipMediaApplications
  ( -- * Creating a Request
    ListSipMediaApplications (..),
    newListSipMediaApplications,

    -- * Request Lenses
    listSipMediaApplications_maxResults,
    listSipMediaApplications_nextToken,

    -- * Destructuring the Response
    ListSipMediaApplicationsResponse (..),
    newListSipMediaApplicationsResponse,

    -- * Response Lenses
    listSipMediaApplicationsResponse_nextToken,
    listSipMediaApplicationsResponse_sipMediaApplications,
    listSipMediaApplicationsResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSipMediaApplications' smart constructor.
data ListSipMediaApplications = ListSipMediaApplications'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listSipMediaApplications_maxResults' - Undocumented member.
--
-- 'nextToken', 'listSipMediaApplications_nextToken' - Undocumented member.
newListSipMediaApplications ::
  ListSipMediaApplications
newListSipMediaApplications =
  ListSipMediaApplications'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
listSipMediaApplications_maxResults :: Lens.Lens' ListSipMediaApplications (Prelude.Maybe Prelude.Natural)
listSipMediaApplications_maxResults = Lens.lens (\ListSipMediaApplications' {maxResults} -> maxResults) (\s@ListSipMediaApplications' {} a -> s {maxResults = a} :: ListSipMediaApplications)

-- | Undocumented member.
listSipMediaApplications_nextToken :: Lens.Lens' ListSipMediaApplications (Prelude.Maybe Prelude.Text)
listSipMediaApplications_nextToken = Lens.lens (\ListSipMediaApplications' {nextToken} -> nextToken) (\s@ListSipMediaApplications' {} a -> s {nextToken = a} :: ListSipMediaApplications)

instance Core.AWSPager ListSipMediaApplications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSipMediaApplicationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSipMediaApplicationsResponse_sipMediaApplications
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSipMediaApplications_nextToken
          Lens..~ rs
          Lens.^? listSipMediaApplicationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSipMediaApplications where
  type
    AWSResponse ListSipMediaApplications =
      ListSipMediaApplicationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSipMediaApplicationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "SipMediaApplications"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSipMediaApplications where
  hashWithSalt _salt ListSipMediaApplications' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSipMediaApplications where
  rnf ListSipMediaApplications' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSipMediaApplications where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListSipMediaApplications where
  toPath = Prelude.const "/sip-media-applications"

instance Data.ToQuery ListSipMediaApplications where
  toQuery ListSipMediaApplications' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListSipMediaApplicationsResponse' smart constructor.
data ListSipMediaApplicationsResponse = ListSipMediaApplicationsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'nextToken', 'listSipMediaApplicationsResponse_nextToken' - Undocumented member.
--
-- 'sipMediaApplications', 'listSipMediaApplicationsResponse_sipMediaApplications' - Undocumented member.
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

-- | Undocumented member.
listSipMediaApplicationsResponse_nextToken :: Lens.Lens' ListSipMediaApplicationsResponse (Prelude.Maybe Prelude.Text)
listSipMediaApplicationsResponse_nextToken = Lens.lens (\ListSipMediaApplicationsResponse' {nextToken} -> nextToken) (\s@ListSipMediaApplicationsResponse' {} a -> s {nextToken = a} :: ListSipMediaApplicationsResponse)

-- | Undocumented member.
listSipMediaApplicationsResponse_sipMediaApplications :: Lens.Lens' ListSipMediaApplicationsResponse (Prelude.Maybe [SipMediaApplication])
listSipMediaApplicationsResponse_sipMediaApplications = Lens.lens (\ListSipMediaApplicationsResponse' {sipMediaApplications} -> sipMediaApplications) (\s@ListSipMediaApplicationsResponse' {} a -> s {sipMediaApplications = a} :: ListSipMediaApplicationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSipMediaApplicationsResponse_httpStatus :: Lens.Lens' ListSipMediaApplicationsResponse Prelude.Int
listSipMediaApplicationsResponse_httpStatus = Lens.lens (\ListSipMediaApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListSipMediaApplicationsResponse' {} a -> s {httpStatus = a} :: ListSipMediaApplicationsResponse)

instance
  Prelude.NFData
    ListSipMediaApplicationsResponse
  where
  rnf ListSipMediaApplicationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sipMediaApplications
      `Prelude.seq` Prelude.rnf httpStatus
