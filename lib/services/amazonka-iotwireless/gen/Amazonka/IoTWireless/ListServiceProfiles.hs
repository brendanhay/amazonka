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
-- Module      : Amazonka.IoTWireless.ListServiceProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the service profiles registered to your AWS account.
module Amazonka.IoTWireless.ListServiceProfiles
  ( -- * Creating a Request
    ListServiceProfiles (..),
    newListServiceProfiles,

    -- * Request Lenses
    listServiceProfiles_nextToken,
    listServiceProfiles_maxResults,

    -- * Destructuring the Response
    ListServiceProfilesResponse (..),
    newListServiceProfilesResponse,

    -- * Response Lenses
    listServiceProfilesResponse_serviceProfileList,
    listServiceProfilesResponse_nextToken,
    listServiceProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListServiceProfiles' smart constructor.
data ListServiceProfiles = ListServiceProfiles'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceProfiles_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listServiceProfiles_maxResults' - The maximum number of results to return in this operation.
newListServiceProfiles ::
  ListServiceProfiles
newListServiceProfiles =
  ListServiceProfiles'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listServiceProfiles_nextToken :: Lens.Lens' ListServiceProfiles (Prelude.Maybe Prelude.Text)
listServiceProfiles_nextToken = Lens.lens (\ListServiceProfiles' {nextToken} -> nextToken) (\s@ListServiceProfiles' {} a -> s {nextToken = a} :: ListServiceProfiles)

-- | The maximum number of results to return in this operation.
listServiceProfiles_maxResults :: Lens.Lens' ListServiceProfiles (Prelude.Maybe Prelude.Natural)
listServiceProfiles_maxResults = Lens.lens (\ListServiceProfiles' {maxResults} -> maxResults) (\s@ListServiceProfiles' {} a -> s {maxResults = a} :: ListServiceProfiles)

instance Core.AWSRequest ListServiceProfiles where
  type
    AWSResponse ListServiceProfiles =
      ListServiceProfilesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceProfilesResponse'
            Prelude.<$> ( x Core..?> "ServiceProfileList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListServiceProfiles where
  hashWithSalt _salt ListServiceProfiles' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListServiceProfiles where
  rnf ListServiceProfiles' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListServiceProfiles where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListServiceProfiles where
  toPath = Prelude.const "/service-profiles"

instance Core.ToQuery ListServiceProfiles where
  toQuery ListServiceProfiles' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListServiceProfilesResponse' smart constructor.
data ListServiceProfilesResponse = ListServiceProfilesResponse'
  { -- | The list of service profiles.
    serviceProfileList :: Prelude.Maybe [ServiceProfile],
    -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceProfileList', 'listServiceProfilesResponse_serviceProfileList' - The list of service profiles.
--
-- 'nextToken', 'listServiceProfilesResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'httpStatus', 'listServiceProfilesResponse_httpStatus' - The response's http status code.
newListServiceProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceProfilesResponse
newListServiceProfilesResponse pHttpStatus_ =
  ListServiceProfilesResponse'
    { serviceProfileList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of service profiles.
listServiceProfilesResponse_serviceProfileList :: Lens.Lens' ListServiceProfilesResponse (Prelude.Maybe [ServiceProfile])
listServiceProfilesResponse_serviceProfileList = Lens.lens (\ListServiceProfilesResponse' {serviceProfileList} -> serviceProfileList) (\s@ListServiceProfilesResponse' {} a -> s {serviceProfileList = a} :: ListServiceProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listServiceProfilesResponse_nextToken :: Lens.Lens' ListServiceProfilesResponse (Prelude.Maybe Prelude.Text)
listServiceProfilesResponse_nextToken = Lens.lens (\ListServiceProfilesResponse' {nextToken} -> nextToken) (\s@ListServiceProfilesResponse' {} a -> s {nextToken = a} :: ListServiceProfilesResponse)

-- | The response's http status code.
listServiceProfilesResponse_httpStatus :: Lens.Lens' ListServiceProfilesResponse Prelude.Int
listServiceProfilesResponse_httpStatus = Lens.lens (\ListServiceProfilesResponse' {httpStatus} -> httpStatus) (\s@ListServiceProfilesResponse' {} a -> s {httpStatus = a} :: ListServiceProfilesResponse)

instance Prelude.NFData ListServiceProfilesResponse where
  rnf ListServiceProfilesResponse' {..} =
    Prelude.rnf serviceProfileList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
