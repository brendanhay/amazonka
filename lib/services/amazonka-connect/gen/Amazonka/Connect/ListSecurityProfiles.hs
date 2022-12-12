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
-- Module      : Amazonka.Connect.ListSecurityProfiles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the security profiles for the
-- specified Amazon Connect instance.
--
-- For more information about security profiles, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/connect-security-profiles.html Security Profiles>
-- in the /Amazon Connect Administrator Guide/.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListSecurityProfiles
  ( -- * Creating a Request
    ListSecurityProfiles (..),
    newListSecurityProfiles,

    -- * Request Lenses
    listSecurityProfiles_maxResults,
    listSecurityProfiles_nextToken,
    listSecurityProfiles_instanceId,

    -- * Destructuring the Response
    ListSecurityProfilesResponse (..),
    newListSecurityProfilesResponse,

    -- * Response Lenses
    listSecurityProfilesResponse_nextToken,
    listSecurityProfilesResponse_securityProfileSummaryList,
    listSecurityProfilesResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSecurityProfiles' smart constructor.
data ListSecurityProfiles = ListSecurityProfiles'
  { -- | The maximum number of results to return per page. The default MaxResult
    -- size is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSecurityProfiles_maxResults' - The maximum number of results to return per page. The default MaxResult
-- size is 100.
--
-- 'nextToken', 'listSecurityProfiles_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listSecurityProfiles_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newListSecurityProfiles ::
  -- | 'instanceId'
  Prelude.Text ->
  ListSecurityProfiles
newListSecurityProfiles pInstanceId_ =
  ListSecurityProfiles'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page. The default MaxResult
-- size is 100.
listSecurityProfiles_maxResults :: Lens.Lens' ListSecurityProfiles (Prelude.Maybe Prelude.Natural)
listSecurityProfiles_maxResults = Lens.lens (\ListSecurityProfiles' {maxResults} -> maxResults) (\s@ListSecurityProfiles' {} a -> s {maxResults = a} :: ListSecurityProfiles)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listSecurityProfiles_nextToken :: Lens.Lens' ListSecurityProfiles (Prelude.Maybe Prelude.Text)
listSecurityProfiles_nextToken = Lens.lens (\ListSecurityProfiles' {nextToken} -> nextToken) (\s@ListSecurityProfiles' {} a -> s {nextToken = a} :: ListSecurityProfiles)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listSecurityProfiles_instanceId :: Lens.Lens' ListSecurityProfiles Prelude.Text
listSecurityProfiles_instanceId = Lens.lens (\ListSecurityProfiles' {instanceId} -> instanceId) (\s@ListSecurityProfiles' {} a -> s {instanceId = a} :: ListSecurityProfiles)

instance Core.AWSPager ListSecurityProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesResponse_securityProfileSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSecurityProfiles_nextToken
          Lens..~ rs
          Lens.^? listSecurityProfilesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSecurityProfiles where
  type
    AWSResponse ListSecurityProfiles =
      ListSecurityProfilesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityProfilesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "SecurityProfileSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSecurityProfiles where
  hashWithSalt _salt ListSecurityProfiles' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListSecurityProfiles where
  rnf ListSecurityProfiles' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ListSecurityProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSecurityProfiles where
  toPath ListSecurityProfiles' {..} =
    Prelude.mconcat
      ["/security-profiles-summary/", Data.toBS instanceId]

instance Data.ToQuery ListSecurityProfiles where
  toQuery ListSecurityProfiles' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSecurityProfilesResponse' smart constructor.
data ListSecurityProfilesResponse = ListSecurityProfilesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the security profiles.
    securityProfileSummaryList :: Prelude.Maybe [SecurityProfileSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecurityProfilesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'securityProfileSummaryList', 'listSecurityProfilesResponse_securityProfileSummaryList' - Information about the security profiles.
--
-- 'httpStatus', 'listSecurityProfilesResponse_httpStatus' - The response's http status code.
newListSecurityProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecurityProfilesResponse
newListSecurityProfilesResponse pHttpStatus_ =
  ListSecurityProfilesResponse'
    { nextToken =
        Prelude.Nothing,
      securityProfileSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listSecurityProfilesResponse_nextToken :: Lens.Lens' ListSecurityProfilesResponse (Prelude.Maybe Prelude.Text)
listSecurityProfilesResponse_nextToken = Lens.lens (\ListSecurityProfilesResponse' {nextToken} -> nextToken) (\s@ListSecurityProfilesResponse' {} a -> s {nextToken = a} :: ListSecurityProfilesResponse)

-- | Information about the security profiles.
listSecurityProfilesResponse_securityProfileSummaryList :: Lens.Lens' ListSecurityProfilesResponse (Prelude.Maybe [SecurityProfileSummary])
listSecurityProfilesResponse_securityProfileSummaryList = Lens.lens (\ListSecurityProfilesResponse' {securityProfileSummaryList} -> securityProfileSummaryList) (\s@ListSecurityProfilesResponse' {} a -> s {securityProfileSummaryList = a} :: ListSecurityProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSecurityProfilesResponse_httpStatus :: Lens.Lens' ListSecurityProfilesResponse Prelude.Int
listSecurityProfilesResponse_httpStatus = Lens.lens (\ListSecurityProfilesResponse' {httpStatus} -> httpStatus) (\s@ListSecurityProfilesResponse' {} a -> s {httpStatus = a} :: ListSecurityProfilesResponse)

instance Prelude.NFData ListSecurityProfilesResponse where
  rnf ListSecurityProfilesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf securityProfileSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
