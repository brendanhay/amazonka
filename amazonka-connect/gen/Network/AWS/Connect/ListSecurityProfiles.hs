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
-- Module      : Network.AWS.Connect.ListSecurityProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Connect.ListSecurityProfiles
  ( -- * Creating a Request
    ListSecurityProfiles (..),
    newListSecurityProfiles,

    -- * Request Lenses
    listSecurityProfiles_nextToken,
    listSecurityProfiles_maxResults,
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSecurityProfiles' smart constructor.
data ListSecurityProfiles = ListSecurityProfiles'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSecurityProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecurityProfiles_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listSecurityProfiles_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listSecurityProfiles_instanceId' - The identifier of the Amazon Connect instance.
newListSecurityProfiles ::
  -- | 'instanceId'
  Core.Text ->
  ListSecurityProfiles
newListSecurityProfiles pInstanceId_ =
  ListSecurityProfiles'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listSecurityProfiles_nextToken :: Lens.Lens' ListSecurityProfiles (Core.Maybe Core.Text)
listSecurityProfiles_nextToken = Lens.lens (\ListSecurityProfiles' {nextToken} -> nextToken) (\s@ListSecurityProfiles' {} a -> s {nextToken = a} :: ListSecurityProfiles)

-- | The maximum number of results to return per page.
listSecurityProfiles_maxResults :: Lens.Lens' ListSecurityProfiles (Core.Maybe Core.Natural)
listSecurityProfiles_maxResults = Lens.lens (\ListSecurityProfiles' {maxResults} -> maxResults) (\s@ListSecurityProfiles' {} a -> s {maxResults = a} :: ListSecurityProfiles)

-- | The identifier of the Amazon Connect instance.
listSecurityProfiles_instanceId :: Lens.Lens' ListSecurityProfiles Core.Text
listSecurityProfiles_instanceId = Lens.lens (\ListSecurityProfiles' {instanceId} -> instanceId) (\s@ListSecurityProfiles' {} a -> s {instanceId = a} :: ListSecurityProfiles)

instance Core.AWSPager ListSecurityProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesResponse_securityProfileSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSecurityProfiles_nextToken
          Lens..~ rs
          Lens.^? listSecurityProfilesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListSecurityProfiles where
  type
    AWSResponse ListSecurityProfiles =
      ListSecurityProfilesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityProfilesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "SecurityProfileSummaryList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSecurityProfiles

instance Core.NFData ListSecurityProfiles

instance Core.ToHeaders ListSecurityProfiles where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListSecurityProfiles where
  toPath ListSecurityProfiles' {..} =
    Core.mconcat
      ["/security-profiles-summary/", Core.toBS instanceId]

instance Core.ToQuery ListSecurityProfiles where
  toQuery ListSecurityProfiles' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListSecurityProfilesResponse' smart constructor.
data ListSecurityProfilesResponse = ListSecurityProfilesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the security profiles.
    securityProfileSummaryList :: Core.Maybe [SecurityProfileSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListSecurityProfilesResponse
newListSecurityProfilesResponse pHttpStatus_ =
  ListSecurityProfilesResponse'
    { nextToken =
        Core.Nothing,
      securityProfileSummaryList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listSecurityProfilesResponse_nextToken :: Lens.Lens' ListSecurityProfilesResponse (Core.Maybe Core.Text)
listSecurityProfilesResponse_nextToken = Lens.lens (\ListSecurityProfilesResponse' {nextToken} -> nextToken) (\s@ListSecurityProfilesResponse' {} a -> s {nextToken = a} :: ListSecurityProfilesResponse)

-- | Information about the security profiles.
listSecurityProfilesResponse_securityProfileSummaryList :: Lens.Lens' ListSecurityProfilesResponse (Core.Maybe [SecurityProfileSummary])
listSecurityProfilesResponse_securityProfileSummaryList = Lens.lens (\ListSecurityProfilesResponse' {securityProfileSummaryList} -> securityProfileSummaryList) (\s@ListSecurityProfilesResponse' {} a -> s {securityProfileSummaryList = a} :: ListSecurityProfilesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSecurityProfilesResponse_httpStatus :: Lens.Lens' ListSecurityProfilesResponse Core.Int
listSecurityProfilesResponse_httpStatus = Lens.lens (\ListSecurityProfilesResponse' {httpStatus} -> httpStatus) (\s@ListSecurityProfilesResponse' {} a -> s {httpStatus = a} :: ListSecurityProfilesResponse)

instance Core.NFData ListSecurityProfilesResponse
