{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Connect.ListRoutingProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the routing profiles for the
-- specified Amazon Connect instance.
--
-- For more information about routing profiles, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing.html Routing Profiles>
-- and
-- <https://docs.aws.amazon.com/connect/latest/adminguide/routing-profiles.html Create a Routing Profile>
-- in the /Amazon Connect Administrator Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListRoutingProfiles
  ( -- * Creating a Request
    ListRoutingProfiles (..),
    newListRoutingProfiles,

    -- * Request Lenses
    listRoutingProfiles_nextToken,
    listRoutingProfiles_maxResults,
    listRoutingProfiles_instanceId,

    -- * Destructuring the Response
    ListRoutingProfilesResponse (..),
    newListRoutingProfilesResponse,

    -- * Response Lenses
    listRoutingProfilesResponse_nextToken,
    listRoutingProfilesResponse_routingProfileSummaryList,
    listRoutingProfilesResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRoutingProfiles' smart constructor.
data ListRoutingProfiles = ListRoutingProfiles'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListRoutingProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoutingProfiles_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listRoutingProfiles_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listRoutingProfiles_instanceId' - The identifier of the Amazon Connect instance.
newListRoutingProfiles ::
  -- | 'instanceId'
  Prelude.Text ->
  ListRoutingProfiles
newListRoutingProfiles pInstanceId_ =
  ListRoutingProfiles'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listRoutingProfiles_nextToken :: Lens.Lens' ListRoutingProfiles (Prelude.Maybe Prelude.Text)
listRoutingProfiles_nextToken = Lens.lens (\ListRoutingProfiles' {nextToken} -> nextToken) (\s@ListRoutingProfiles' {} a -> s {nextToken = a} :: ListRoutingProfiles)

-- | The maximum number of results to return per page.
listRoutingProfiles_maxResults :: Lens.Lens' ListRoutingProfiles (Prelude.Maybe Prelude.Natural)
listRoutingProfiles_maxResults = Lens.lens (\ListRoutingProfiles' {maxResults} -> maxResults) (\s@ListRoutingProfiles' {} a -> s {maxResults = a} :: ListRoutingProfiles)

-- | The identifier of the Amazon Connect instance.
listRoutingProfiles_instanceId :: Lens.Lens' ListRoutingProfiles Prelude.Text
listRoutingProfiles_instanceId = Lens.lens (\ListRoutingProfiles' {instanceId} -> instanceId) (\s@ListRoutingProfiles' {} a -> s {instanceId = a} :: ListRoutingProfiles)

instance Pager.AWSPager ListRoutingProfiles where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listRoutingProfilesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listRoutingProfilesResponse_routingProfileSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listRoutingProfiles_nextToken
          Lens..~ rs
          Lens.^? listRoutingProfilesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListRoutingProfiles where
  type
    Rs ListRoutingProfiles =
      ListRoutingProfilesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoutingProfilesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "RoutingProfileSummaryList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRoutingProfiles

instance Prelude.NFData ListRoutingProfiles

instance Prelude.ToHeaders ListRoutingProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListRoutingProfiles where
  toPath ListRoutingProfiles' {..} =
    Prelude.mconcat
      [ "/routing-profiles-summary/",
        Prelude.toBS instanceId
      ]

instance Prelude.ToQuery ListRoutingProfiles where
  toQuery ListRoutingProfiles' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListRoutingProfilesResponse' smart constructor.
data ListRoutingProfilesResponse = ListRoutingProfilesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the routing profiles.
    routingProfileSummaryList :: Prelude.Maybe [RoutingProfileSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListRoutingProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoutingProfilesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'routingProfileSummaryList', 'listRoutingProfilesResponse_routingProfileSummaryList' - Information about the routing profiles.
--
-- 'httpStatus', 'listRoutingProfilesResponse_httpStatus' - The response's http status code.
newListRoutingProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRoutingProfilesResponse
newListRoutingProfilesResponse pHttpStatus_ =
  ListRoutingProfilesResponse'
    { nextToken =
        Prelude.Nothing,
      routingProfileSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listRoutingProfilesResponse_nextToken :: Lens.Lens' ListRoutingProfilesResponse (Prelude.Maybe Prelude.Text)
listRoutingProfilesResponse_nextToken = Lens.lens (\ListRoutingProfilesResponse' {nextToken} -> nextToken) (\s@ListRoutingProfilesResponse' {} a -> s {nextToken = a} :: ListRoutingProfilesResponse)

-- | Information about the routing profiles.
listRoutingProfilesResponse_routingProfileSummaryList :: Lens.Lens' ListRoutingProfilesResponse (Prelude.Maybe [RoutingProfileSummary])
listRoutingProfilesResponse_routingProfileSummaryList = Lens.lens (\ListRoutingProfilesResponse' {routingProfileSummaryList} -> routingProfileSummaryList) (\s@ListRoutingProfilesResponse' {} a -> s {routingProfileSummaryList = a} :: ListRoutingProfilesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listRoutingProfilesResponse_httpStatus :: Lens.Lens' ListRoutingProfilesResponse Prelude.Int
listRoutingProfilesResponse_httpStatus = Lens.lens (\ListRoutingProfilesResponse' {httpStatus} -> httpStatus) (\s@ListRoutingProfilesResponse' {} a -> s {httpStatus = a} :: ListRoutingProfilesResponse)

instance Prelude.NFData ListRoutingProfilesResponse
