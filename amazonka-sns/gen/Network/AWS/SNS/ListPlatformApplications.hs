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
-- Module      : Network.AWS.SNS.ListPlatformApplications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform application objects for the supported push
-- notification services, such as APNS and GCM (Firebase Cloud Messaging).
-- The results for @ListPlatformApplications@ are paginated and return a
-- limited list of applications, up to 100. If additional records are
-- available after the first page results, then a NextToken string will be
-- returned. To receive the next page, you call @ListPlatformApplications@
-- using the NextToken string received from the previous call. When there
-- are no more records to return, @NextToken@ will be null. For more
-- information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- This action is throttled at 15 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListPlatformApplications
  ( -- * Creating a Request
    ListPlatformApplications (..),
    newListPlatformApplications,

    -- * Request Lenses
    listPlatformApplications_nextToken,

    -- * Destructuring the Response
    ListPlatformApplicationsResponse (..),
    newListPlatformApplicationsResponse,

    -- * Response Lenses
    listPlatformApplicationsResponse_nextToken,
    listPlatformApplicationsResponse_platformApplications,
    listPlatformApplicationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for ListPlatformApplications action.
--
-- /See:/ 'newListPlatformApplications' smart constructor.
data ListPlatformApplications = ListPlatformApplications'
  { -- | NextToken string is used when calling ListPlatformApplications action to
    -- retrieve additional records that are available after the first page
    -- results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPlatformApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPlatformApplications_nextToken' - NextToken string is used when calling ListPlatformApplications action to
-- retrieve additional records that are available after the first page
-- results.
newListPlatformApplications ::
  ListPlatformApplications
newListPlatformApplications =
  ListPlatformApplications' {nextToken = Core.Nothing}

-- | NextToken string is used when calling ListPlatformApplications action to
-- retrieve additional records that are available after the first page
-- results.
listPlatformApplications_nextToken :: Lens.Lens' ListPlatformApplications (Core.Maybe Core.Text)
listPlatformApplications_nextToken = Lens.lens (\ListPlatformApplications' {nextToken} -> nextToken) (\s@ListPlatformApplications' {} a -> s {nextToken = a} :: ListPlatformApplications)

instance Core.AWSPager ListPlatformApplications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPlatformApplicationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPlatformApplicationsResponse_platformApplications
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPlatformApplications_nextToken
          Lens..~ rs
          Lens.^? listPlatformApplicationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListPlatformApplications where
  type
    AWSResponse ListPlatformApplications =
      ListPlatformApplicationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListPlatformApplicationsResult"
      ( \s h x ->
          ListPlatformApplicationsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "PlatformApplications"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPlatformApplications

instance Core.NFData ListPlatformApplications

instance Core.ToHeaders ListPlatformApplications where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListPlatformApplications where
  toPath = Core.const "/"

instance Core.ToQuery ListPlatformApplications where
  toQuery ListPlatformApplications' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListPlatformApplications" :: Core.ByteString),
        "Version" Core.=: ("2010-03-31" :: Core.ByteString),
        "NextToken" Core.=: nextToken
      ]

-- | Response for ListPlatformApplications action.
--
-- /See:/ 'newListPlatformApplicationsResponse' smart constructor.
data ListPlatformApplicationsResponse = ListPlatformApplicationsResponse'
  { -- | NextToken string is returned when calling ListPlatformApplications
    -- action if additional records are available after the first page results.
    nextToken :: Core.Maybe Core.Text,
    -- | Platform applications returned when calling ListPlatformApplications
    -- action.
    platformApplications :: Core.Maybe [PlatformApplication],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPlatformApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPlatformApplicationsResponse_nextToken' - NextToken string is returned when calling ListPlatformApplications
-- action if additional records are available after the first page results.
--
-- 'platformApplications', 'listPlatformApplicationsResponse_platformApplications' - Platform applications returned when calling ListPlatformApplications
-- action.
--
-- 'httpStatus', 'listPlatformApplicationsResponse_httpStatus' - The response's http status code.
newListPlatformApplicationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPlatformApplicationsResponse
newListPlatformApplicationsResponse pHttpStatus_ =
  ListPlatformApplicationsResponse'
    { nextToken =
        Core.Nothing,
      platformApplications = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | NextToken string is returned when calling ListPlatformApplications
-- action if additional records are available after the first page results.
listPlatformApplicationsResponse_nextToken :: Lens.Lens' ListPlatformApplicationsResponse (Core.Maybe Core.Text)
listPlatformApplicationsResponse_nextToken = Lens.lens (\ListPlatformApplicationsResponse' {nextToken} -> nextToken) (\s@ListPlatformApplicationsResponse' {} a -> s {nextToken = a} :: ListPlatformApplicationsResponse)

-- | Platform applications returned when calling ListPlatformApplications
-- action.
listPlatformApplicationsResponse_platformApplications :: Lens.Lens' ListPlatformApplicationsResponse (Core.Maybe [PlatformApplication])
listPlatformApplicationsResponse_platformApplications = Lens.lens (\ListPlatformApplicationsResponse' {platformApplications} -> platformApplications) (\s@ListPlatformApplicationsResponse' {} a -> s {platformApplications = a} :: ListPlatformApplicationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPlatformApplicationsResponse_httpStatus :: Lens.Lens' ListPlatformApplicationsResponse Core.Int
listPlatformApplicationsResponse_httpStatus = Lens.lens (\ListPlatformApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListPlatformApplicationsResponse' {} a -> s {httpStatus = a} :: ListPlatformApplicationsResponse)

instance Core.NFData ListPlatformApplicationsResponse
