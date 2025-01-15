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
-- Module      : Amazonka.SNS.ListPlatformApplications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.SNS.ListPlatformApplications
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for ListPlatformApplications action.
--
-- /See:/ 'newListPlatformApplications' smart constructor.
data ListPlatformApplications = ListPlatformApplications'
  { -- | NextToken string is used when calling ListPlatformApplications action to
    -- retrieve additional records that are available after the first page
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  ListPlatformApplications'
    { nextToken =
        Prelude.Nothing
    }

-- | NextToken string is used when calling ListPlatformApplications action to
-- retrieve additional records that are available after the first page
-- results.
listPlatformApplications_nextToken :: Lens.Lens' ListPlatformApplications (Prelude.Maybe Prelude.Text)
listPlatformApplications_nextToken = Lens.lens (\ListPlatformApplications' {nextToken} -> nextToken) (\s@ListPlatformApplications' {} a -> s {nextToken = a} :: ListPlatformApplications)

instance Core.AWSPager ListPlatformApplications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPlatformApplicationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPlatformApplicationsResponse_platformApplications
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listPlatformApplications_nextToken
              Lens..~ rs
              Lens.^? listPlatformApplicationsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListPlatformApplications where
  type
    AWSResponse ListPlatformApplications =
      ListPlatformApplicationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListPlatformApplicationsResult"
      ( \s h x ->
          ListPlatformApplicationsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x
                            Data..@? "PlatformApplications"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPlatformApplications where
  hashWithSalt _salt ListPlatformApplications' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPlatformApplications where
  rnf ListPlatformApplications' {..} =
    Prelude.rnf nextToken

instance Data.ToHeaders ListPlatformApplications where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListPlatformApplications where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPlatformApplications where
  toQuery ListPlatformApplications' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListPlatformApplications" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken
      ]

-- | Response for ListPlatformApplications action.
--
-- /See:/ 'newListPlatformApplicationsResponse' smart constructor.
data ListPlatformApplicationsResponse = ListPlatformApplicationsResponse'
  { -- | NextToken string is returned when calling ListPlatformApplications
    -- action if additional records are available after the first page results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Platform applications returned when calling ListPlatformApplications
    -- action.
    platformApplications :: Prelude.Maybe [PlatformApplication],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListPlatformApplicationsResponse
newListPlatformApplicationsResponse pHttpStatus_ =
  ListPlatformApplicationsResponse'
    { nextToken =
        Prelude.Nothing,
      platformApplications = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | NextToken string is returned when calling ListPlatformApplications
-- action if additional records are available after the first page results.
listPlatformApplicationsResponse_nextToken :: Lens.Lens' ListPlatformApplicationsResponse (Prelude.Maybe Prelude.Text)
listPlatformApplicationsResponse_nextToken = Lens.lens (\ListPlatformApplicationsResponse' {nextToken} -> nextToken) (\s@ListPlatformApplicationsResponse' {} a -> s {nextToken = a} :: ListPlatformApplicationsResponse)

-- | Platform applications returned when calling ListPlatformApplications
-- action.
listPlatformApplicationsResponse_platformApplications :: Lens.Lens' ListPlatformApplicationsResponse (Prelude.Maybe [PlatformApplication])
listPlatformApplicationsResponse_platformApplications = Lens.lens (\ListPlatformApplicationsResponse' {platformApplications} -> platformApplications) (\s@ListPlatformApplicationsResponse' {} a -> s {platformApplications = a} :: ListPlatformApplicationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPlatformApplicationsResponse_httpStatus :: Lens.Lens' ListPlatformApplicationsResponse Prelude.Int
listPlatformApplicationsResponse_httpStatus = Lens.lens (\ListPlatformApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListPlatformApplicationsResponse' {} a -> s {httpStatus = a} :: ListPlatformApplicationsResponse)

instance
  Prelude.NFData
    ListPlatformApplicationsResponse
  where
  rnf ListPlatformApplicationsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf platformApplications `Prelude.seq`
        Prelude.rnf httpStatus
