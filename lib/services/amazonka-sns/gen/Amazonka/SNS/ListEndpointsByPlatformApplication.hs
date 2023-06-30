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
-- Module      : Amazonka.SNS.ListEndpointsByPlatformApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the endpoints and endpoint attributes for devices in a supported
-- push notification service, such as GCM (Firebase Cloud Messaging) and
-- APNS. The results for @ListEndpointsByPlatformApplication@ are paginated
-- and return a limited list of endpoints, up to 100. If additional records
-- are available after the first page results, then a NextToken string will
-- be returned. To receive the next page, you call
-- @ListEndpointsByPlatformApplication@ again using the NextToken string
-- received from the previous call. When there are no more records to
-- return, NextToken will be null. For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- This action is throttled at 30 transactions per second (TPS).
--
-- This operation returns paginated results.
module Amazonka.SNS.ListEndpointsByPlatformApplication
  ( -- * Creating a Request
    ListEndpointsByPlatformApplication (..),
    newListEndpointsByPlatformApplication,

    -- * Request Lenses
    listEndpointsByPlatformApplication_nextToken,
    listEndpointsByPlatformApplication_platformApplicationArn,

    -- * Destructuring the Response
    ListEndpointsByPlatformApplicationResponse (..),
    newListEndpointsByPlatformApplicationResponse,

    -- * Response Lenses
    listEndpointsByPlatformApplicationResponse_endpoints,
    listEndpointsByPlatformApplicationResponse_nextToken,
    listEndpointsByPlatformApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for ListEndpointsByPlatformApplication action.
--
-- /See:/ 'newListEndpointsByPlatformApplication' smart constructor.
data ListEndpointsByPlatformApplication = ListEndpointsByPlatformApplication'
  { -- | NextToken string is used when calling ListEndpointsByPlatformApplication
    -- action to retrieve additional records that are available after the first
    -- page results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | PlatformApplicationArn for ListEndpointsByPlatformApplicationInput
    -- action.
    platformApplicationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpointsByPlatformApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEndpointsByPlatformApplication_nextToken' - NextToken string is used when calling ListEndpointsByPlatformApplication
-- action to retrieve additional records that are available after the first
-- page results.
--
-- 'platformApplicationArn', 'listEndpointsByPlatformApplication_platformApplicationArn' - PlatformApplicationArn for ListEndpointsByPlatformApplicationInput
-- action.
newListEndpointsByPlatformApplication ::
  -- | 'platformApplicationArn'
  Prelude.Text ->
  ListEndpointsByPlatformApplication
newListEndpointsByPlatformApplication
  pPlatformApplicationArn_ =
    ListEndpointsByPlatformApplication'
      { nextToken =
          Prelude.Nothing,
        platformApplicationArn =
          pPlatformApplicationArn_
      }

-- | NextToken string is used when calling ListEndpointsByPlatformApplication
-- action to retrieve additional records that are available after the first
-- page results.
listEndpointsByPlatformApplication_nextToken :: Lens.Lens' ListEndpointsByPlatformApplication (Prelude.Maybe Prelude.Text)
listEndpointsByPlatformApplication_nextToken = Lens.lens (\ListEndpointsByPlatformApplication' {nextToken} -> nextToken) (\s@ListEndpointsByPlatformApplication' {} a -> s {nextToken = a} :: ListEndpointsByPlatformApplication)

-- | PlatformApplicationArn for ListEndpointsByPlatformApplicationInput
-- action.
listEndpointsByPlatformApplication_platformApplicationArn :: Lens.Lens' ListEndpointsByPlatformApplication Prelude.Text
listEndpointsByPlatformApplication_platformApplicationArn = Lens.lens (\ListEndpointsByPlatformApplication' {platformApplicationArn} -> platformApplicationArn) (\s@ListEndpointsByPlatformApplication' {} a -> s {platformApplicationArn = a} :: ListEndpointsByPlatformApplication)

instance
  Core.AWSPager
    ListEndpointsByPlatformApplication
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEndpointsByPlatformApplicationResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEndpointsByPlatformApplicationResponse_endpoints
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEndpointsByPlatformApplication_nextToken
          Lens..~ rs
          Lens.^? listEndpointsByPlatformApplicationResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListEndpointsByPlatformApplication
  where
  type
    AWSResponse ListEndpointsByPlatformApplication =
      ListEndpointsByPlatformApplicationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListEndpointsByPlatformApplicationResult"
      ( \s h x ->
          ListEndpointsByPlatformApplicationResponse'
            Prelude.<$> ( x
                            Data..@? "Endpoints"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListEndpointsByPlatformApplication
  where
  hashWithSalt
    _salt
    ListEndpointsByPlatformApplication' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` platformApplicationArn

instance
  Prelude.NFData
    ListEndpointsByPlatformApplication
  where
  rnf ListEndpointsByPlatformApplication' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf platformApplicationArn

instance
  Data.ToHeaders
    ListEndpointsByPlatformApplication
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListEndpointsByPlatformApplication
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListEndpointsByPlatformApplication
  where
  toQuery ListEndpointsByPlatformApplication' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ListEndpointsByPlatformApplication" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        "PlatformApplicationArn"
          Data.=: platformApplicationArn
      ]

-- | Response for ListEndpointsByPlatformApplication action.
--
-- /See:/ 'newListEndpointsByPlatformApplicationResponse' smart constructor.
data ListEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse'
  { -- | Endpoints returned for ListEndpointsByPlatformApplication action.
    endpoints :: Prelude.Maybe [Endpoint],
    -- | NextToken string is returned when calling
    -- ListEndpointsByPlatformApplication action if additional records are
    -- available after the first page results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpointsByPlatformApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoints', 'listEndpointsByPlatformApplicationResponse_endpoints' - Endpoints returned for ListEndpointsByPlatformApplication action.
--
-- 'nextToken', 'listEndpointsByPlatformApplicationResponse_nextToken' - NextToken string is returned when calling
-- ListEndpointsByPlatformApplication action if additional records are
-- available after the first page results.
--
-- 'httpStatus', 'listEndpointsByPlatformApplicationResponse_httpStatus' - The response's http status code.
newListEndpointsByPlatformApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEndpointsByPlatformApplicationResponse
newListEndpointsByPlatformApplicationResponse
  pHttpStatus_ =
    ListEndpointsByPlatformApplicationResponse'
      { endpoints =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Endpoints returned for ListEndpointsByPlatformApplication action.
listEndpointsByPlatformApplicationResponse_endpoints :: Lens.Lens' ListEndpointsByPlatformApplicationResponse (Prelude.Maybe [Endpoint])
listEndpointsByPlatformApplicationResponse_endpoints = Lens.lens (\ListEndpointsByPlatformApplicationResponse' {endpoints} -> endpoints) (\s@ListEndpointsByPlatformApplicationResponse' {} a -> s {endpoints = a} :: ListEndpointsByPlatformApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | NextToken string is returned when calling
-- ListEndpointsByPlatformApplication action if additional records are
-- available after the first page results.
listEndpointsByPlatformApplicationResponse_nextToken :: Lens.Lens' ListEndpointsByPlatformApplicationResponse (Prelude.Maybe Prelude.Text)
listEndpointsByPlatformApplicationResponse_nextToken = Lens.lens (\ListEndpointsByPlatformApplicationResponse' {nextToken} -> nextToken) (\s@ListEndpointsByPlatformApplicationResponse' {} a -> s {nextToken = a} :: ListEndpointsByPlatformApplicationResponse)

-- | The response's http status code.
listEndpointsByPlatformApplicationResponse_httpStatus :: Lens.Lens' ListEndpointsByPlatformApplicationResponse Prelude.Int
listEndpointsByPlatformApplicationResponse_httpStatus = Lens.lens (\ListEndpointsByPlatformApplicationResponse' {httpStatus} -> httpStatus) (\s@ListEndpointsByPlatformApplicationResponse' {} a -> s {httpStatus = a} :: ListEndpointsByPlatformApplicationResponse)

instance
  Prelude.NFData
    ListEndpointsByPlatformApplicationResponse
  where
  rnf ListEndpointsByPlatformApplicationResponse' {..} =
    Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
