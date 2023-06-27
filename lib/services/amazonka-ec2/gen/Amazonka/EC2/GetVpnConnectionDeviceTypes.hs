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
-- Module      : Amazonka.EC2.GetVpnConnectionDeviceTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtain a list of customer gateway devices for which sample configuration
-- files can be provided. The request has no additional parameters. You can
-- also see the list of device types with sample configuration files
-- available under
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/your-cgw.html Your customer gateway device>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetVpnConnectionDeviceTypes
  ( -- * Creating a Request
    GetVpnConnectionDeviceTypes (..),
    newGetVpnConnectionDeviceTypes,

    -- * Request Lenses
    getVpnConnectionDeviceTypes_dryRun,
    getVpnConnectionDeviceTypes_maxResults,
    getVpnConnectionDeviceTypes_nextToken,

    -- * Destructuring the Response
    GetVpnConnectionDeviceTypesResponse (..),
    newGetVpnConnectionDeviceTypesResponse,

    -- * Response Lenses
    getVpnConnectionDeviceTypesResponse_nextToken,
    getVpnConnectionDeviceTypesResponse_vpnConnectionDeviceTypes,
    getVpnConnectionDeviceTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVpnConnectionDeviceTypes' smart constructor.
data GetVpnConnectionDeviceTypes = GetVpnConnectionDeviceTypes'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results returned by @GetVpnConnectionDeviceTypes@
    -- in paginated output. When this parameter is used,
    -- @GetVpnConnectionDeviceTypes@ only returns @MaxResults@ results in a
    -- single page along with a @NextToken@ response element. The remaining
    -- results of the initial request can be seen by sending another
    -- @GetVpnConnectionDeviceTypes@ request with the returned @NextToken@
    -- value. This value can be between 200 and 1000. If this parameter is not
    -- used, then @GetVpnConnectionDeviceTypes@ returns all results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The @NextToken@ value returned from a previous paginated
    -- @GetVpnConnectionDeviceTypes@ request where @MaxResults@ was used and
    -- the results exceeded the value of that parameter. Pagination continues
    -- from the end of the previous results that returned the @NextToken@
    -- value. This value is null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpnConnectionDeviceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getVpnConnectionDeviceTypes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getVpnConnectionDeviceTypes_maxResults' - The maximum number of results returned by @GetVpnConnectionDeviceTypes@
-- in paginated output. When this parameter is used,
-- @GetVpnConnectionDeviceTypes@ only returns @MaxResults@ results in a
-- single page along with a @NextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @GetVpnConnectionDeviceTypes@ request with the returned @NextToken@
-- value. This value can be between 200 and 1000. If this parameter is not
-- used, then @GetVpnConnectionDeviceTypes@ returns all results.
--
-- 'nextToken', 'getVpnConnectionDeviceTypes_nextToken' - The @NextToken@ value returned from a previous paginated
-- @GetVpnConnectionDeviceTypes@ request where @MaxResults@ was used and
-- the results exceeded the value of that parameter. Pagination continues
-- from the end of the previous results that returned the @NextToken@
-- value. This value is null when there are no more results to return.
newGetVpnConnectionDeviceTypes ::
  GetVpnConnectionDeviceTypes
newGetVpnConnectionDeviceTypes =
  GetVpnConnectionDeviceTypes'
    { dryRun =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getVpnConnectionDeviceTypes_dryRun :: Lens.Lens' GetVpnConnectionDeviceTypes (Prelude.Maybe Prelude.Bool)
getVpnConnectionDeviceTypes_dryRun = Lens.lens (\GetVpnConnectionDeviceTypes' {dryRun} -> dryRun) (\s@GetVpnConnectionDeviceTypes' {} a -> s {dryRun = a} :: GetVpnConnectionDeviceTypes)

-- | The maximum number of results returned by @GetVpnConnectionDeviceTypes@
-- in paginated output. When this parameter is used,
-- @GetVpnConnectionDeviceTypes@ only returns @MaxResults@ results in a
-- single page along with a @NextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @GetVpnConnectionDeviceTypes@ request with the returned @NextToken@
-- value. This value can be between 200 and 1000. If this parameter is not
-- used, then @GetVpnConnectionDeviceTypes@ returns all results.
getVpnConnectionDeviceTypes_maxResults :: Lens.Lens' GetVpnConnectionDeviceTypes (Prelude.Maybe Prelude.Natural)
getVpnConnectionDeviceTypes_maxResults = Lens.lens (\GetVpnConnectionDeviceTypes' {maxResults} -> maxResults) (\s@GetVpnConnectionDeviceTypes' {} a -> s {maxResults = a} :: GetVpnConnectionDeviceTypes)

-- | The @NextToken@ value returned from a previous paginated
-- @GetVpnConnectionDeviceTypes@ request where @MaxResults@ was used and
-- the results exceeded the value of that parameter. Pagination continues
-- from the end of the previous results that returned the @NextToken@
-- value. This value is null when there are no more results to return.
getVpnConnectionDeviceTypes_nextToken :: Lens.Lens' GetVpnConnectionDeviceTypes (Prelude.Maybe Prelude.Text)
getVpnConnectionDeviceTypes_nextToken = Lens.lens (\GetVpnConnectionDeviceTypes' {nextToken} -> nextToken) (\s@GetVpnConnectionDeviceTypes' {} a -> s {nextToken = a} :: GetVpnConnectionDeviceTypes)

instance Core.AWSPager GetVpnConnectionDeviceTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getVpnConnectionDeviceTypesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getVpnConnectionDeviceTypesResponse_vpnConnectionDeviceTypes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getVpnConnectionDeviceTypes_nextToken
          Lens..~ rs
          Lens.^? getVpnConnectionDeviceTypesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetVpnConnectionDeviceTypes where
  type
    AWSResponse GetVpnConnectionDeviceTypes =
      GetVpnConnectionDeviceTypesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetVpnConnectionDeviceTypesResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "vpnConnectionDeviceTypeSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVpnConnectionDeviceTypes where
  hashWithSalt _salt GetVpnConnectionDeviceTypes' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetVpnConnectionDeviceTypes where
  rnf GetVpnConnectionDeviceTypes' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetVpnConnectionDeviceTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetVpnConnectionDeviceTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery GetVpnConnectionDeviceTypes where
  toQuery GetVpnConnectionDeviceTypes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetVpnConnectionDeviceTypes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetVpnConnectionDeviceTypesResponse' smart constructor.
data GetVpnConnectionDeviceTypesResponse = GetVpnConnectionDeviceTypesResponse'
  { -- | The @NextToken@ value to include in a future
    -- @GetVpnConnectionDeviceTypes@ request. When the results of a
    -- @GetVpnConnectionDeviceTypes@ request exceed @MaxResults@, this value
    -- can be used to retrieve the next page of results. This value is null
    -- when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of customer gateway devices that have a sample configuration file
    -- available for use.
    vpnConnectionDeviceTypes :: Prelude.Maybe [VpnConnectionDeviceType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpnConnectionDeviceTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getVpnConnectionDeviceTypesResponse_nextToken' - The @NextToken@ value to include in a future
-- @GetVpnConnectionDeviceTypes@ request. When the results of a
-- @GetVpnConnectionDeviceTypes@ request exceed @MaxResults@, this value
-- can be used to retrieve the next page of results. This value is null
-- when there are no more results to return.
--
-- 'vpnConnectionDeviceTypes', 'getVpnConnectionDeviceTypesResponse_vpnConnectionDeviceTypes' - List of customer gateway devices that have a sample configuration file
-- available for use.
--
-- 'httpStatus', 'getVpnConnectionDeviceTypesResponse_httpStatus' - The response's http status code.
newGetVpnConnectionDeviceTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVpnConnectionDeviceTypesResponse
newGetVpnConnectionDeviceTypesResponse pHttpStatus_ =
  GetVpnConnectionDeviceTypesResponse'
    { nextToken =
        Prelude.Nothing,
      vpnConnectionDeviceTypes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @NextToken@ value to include in a future
-- @GetVpnConnectionDeviceTypes@ request. When the results of a
-- @GetVpnConnectionDeviceTypes@ request exceed @MaxResults@, this value
-- can be used to retrieve the next page of results. This value is null
-- when there are no more results to return.
getVpnConnectionDeviceTypesResponse_nextToken :: Lens.Lens' GetVpnConnectionDeviceTypesResponse (Prelude.Maybe Prelude.Text)
getVpnConnectionDeviceTypesResponse_nextToken = Lens.lens (\GetVpnConnectionDeviceTypesResponse' {nextToken} -> nextToken) (\s@GetVpnConnectionDeviceTypesResponse' {} a -> s {nextToken = a} :: GetVpnConnectionDeviceTypesResponse)

-- | List of customer gateway devices that have a sample configuration file
-- available for use.
getVpnConnectionDeviceTypesResponse_vpnConnectionDeviceTypes :: Lens.Lens' GetVpnConnectionDeviceTypesResponse (Prelude.Maybe [VpnConnectionDeviceType])
getVpnConnectionDeviceTypesResponse_vpnConnectionDeviceTypes = Lens.lens (\GetVpnConnectionDeviceTypesResponse' {vpnConnectionDeviceTypes} -> vpnConnectionDeviceTypes) (\s@GetVpnConnectionDeviceTypesResponse' {} a -> s {vpnConnectionDeviceTypes = a} :: GetVpnConnectionDeviceTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getVpnConnectionDeviceTypesResponse_httpStatus :: Lens.Lens' GetVpnConnectionDeviceTypesResponse Prelude.Int
getVpnConnectionDeviceTypesResponse_httpStatus = Lens.lens (\GetVpnConnectionDeviceTypesResponse' {httpStatus} -> httpStatus) (\s@GetVpnConnectionDeviceTypesResponse' {} a -> s {httpStatus = a} :: GetVpnConnectionDeviceTypesResponse)

instance
  Prelude.NFData
    GetVpnConnectionDeviceTypesResponse
  where
  rnf GetVpnConnectionDeviceTypesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpnConnectionDeviceTypes
      `Prelude.seq` Prelude.rnf httpStatus
