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
-- Module      : Network.AWS.EC2.GetVpnConnectionDeviceTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EC2.GetVpnConnectionDeviceTypes
  ( -- * Creating a Request
    GetVpnConnectionDeviceTypes (..),
    newGetVpnConnectionDeviceTypes,

    -- * Request Lenses
    getVpnConnectionDeviceTypes_nextToken,
    getVpnConnectionDeviceTypes_maxResults,
    getVpnConnectionDeviceTypes_dryRun,

    -- * Destructuring the Response
    GetVpnConnectionDeviceTypesResponse (..),
    newGetVpnConnectionDeviceTypesResponse,

    -- * Response Lenses
    getVpnConnectionDeviceTypesResponse_nextToken,
    getVpnConnectionDeviceTypesResponse_vpnConnectionDeviceTypes,
    getVpnConnectionDeviceTypesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetVpnConnectionDeviceTypes' smart constructor.
data GetVpnConnectionDeviceTypes = GetVpnConnectionDeviceTypes'
  { -- | The @NextToken@ value returned from a previous paginated
    -- @GetVpnConnectionDeviceTypes@ request where @MaxResults@ was used and
    -- the results exceeded the value of that parameter. Pagination continues
    -- from the end of the previous results that returned the @NextToken@
    -- value. This value is null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by @GetVpnConnectionDeviceTypes@
    -- in paginated output. When this parameter is used,
    -- @GetVpnConnectionDeviceTypes@ only returns @MaxResults@ results in a
    -- single page along with a @NextToken@ response element. The remaining
    -- results of the initial request can be seen by sending another
    -- @GetVpnConnectionDeviceTypes@ request with the returned @NextToken@
    -- value. This value can be between 200 and 1000. If this parameter is not
    -- used, then @GetVpnConnectionDeviceTypes@ returns all results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
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
-- 'nextToken', 'getVpnConnectionDeviceTypes_nextToken' - The @NextToken@ value returned from a previous paginated
-- @GetVpnConnectionDeviceTypes@ request where @MaxResults@ was used and
-- the results exceeded the value of that parameter. Pagination continues
-- from the end of the previous results that returned the @NextToken@
-- value. This value is null when there are no more results to return.
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
-- 'dryRun', 'getVpnConnectionDeviceTypes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newGetVpnConnectionDeviceTypes ::
  GetVpnConnectionDeviceTypes
newGetVpnConnectionDeviceTypes =
  GetVpnConnectionDeviceTypes'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dryRun = Prelude.Nothing
    }

-- | The @NextToken@ value returned from a previous paginated
-- @GetVpnConnectionDeviceTypes@ request where @MaxResults@ was used and
-- the results exceeded the value of that parameter. Pagination continues
-- from the end of the previous results that returned the @NextToken@
-- value. This value is null when there are no more results to return.
getVpnConnectionDeviceTypes_nextToken :: Lens.Lens' GetVpnConnectionDeviceTypes (Prelude.Maybe Prelude.Text)
getVpnConnectionDeviceTypes_nextToken = Lens.lens (\GetVpnConnectionDeviceTypes' {nextToken} -> nextToken) (\s@GetVpnConnectionDeviceTypes' {} a -> s {nextToken = a} :: GetVpnConnectionDeviceTypes)

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

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getVpnConnectionDeviceTypes_dryRun :: Lens.Lens' GetVpnConnectionDeviceTypes (Prelude.Maybe Prelude.Bool)
getVpnConnectionDeviceTypes_dryRun = Lens.lens (\GetVpnConnectionDeviceTypes' {dryRun} -> dryRun) (\s@GetVpnConnectionDeviceTypes' {} a -> s {dryRun = a} :: GetVpnConnectionDeviceTypes)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& getVpnConnectionDeviceTypes_nextToken
          Lens..~ rs
          Lens.^? getVpnConnectionDeviceTypesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetVpnConnectionDeviceTypes where
  type
    AWSResponse GetVpnConnectionDeviceTypes =
      GetVpnConnectionDeviceTypesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetVpnConnectionDeviceTypesResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "vpnConnectionDeviceTypeSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVpnConnectionDeviceTypes

instance Prelude.NFData GetVpnConnectionDeviceTypes

instance Core.ToHeaders GetVpnConnectionDeviceTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetVpnConnectionDeviceTypes where
  toPath = Prelude.const "/"

instance Core.ToQuery GetVpnConnectionDeviceTypes where
  toQuery GetVpnConnectionDeviceTypes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetVpnConnectionDeviceTypes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        "DryRun" Core.=: dryRun
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
getVpnConnectionDeviceTypesResponse_vpnConnectionDeviceTypes = Lens.lens (\GetVpnConnectionDeviceTypesResponse' {vpnConnectionDeviceTypes} -> vpnConnectionDeviceTypes) (\s@GetVpnConnectionDeviceTypesResponse' {} a -> s {vpnConnectionDeviceTypes = a} :: GetVpnConnectionDeviceTypesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getVpnConnectionDeviceTypesResponse_httpStatus :: Lens.Lens' GetVpnConnectionDeviceTypesResponse Prelude.Int
getVpnConnectionDeviceTypesResponse_httpStatus = Lens.lens (\GetVpnConnectionDeviceTypesResponse' {httpStatus} -> httpStatus) (\s@GetVpnConnectionDeviceTypesResponse' {} a -> s {httpStatus = a} :: GetVpnConnectionDeviceTypesResponse)

instance
  Prelude.NFData
    GetVpnConnectionDeviceTypesResponse
