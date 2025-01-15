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
-- Module      : Amazonka.EC2.GetIpamAddressHistory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve historical information about a CIDR within an IPAM scope. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/view-history-cidr-ipam.html View the history of IP addresses>
-- in the /Amazon VPC IPAM User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetIpamAddressHistory
  ( -- * Creating a Request
    GetIpamAddressHistory (..),
    newGetIpamAddressHistory,

    -- * Request Lenses
    getIpamAddressHistory_dryRun,
    getIpamAddressHistory_endTime,
    getIpamAddressHistory_maxResults,
    getIpamAddressHistory_nextToken,
    getIpamAddressHistory_startTime,
    getIpamAddressHistory_vpcId,
    getIpamAddressHistory_cidr,
    getIpamAddressHistory_ipamScopeId,

    -- * Destructuring the Response
    GetIpamAddressHistoryResponse (..),
    newGetIpamAddressHistoryResponse,

    -- * Response Lenses
    getIpamAddressHistoryResponse_historyRecords,
    getIpamAddressHistoryResponse_nextToken,
    getIpamAddressHistoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIpamAddressHistory' smart constructor.
data GetIpamAddressHistory = GetIpamAddressHistory'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The end of the time period for which you are looking for history. If you
    -- omit this option, it will default to the current time.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The maximum number of historical results you would like returned per
    -- page. Defaults to 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The start of the time period for which you are looking for history. If
    -- you omit this option, it will default to the value of EndTime.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the VPC you want your history records filtered by.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The CIDR you want the history of. The CIDR can be an IPv4 or IPv6 IP
    -- address range. If you enter a \/16 IPv4 CIDR, you will get records that
    -- match it exactly. You will not get records for any subnets within the
    -- \/16 CIDR.
    cidr :: Prelude.Text,
    -- | The ID of the IPAM scope that the CIDR is in.
    ipamScopeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamAddressHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getIpamAddressHistory_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'endTime', 'getIpamAddressHistory_endTime' - The end of the time period for which you are looking for history. If you
-- omit this option, it will default to the current time.
--
-- 'maxResults', 'getIpamAddressHistory_maxResults' - The maximum number of historical results you would like returned per
-- page. Defaults to 100.
--
-- 'nextToken', 'getIpamAddressHistory_nextToken' - The token for the next page of results.
--
-- 'startTime', 'getIpamAddressHistory_startTime' - The start of the time period for which you are looking for history. If
-- you omit this option, it will default to the value of EndTime.
--
-- 'vpcId', 'getIpamAddressHistory_vpcId' - The ID of the VPC you want your history records filtered by.
--
-- 'cidr', 'getIpamAddressHistory_cidr' - The CIDR you want the history of. The CIDR can be an IPv4 or IPv6 IP
-- address range. If you enter a \/16 IPv4 CIDR, you will get records that
-- match it exactly. You will not get records for any subnets within the
-- \/16 CIDR.
--
-- 'ipamScopeId', 'getIpamAddressHistory_ipamScopeId' - The ID of the IPAM scope that the CIDR is in.
newGetIpamAddressHistory ::
  -- | 'cidr'
  Prelude.Text ->
  -- | 'ipamScopeId'
  Prelude.Text ->
  GetIpamAddressHistory
newGetIpamAddressHistory pCidr_ pIpamScopeId_ =
  GetIpamAddressHistory'
    { dryRun = Prelude.Nothing,
      endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTime = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      cidr = pCidr_,
      ipamScopeId = pIpamScopeId_
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
getIpamAddressHistory_dryRun :: Lens.Lens' GetIpamAddressHistory (Prelude.Maybe Prelude.Bool)
getIpamAddressHistory_dryRun = Lens.lens (\GetIpamAddressHistory' {dryRun} -> dryRun) (\s@GetIpamAddressHistory' {} a -> s {dryRun = a} :: GetIpamAddressHistory)

-- | The end of the time period for which you are looking for history. If you
-- omit this option, it will default to the current time.
getIpamAddressHistory_endTime :: Lens.Lens' GetIpamAddressHistory (Prelude.Maybe Prelude.UTCTime)
getIpamAddressHistory_endTime = Lens.lens (\GetIpamAddressHistory' {endTime} -> endTime) (\s@GetIpamAddressHistory' {} a -> s {endTime = a} :: GetIpamAddressHistory) Prelude.. Lens.mapping Data._Time

-- | The maximum number of historical results you would like returned per
-- page. Defaults to 100.
getIpamAddressHistory_maxResults :: Lens.Lens' GetIpamAddressHistory (Prelude.Maybe Prelude.Natural)
getIpamAddressHistory_maxResults = Lens.lens (\GetIpamAddressHistory' {maxResults} -> maxResults) (\s@GetIpamAddressHistory' {} a -> s {maxResults = a} :: GetIpamAddressHistory)

-- | The token for the next page of results.
getIpamAddressHistory_nextToken :: Lens.Lens' GetIpamAddressHistory (Prelude.Maybe Prelude.Text)
getIpamAddressHistory_nextToken = Lens.lens (\GetIpamAddressHistory' {nextToken} -> nextToken) (\s@GetIpamAddressHistory' {} a -> s {nextToken = a} :: GetIpamAddressHistory)

-- | The start of the time period for which you are looking for history. If
-- you omit this option, it will default to the value of EndTime.
getIpamAddressHistory_startTime :: Lens.Lens' GetIpamAddressHistory (Prelude.Maybe Prelude.UTCTime)
getIpamAddressHistory_startTime = Lens.lens (\GetIpamAddressHistory' {startTime} -> startTime) (\s@GetIpamAddressHistory' {} a -> s {startTime = a} :: GetIpamAddressHistory) Prelude.. Lens.mapping Data._Time

-- | The ID of the VPC you want your history records filtered by.
getIpamAddressHistory_vpcId :: Lens.Lens' GetIpamAddressHistory (Prelude.Maybe Prelude.Text)
getIpamAddressHistory_vpcId = Lens.lens (\GetIpamAddressHistory' {vpcId} -> vpcId) (\s@GetIpamAddressHistory' {} a -> s {vpcId = a} :: GetIpamAddressHistory)

-- | The CIDR you want the history of. The CIDR can be an IPv4 or IPv6 IP
-- address range. If you enter a \/16 IPv4 CIDR, you will get records that
-- match it exactly. You will not get records for any subnets within the
-- \/16 CIDR.
getIpamAddressHistory_cidr :: Lens.Lens' GetIpamAddressHistory Prelude.Text
getIpamAddressHistory_cidr = Lens.lens (\GetIpamAddressHistory' {cidr} -> cidr) (\s@GetIpamAddressHistory' {} a -> s {cidr = a} :: GetIpamAddressHistory)

-- | The ID of the IPAM scope that the CIDR is in.
getIpamAddressHistory_ipamScopeId :: Lens.Lens' GetIpamAddressHistory Prelude.Text
getIpamAddressHistory_ipamScopeId = Lens.lens (\GetIpamAddressHistory' {ipamScopeId} -> ipamScopeId) (\s@GetIpamAddressHistory' {} a -> s {ipamScopeId = a} :: GetIpamAddressHistory)

instance Core.AWSPager GetIpamAddressHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getIpamAddressHistoryResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getIpamAddressHistoryResponse_historyRecords
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getIpamAddressHistory_nextToken
              Lens..~ rs
              Lens.^? getIpamAddressHistoryResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetIpamAddressHistory where
  type
    AWSResponse GetIpamAddressHistory =
      GetIpamAddressHistoryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetIpamAddressHistoryResponse'
            Prelude.<$> ( x
                            Data..@? "historyRecordSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIpamAddressHistory where
  hashWithSalt _salt GetIpamAddressHistory' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` ipamScopeId

instance Prelude.NFData GetIpamAddressHistory where
  rnf GetIpamAddressHistory' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf endTime `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf startTime `Prelude.seq`
              Prelude.rnf vpcId `Prelude.seq`
                Prelude.rnf cidr `Prelude.seq`
                  Prelude.rnf ipamScopeId

instance Data.ToHeaders GetIpamAddressHistory where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetIpamAddressHistory where
  toPath = Prelude.const "/"

instance Data.ToQuery GetIpamAddressHistory where
  toQuery GetIpamAddressHistory' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetIpamAddressHistory" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "EndTime" Data.=: endTime,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "StartTime" Data.=: startTime,
        "VpcId" Data.=: vpcId,
        "Cidr" Data.=: cidr,
        "IpamScopeId" Data.=: ipamScopeId
      ]

-- | /See:/ 'newGetIpamAddressHistoryResponse' smart constructor.
data GetIpamAddressHistoryResponse = GetIpamAddressHistoryResponse'
  { -- | A historical record for a CIDR within an IPAM scope. If the CIDR is
    -- associated with an EC2 instance, you will see an object in the response
    -- for the instance and one for the network interface.
    historyRecords :: Prelude.Maybe [IpamAddressHistoryRecord],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamAddressHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'historyRecords', 'getIpamAddressHistoryResponse_historyRecords' - A historical record for a CIDR within an IPAM scope. If the CIDR is
-- associated with an EC2 instance, you will see an object in the response
-- for the instance and one for the network interface.
--
-- 'nextToken', 'getIpamAddressHistoryResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'getIpamAddressHistoryResponse_httpStatus' - The response's http status code.
newGetIpamAddressHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIpamAddressHistoryResponse
newGetIpamAddressHistoryResponse pHttpStatus_ =
  GetIpamAddressHistoryResponse'
    { historyRecords =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A historical record for a CIDR within an IPAM scope. If the CIDR is
-- associated with an EC2 instance, you will see an object in the response
-- for the instance and one for the network interface.
getIpamAddressHistoryResponse_historyRecords :: Lens.Lens' GetIpamAddressHistoryResponse (Prelude.Maybe [IpamAddressHistoryRecord])
getIpamAddressHistoryResponse_historyRecords = Lens.lens (\GetIpamAddressHistoryResponse' {historyRecords} -> historyRecords) (\s@GetIpamAddressHistoryResponse' {} a -> s {historyRecords = a} :: GetIpamAddressHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getIpamAddressHistoryResponse_nextToken :: Lens.Lens' GetIpamAddressHistoryResponse (Prelude.Maybe Prelude.Text)
getIpamAddressHistoryResponse_nextToken = Lens.lens (\GetIpamAddressHistoryResponse' {nextToken} -> nextToken) (\s@GetIpamAddressHistoryResponse' {} a -> s {nextToken = a} :: GetIpamAddressHistoryResponse)

-- | The response's http status code.
getIpamAddressHistoryResponse_httpStatus :: Lens.Lens' GetIpamAddressHistoryResponse Prelude.Int
getIpamAddressHistoryResponse_httpStatus = Lens.lens (\GetIpamAddressHistoryResponse' {httpStatus} -> httpStatus) (\s@GetIpamAddressHistoryResponse' {} a -> s {httpStatus = a} :: GetIpamAddressHistoryResponse)

instance Prelude.NFData GetIpamAddressHistoryResponse where
  rnf GetIpamAddressHistoryResponse' {..} =
    Prelude.rnf historyRecords `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
