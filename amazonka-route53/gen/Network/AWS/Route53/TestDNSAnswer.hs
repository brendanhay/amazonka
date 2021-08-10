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
-- Module      : Network.AWS.Route53.TestDNSAnswer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the value that Amazon Route 53 returns in response to a DNS request
-- for a specified record name and type. You can optionally specify the IP
-- address of a DNS resolver, an EDNS0 client subnet IP address, and a
-- subnet mask.
module Network.AWS.Route53.TestDNSAnswer
  ( -- * Creating a Request
    TestDNSAnswer (..),
    newTestDNSAnswer,

    -- * Request Lenses
    testDNSAnswer_resolverIP,
    testDNSAnswer_eDNS0ClientSubnetMask,
    testDNSAnswer_eDNS0ClientSubnetIP,
    testDNSAnswer_hostedZoneId,
    testDNSAnswer_recordName,
    testDNSAnswer_recordType,

    -- * Destructuring the Response
    TestDNSAnswerResponse (..),
    newTestDNSAnswerResponse,

    -- * Response Lenses
    testDNSAnswerResponse_httpStatus,
    testDNSAnswerResponse_nameserver,
    testDNSAnswerResponse_recordName,
    testDNSAnswerResponse_recordType,
    testDNSAnswerResponse_recordData,
    testDNSAnswerResponse_responseCode,
    testDNSAnswerResponse_protocol,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | Gets the value that Amazon Route 53 returns in response to a DNS request
-- for a specified record name and type. You can optionally specify the IP
-- address of a DNS resolver, an EDNS0 client subnet IP address, and a
-- subnet mask.
--
-- /See:/ 'newTestDNSAnswer' smart constructor.
data TestDNSAnswer = TestDNSAnswer'
  { -- | If you want to simulate a request from a specific DNS resolver, specify
    -- the IP address for that resolver. If you omit this value,
    -- @TestDnsAnswer@ uses the IP address of a DNS resolver in the AWS US East
    -- (N. Virginia) Region (@us-east-1@).
    resolverIP :: Prelude.Maybe Prelude.Text,
    -- | If you specify an IP address for @edns0clientsubnetip@, you can
    -- optionally specify the number of bits of the IP address that you want
    -- the checking tool to include in the DNS query. For example, if you
    -- specify @192.0.2.44@ for @edns0clientsubnetip@ and @24@ for
    -- @edns0clientsubnetmask@, the checking tool will simulate a request from
    -- 192.0.2.0\/24. The default value is 24 bits for IPv4 addresses and 64
    -- bits for IPv6 addresses.
    --
    -- The range of valid values depends on whether @edns0clientsubnetip@ is an
    -- IPv4 or an IPv6 address:
    --
    -- -   __IPv4__: Specify a value between 0 and 32
    --
    -- -   __IPv6__: Specify a value between 0 and 128
    eDNS0ClientSubnetMask :: Prelude.Maybe Prelude.Text,
    -- | If the resolver that you specified for resolverip supports EDNS0,
    -- specify the IPv4 or IPv6 address of a client in the applicable location,
    -- for example, @192.0.2.44@ or @2001:db8:85a3::8a2e:370:7334@.
    eDNS0ClientSubnetIP :: Prelude.Maybe Prelude.Text,
    -- | The ID of the hosted zone that you want Amazon Route 53 to simulate a
    -- query for.
    hostedZoneId :: ResourceId,
    -- | The name of the resource record set that you want Amazon Route 53 to
    -- simulate a query for.
    recordName :: Prelude.Text,
    -- | The type of the resource record set.
    recordType :: RRType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestDNSAnswer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverIP', 'testDNSAnswer_resolverIP' - If you want to simulate a request from a specific DNS resolver, specify
-- the IP address for that resolver. If you omit this value,
-- @TestDnsAnswer@ uses the IP address of a DNS resolver in the AWS US East
-- (N. Virginia) Region (@us-east-1@).
--
-- 'eDNS0ClientSubnetMask', 'testDNSAnswer_eDNS0ClientSubnetMask' - If you specify an IP address for @edns0clientsubnetip@, you can
-- optionally specify the number of bits of the IP address that you want
-- the checking tool to include in the DNS query. For example, if you
-- specify @192.0.2.44@ for @edns0clientsubnetip@ and @24@ for
-- @edns0clientsubnetmask@, the checking tool will simulate a request from
-- 192.0.2.0\/24. The default value is 24 bits for IPv4 addresses and 64
-- bits for IPv6 addresses.
--
-- The range of valid values depends on whether @edns0clientsubnetip@ is an
-- IPv4 or an IPv6 address:
--
-- -   __IPv4__: Specify a value between 0 and 32
--
-- -   __IPv6__: Specify a value between 0 and 128
--
-- 'eDNS0ClientSubnetIP', 'testDNSAnswer_eDNS0ClientSubnetIP' - If the resolver that you specified for resolverip supports EDNS0,
-- specify the IPv4 or IPv6 address of a client in the applicable location,
-- for example, @192.0.2.44@ or @2001:db8:85a3::8a2e:370:7334@.
--
-- 'hostedZoneId', 'testDNSAnswer_hostedZoneId' - The ID of the hosted zone that you want Amazon Route 53 to simulate a
-- query for.
--
-- 'recordName', 'testDNSAnswer_recordName' - The name of the resource record set that you want Amazon Route 53 to
-- simulate a query for.
--
-- 'recordType', 'testDNSAnswer_recordType' - The type of the resource record set.
newTestDNSAnswer ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'recordName'
  Prelude.Text ->
  -- | 'recordType'
  RRType ->
  TestDNSAnswer
newTestDNSAnswer
  pHostedZoneId_
  pRecordName_
  pRecordType_ =
    TestDNSAnswer'
      { resolverIP = Prelude.Nothing,
        eDNS0ClientSubnetMask = Prelude.Nothing,
        eDNS0ClientSubnetIP = Prelude.Nothing,
        hostedZoneId = pHostedZoneId_,
        recordName = pRecordName_,
        recordType = pRecordType_
      }

-- | If you want to simulate a request from a specific DNS resolver, specify
-- the IP address for that resolver. If you omit this value,
-- @TestDnsAnswer@ uses the IP address of a DNS resolver in the AWS US East
-- (N. Virginia) Region (@us-east-1@).
testDNSAnswer_resolverIP :: Lens.Lens' TestDNSAnswer (Prelude.Maybe Prelude.Text)
testDNSAnswer_resolverIP = Lens.lens (\TestDNSAnswer' {resolverIP} -> resolverIP) (\s@TestDNSAnswer' {} a -> s {resolverIP = a} :: TestDNSAnswer)

-- | If you specify an IP address for @edns0clientsubnetip@, you can
-- optionally specify the number of bits of the IP address that you want
-- the checking tool to include in the DNS query. For example, if you
-- specify @192.0.2.44@ for @edns0clientsubnetip@ and @24@ for
-- @edns0clientsubnetmask@, the checking tool will simulate a request from
-- 192.0.2.0\/24. The default value is 24 bits for IPv4 addresses and 64
-- bits for IPv6 addresses.
--
-- The range of valid values depends on whether @edns0clientsubnetip@ is an
-- IPv4 or an IPv6 address:
--
-- -   __IPv4__: Specify a value between 0 and 32
--
-- -   __IPv6__: Specify a value between 0 and 128
testDNSAnswer_eDNS0ClientSubnetMask :: Lens.Lens' TestDNSAnswer (Prelude.Maybe Prelude.Text)
testDNSAnswer_eDNS0ClientSubnetMask = Lens.lens (\TestDNSAnswer' {eDNS0ClientSubnetMask} -> eDNS0ClientSubnetMask) (\s@TestDNSAnswer' {} a -> s {eDNS0ClientSubnetMask = a} :: TestDNSAnswer)

-- | If the resolver that you specified for resolverip supports EDNS0,
-- specify the IPv4 or IPv6 address of a client in the applicable location,
-- for example, @192.0.2.44@ or @2001:db8:85a3::8a2e:370:7334@.
testDNSAnswer_eDNS0ClientSubnetIP :: Lens.Lens' TestDNSAnswer (Prelude.Maybe Prelude.Text)
testDNSAnswer_eDNS0ClientSubnetIP = Lens.lens (\TestDNSAnswer' {eDNS0ClientSubnetIP} -> eDNS0ClientSubnetIP) (\s@TestDNSAnswer' {} a -> s {eDNS0ClientSubnetIP = a} :: TestDNSAnswer)

-- | The ID of the hosted zone that you want Amazon Route 53 to simulate a
-- query for.
testDNSAnswer_hostedZoneId :: Lens.Lens' TestDNSAnswer ResourceId
testDNSAnswer_hostedZoneId = Lens.lens (\TestDNSAnswer' {hostedZoneId} -> hostedZoneId) (\s@TestDNSAnswer' {} a -> s {hostedZoneId = a} :: TestDNSAnswer)

-- | The name of the resource record set that you want Amazon Route 53 to
-- simulate a query for.
testDNSAnswer_recordName :: Lens.Lens' TestDNSAnswer Prelude.Text
testDNSAnswer_recordName = Lens.lens (\TestDNSAnswer' {recordName} -> recordName) (\s@TestDNSAnswer' {} a -> s {recordName = a} :: TestDNSAnswer)

-- | The type of the resource record set.
testDNSAnswer_recordType :: Lens.Lens' TestDNSAnswer RRType
testDNSAnswer_recordType = Lens.lens (\TestDNSAnswer' {recordType} -> recordType) (\s@TestDNSAnswer' {} a -> s {recordType = a} :: TestDNSAnswer)

instance Core.AWSRequest TestDNSAnswer where
  type
    AWSResponse TestDNSAnswer =
      TestDNSAnswerResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          TestDNSAnswerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "Nameserver")
            Prelude.<*> (x Core..@ "RecordName")
            Prelude.<*> (x Core..@ "RecordType")
            Prelude.<*> ( x Core..@? "RecordData" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "RecordDataEntry"
                        )
            Prelude.<*> (x Core..@ "ResponseCode")
            Prelude.<*> (x Core..@ "Protocol")
      )

instance Prelude.Hashable TestDNSAnswer

instance Prelude.NFData TestDNSAnswer

instance Core.ToHeaders TestDNSAnswer where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath TestDNSAnswer where
  toPath = Prelude.const "/2013-04-01/testdnsanswer"

instance Core.ToQuery TestDNSAnswer where
  toQuery TestDNSAnswer' {..} =
    Prelude.mconcat
      [ "resolverip" Core.=: resolverIP,
        "edns0clientsubnetmask"
          Core.=: eDNS0ClientSubnetMask,
        "edns0clientsubnetip" Core.=: eDNS0ClientSubnetIP,
        "hostedzoneid" Core.=: hostedZoneId,
        "recordname" Core.=: recordName,
        "recordtype" Core.=: recordType
      ]

-- | A complex type that contains the response to a @TestDNSAnswer@ request.
--
-- /See:/ 'newTestDNSAnswerResponse' smart constructor.
data TestDNSAnswerResponse = TestDNSAnswerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Route 53 name server used to respond to the request.
    nameserver :: Prelude.Text,
    -- | The name of the resource record set that you submitted a request for.
    recordName :: Prelude.Text,
    -- | The type of the resource record set that you submitted a request for.
    recordType :: RRType,
    -- | A list that contains values that Amazon Route 53 returned for this
    -- resource record set.
    recordData :: [Prelude.Text],
    -- | A code that indicates whether the request is valid or not. The most
    -- common response code is @NOERROR@, meaning that the request is valid. If
    -- the response is not valid, Amazon Route 53 returns a response code that
    -- describes the error. For a list of possible response codes, see
    -- <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES>
    -- on the IANA website.
    responseCode :: Prelude.Text,
    -- | The protocol that Amazon Route 53 used to respond to the request, either
    -- @UDP@ or @TCP@.
    protocol :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestDNSAnswerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'testDNSAnswerResponse_httpStatus' - The response's http status code.
--
-- 'nameserver', 'testDNSAnswerResponse_nameserver' - The Amazon Route 53 name server used to respond to the request.
--
-- 'recordName', 'testDNSAnswerResponse_recordName' - The name of the resource record set that you submitted a request for.
--
-- 'recordType', 'testDNSAnswerResponse_recordType' - The type of the resource record set that you submitted a request for.
--
-- 'recordData', 'testDNSAnswerResponse_recordData' - A list that contains values that Amazon Route 53 returned for this
-- resource record set.
--
-- 'responseCode', 'testDNSAnswerResponse_responseCode' - A code that indicates whether the request is valid or not. The most
-- common response code is @NOERROR@, meaning that the request is valid. If
-- the response is not valid, Amazon Route 53 returns a response code that
-- describes the error. For a list of possible response codes, see
-- <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES>
-- on the IANA website.
--
-- 'protocol', 'testDNSAnswerResponse_protocol' - The protocol that Amazon Route 53 used to respond to the request, either
-- @UDP@ or @TCP@.
newTestDNSAnswerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'nameserver'
  Prelude.Text ->
  -- | 'recordName'
  Prelude.Text ->
  -- | 'recordType'
  RRType ->
  -- | 'responseCode'
  Prelude.Text ->
  -- | 'protocol'
  Prelude.Text ->
  TestDNSAnswerResponse
newTestDNSAnswerResponse
  pHttpStatus_
  pNameserver_
  pRecordName_
  pRecordType_
  pResponseCode_
  pProtocol_ =
    TestDNSAnswerResponse'
      { httpStatus = pHttpStatus_,
        nameserver = pNameserver_,
        recordName = pRecordName_,
        recordType = pRecordType_,
        recordData = Prelude.mempty,
        responseCode = pResponseCode_,
        protocol = pProtocol_
      }

-- | The response's http status code.
testDNSAnswerResponse_httpStatus :: Lens.Lens' TestDNSAnswerResponse Prelude.Int
testDNSAnswerResponse_httpStatus = Lens.lens (\TestDNSAnswerResponse' {httpStatus} -> httpStatus) (\s@TestDNSAnswerResponse' {} a -> s {httpStatus = a} :: TestDNSAnswerResponse)

-- | The Amazon Route 53 name server used to respond to the request.
testDNSAnswerResponse_nameserver :: Lens.Lens' TestDNSAnswerResponse Prelude.Text
testDNSAnswerResponse_nameserver = Lens.lens (\TestDNSAnswerResponse' {nameserver} -> nameserver) (\s@TestDNSAnswerResponse' {} a -> s {nameserver = a} :: TestDNSAnswerResponse)

-- | The name of the resource record set that you submitted a request for.
testDNSAnswerResponse_recordName :: Lens.Lens' TestDNSAnswerResponse Prelude.Text
testDNSAnswerResponse_recordName = Lens.lens (\TestDNSAnswerResponse' {recordName} -> recordName) (\s@TestDNSAnswerResponse' {} a -> s {recordName = a} :: TestDNSAnswerResponse)

-- | The type of the resource record set that you submitted a request for.
testDNSAnswerResponse_recordType :: Lens.Lens' TestDNSAnswerResponse RRType
testDNSAnswerResponse_recordType = Lens.lens (\TestDNSAnswerResponse' {recordType} -> recordType) (\s@TestDNSAnswerResponse' {} a -> s {recordType = a} :: TestDNSAnswerResponse)

-- | A list that contains values that Amazon Route 53 returned for this
-- resource record set.
testDNSAnswerResponse_recordData :: Lens.Lens' TestDNSAnswerResponse [Prelude.Text]
testDNSAnswerResponse_recordData = Lens.lens (\TestDNSAnswerResponse' {recordData} -> recordData) (\s@TestDNSAnswerResponse' {} a -> s {recordData = a} :: TestDNSAnswerResponse) Prelude.. Lens._Coerce

-- | A code that indicates whether the request is valid or not. The most
-- common response code is @NOERROR@, meaning that the request is valid. If
-- the response is not valid, Amazon Route 53 returns a response code that
-- describes the error. For a list of possible response codes, see
-- <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES>
-- on the IANA website.
testDNSAnswerResponse_responseCode :: Lens.Lens' TestDNSAnswerResponse Prelude.Text
testDNSAnswerResponse_responseCode = Lens.lens (\TestDNSAnswerResponse' {responseCode} -> responseCode) (\s@TestDNSAnswerResponse' {} a -> s {responseCode = a} :: TestDNSAnswerResponse)

-- | The protocol that Amazon Route 53 used to respond to the request, either
-- @UDP@ or @TCP@.
testDNSAnswerResponse_protocol :: Lens.Lens' TestDNSAnswerResponse Prelude.Text
testDNSAnswerResponse_protocol = Lens.lens (\TestDNSAnswerResponse' {protocol} -> protocol) (\s@TestDNSAnswerResponse' {} a -> s {protocol = a} :: TestDNSAnswerResponse)

instance Prelude.NFData TestDNSAnswerResponse
