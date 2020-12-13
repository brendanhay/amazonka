{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.TestDNSAnswer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the value that Amazon Route 53 returns in response to a DNS request for a specified record name and type. You can optionally specify the IP address of a DNS resolver, an EDNS0 client subnet IP address, and a subnet mask.
module Network.AWS.Route53.TestDNSAnswer
  ( -- * Creating a request
    TestDNSAnswer (..),
    mkTestDNSAnswer,

    -- ** Request lenses
    tdaResolverIP,
    tdaEDNS0ClientSubnetIP,
    tdaRecordName,
    tdaHostedZoneId,
    tdaRecordType,
    tdaEDNS0ClientSubnetMask,

    -- * Destructuring the response
    TestDNSAnswerResponse (..),
    mkTestDNSAnswerResponse,

    -- ** Response lenses
    tdarsRecordName,
    tdarsNameserver,
    tdarsRecordType,
    tdarsProtocol,
    tdarsResponseCode,
    tdarsRecordData,
    tdarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | Gets the value that Amazon Route 53 returns in response to a DNS request for a specified record name and type. You can optionally specify the IP address of a DNS resolver, an EDNS0 client subnet IP address, and a subnet mask.
--
-- /See:/ 'mkTestDNSAnswer' smart constructor.
data TestDNSAnswer = TestDNSAnswer'
  { -- | If you want to simulate a request from a specific DNS resolver, specify the IP address for that resolver. If you omit this value, @TestDnsAnswer@ uses the IP address of a DNS resolver in the AWS US East (N. Virginia) Region (@us-east-1@ ).
    resolverIP :: Lude.Maybe Lude.Text,
    -- | If the resolver that you specified for resolverip supports EDNS0, specify the IPv4 or IPv6 address of a client in the applicable location, for example, @192.0.2.44@ or @2001:db8:85a3::8a2e:370:7334@ .
    eDNS0ClientSubnetIP :: Lude.Maybe Lude.Text,
    -- | The name of the resource record set that you want Amazon Route 53 to simulate a query for.
    recordName :: Lude.Text,
    -- | The ID of the hosted zone that you want Amazon Route 53 to simulate a query for.
    hostedZoneId :: ResourceId,
    -- | The type of the resource record set.
    recordType :: RecordType,
    -- | If you specify an IP address for @edns0clientsubnetip@ , you can optionally specify the number of bits of the IP address that you want the checking tool to include in the DNS query. For example, if you specify @192.0.2.44@ for @edns0clientsubnetip@ and @24@ for @edns0clientsubnetmask@ , the checking tool will simulate a request from 192.0.2.0/24. The default value is 24 bits for IPv4 addresses and 64 bits for IPv6 addresses.
    --
    -- The range of valid values depends on whether @edns0clientsubnetip@ is an IPv4 or an IPv6 address:
    --
    --     * __IPv4__ : Specify a value between 0 and 32
    --
    --
    --     * __IPv6__ : Specify a value between 0 and 128
    eDNS0ClientSubnetMask :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestDNSAnswer' with the minimum fields required to make a request.
--
-- * 'resolverIP' - If you want to simulate a request from a specific DNS resolver, specify the IP address for that resolver. If you omit this value, @TestDnsAnswer@ uses the IP address of a DNS resolver in the AWS US East (N. Virginia) Region (@us-east-1@ ).
-- * 'eDNS0ClientSubnetIP' - If the resolver that you specified for resolverip supports EDNS0, specify the IPv4 or IPv6 address of a client in the applicable location, for example, @192.0.2.44@ or @2001:db8:85a3::8a2e:370:7334@ .
-- * 'recordName' - The name of the resource record set that you want Amazon Route 53 to simulate a query for.
-- * 'hostedZoneId' - The ID of the hosted zone that you want Amazon Route 53 to simulate a query for.
-- * 'recordType' - The type of the resource record set.
-- * 'eDNS0ClientSubnetMask' - If you specify an IP address for @edns0clientsubnetip@ , you can optionally specify the number of bits of the IP address that you want the checking tool to include in the DNS query. For example, if you specify @192.0.2.44@ for @edns0clientsubnetip@ and @24@ for @edns0clientsubnetmask@ , the checking tool will simulate a request from 192.0.2.0/24. The default value is 24 bits for IPv4 addresses and 64 bits for IPv6 addresses.
--
-- The range of valid values depends on whether @edns0clientsubnetip@ is an IPv4 or an IPv6 address:
--
--     * __IPv4__ : Specify a value between 0 and 32
--
--
--     * __IPv6__ : Specify a value between 0 and 128
mkTestDNSAnswer ::
  -- | 'recordName'
  Lude.Text ->
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'recordType'
  RecordType ->
  TestDNSAnswer
mkTestDNSAnswer pRecordName_ pHostedZoneId_ pRecordType_ =
  TestDNSAnswer'
    { resolverIP = Lude.Nothing,
      eDNS0ClientSubnetIP = Lude.Nothing,
      recordName = pRecordName_,
      hostedZoneId = pHostedZoneId_,
      recordType = pRecordType_,
      eDNS0ClientSubnetMask = Lude.Nothing
    }

-- | If you want to simulate a request from a specific DNS resolver, specify the IP address for that resolver. If you omit this value, @TestDnsAnswer@ uses the IP address of a DNS resolver in the AWS US East (N. Virginia) Region (@us-east-1@ ).
--
-- /Note:/ Consider using 'resolverIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdaResolverIP :: Lens.Lens' TestDNSAnswer (Lude.Maybe Lude.Text)
tdaResolverIP = Lens.lens (resolverIP :: TestDNSAnswer -> Lude.Maybe Lude.Text) (\s a -> s {resolverIP = a} :: TestDNSAnswer)
{-# DEPRECATED tdaResolverIP "Use generic-lens or generic-optics with 'resolverIP' instead." #-}

-- | If the resolver that you specified for resolverip supports EDNS0, specify the IPv4 or IPv6 address of a client in the applicable location, for example, @192.0.2.44@ or @2001:db8:85a3::8a2e:370:7334@ .
--
-- /Note:/ Consider using 'eDNS0ClientSubnetIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdaEDNS0ClientSubnetIP :: Lens.Lens' TestDNSAnswer (Lude.Maybe Lude.Text)
tdaEDNS0ClientSubnetIP = Lens.lens (eDNS0ClientSubnetIP :: TestDNSAnswer -> Lude.Maybe Lude.Text) (\s a -> s {eDNS0ClientSubnetIP = a} :: TestDNSAnswer)
{-# DEPRECATED tdaEDNS0ClientSubnetIP "Use generic-lens or generic-optics with 'eDNS0ClientSubnetIP' instead." #-}

-- | The name of the resource record set that you want Amazon Route 53 to simulate a query for.
--
-- /Note:/ Consider using 'recordName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdaRecordName :: Lens.Lens' TestDNSAnswer Lude.Text
tdaRecordName = Lens.lens (recordName :: TestDNSAnswer -> Lude.Text) (\s a -> s {recordName = a} :: TestDNSAnswer)
{-# DEPRECATED tdaRecordName "Use generic-lens or generic-optics with 'recordName' instead." #-}

-- | The ID of the hosted zone that you want Amazon Route 53 to simulate a query for.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdaHostedZoneId :: Lens.Lens' TestDNSAnswer ResourceId
tdaHostedZoneId = Lens.lens (hostedZoneId :: TestDNSAnswer -> ResourceId) (\s a -> s {hostedZoneId = a} :: TestDNSAnswer)
{-# DEPRECATED tdaHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The type of the resource record set.
--
-- /Note:/ Consider using 'recordType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdaRecordType :: Lens.Lens' TestDNSAnswer RecordType
tdaRecordType = Lens.lens (recordType :: TestDNSAnswer -> RecordType) (\s a -> s {recordType = a} :: TestDNSAnswer)
{-# DEPRECATED tdaRecordType "Use generic-lens or generic-optics with 'recordType' instead." #-}

-- | If you specify an IP address for @edns0clientsubnetip@ , you can optionally specify the number of bits of the IP address that you want the checking tool to include in the DNS query. For example, if you specify @192.0.2.44@ for @edns0clientsubnetip@ and @24@ for @edns0clientsubnetmask@ , the checking tool will simulate a request from 192.0.2.0/24. The default value is 24 bits for IPv4 addresses and 64 bits for IPv6 addresses.
--
-- The range of valid values depends on whether @edns0clientsubnetip@ is an IPv4 or an IPv6 address:
--
--     * __IPv4__ : Specify a value between 0 and 32
--
--
--     * __IPv6__ : Specify a value between 0 and 128
--
--
--
-- /Note:/ Consider using 'eDNS0ClientSubnetMask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdaEDNS0ClientSubnetMask :: Lens.Lens' TestDNSAnswer (Lude.Maybe Lude.Text)
tdaEDNS0ClientSubnetMask = Lens.lens (eDNS0ClientSubnetMask :: TestDNSAnswer -> Lude.Maybe Lude.Text) (\s a -> s {eDNS0ClientSubnetMask = a} :: TestDNSAnswer)
{-# DEPRECATED tdaEDNS0ClientSubnetMask "Use generic-lens or generic-optics with 'eDNS0ClientSubnetMask' instead." #-}

instance Lude.AWSRequest TestDNSAnswer where
  type Rs TestDNSAnswer = TestDNSAnswerResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          TestDNSAnswerResponse'
            Lude.<$> (x Lude..@ "RecordName")
            Lude.<*> (x Lude..@ "Nameserver")
            Lude.<*> (x Lude..@ "RecordType")
            Lude.<*> (x Lude..@ "Protocol")
            Lude.<*> (x Lude..@ "ResponseCode")
            Lude.<*> ( x Lude..@? "RecordData" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "RecordDataEntry"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestDNSAnswer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath TestDNSAnswer where
  toPath = Lude.const "/2013-04-01/testdnsanswer"

instance Lude.ToQuery TestDNSAnswer where
  toQuery TestDNSAnswer' {..} =
    Lude.mconcat
      [ "resolverip" Lude.=: resolverIP,
        "edns0clientsubnetip" Lude.=: eDNS0ClientSubnetIP,
        "recordname" Lude.=: recordName,
        "hostedzoneid" Lude.=: hostedZoneId,
        "recordtype" Lude.=: recordType,
        "edns0clientsubnetmask" Lude.=: eDNS0ClientSubnetMask
      ]

-- | A complex type that contains the response to a @TestDNSAnswer@ request.
--
-- /See:/ 'mkTestDNSAnswerResponse' smart constructor.
data TestDNSAnswerResponse = TestDNSAnswerResponse'
  { -- | The name of the resource record set that you submitted a request for.
    recordName :: Lude.Text,
    -- | The Amazon Route 53 name server used to respond to the request.
    nameserver :: Lude.Text,
    -- | The type of the resource record set that you submitted a request for.
    recordType :: RecordType,
    -- | The protocol that Amazon Route 53 used to respond to the request, either @UDP@ or @TCP@ .
    protocol :: Lude.Text,
    -- | A code that indicates whether the request is valid or not. The most common response code is @NOERROR@ , meaning that the request is valid. If the response is not valid, Amazon Route 53 returns a response code that describes the error. For a list of possible response codes, see <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES> on the IANA website.
    responseCode :: Lude.Text,
    -- | A list that contains values that Amazon Route 53 returned for this resource record set.
    recordData :: [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestDNSAnswerResponse' with the minimum fields required to make a request.
--
-- * 'recordName' - The name of the resource record set that you submitted a request for.
-- * 'nameserver' - The Amazon Route 53 name server used to respond to the request.
-- * 'recordType' - The type of the resource record set that you submitted a request for.
-- * 'protocol' - The protocol that Amazon Route 53 used to respond to the request, either @UDP@ or @TCP@ .
-- * 'responseCode' - A code that indicates whether the request is valid or not. The most common response code is @NOERROR@ , meaning that the request is valid. If the response is not valid, Amazon Route 53 returns a response code that describes the error. For a list of possible response codes, see <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES> on the IANA website.
-- * 'recordData' - A list that contains values that Amazon Route 53 returned for this resource record set.
-- * 'responseStatus' - The response status code.
mkTestDNSAnswerResponse ::
  -- | 'recordName'
  Lude.Text ->
  -- | 'nameserver'
  Lude.Text ->
  -- | 'recordType'
  RecordType ->
  -- | 'protocol'
  Lude.Text ->
  -- | 'responseCode'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  TestDNSAnswerResponse
mkTestDNSAnswerResponse
  pRecordName_
  pNameserver_
  pRecordType_
  pProtocol_
  pResponseCode_
  pResponseStatus_ =
    TestDNSAnswerResponse'
      { recordName = pRecordName_,
        nameserver = pNameserver_,
        recordType = pRecordType_,
        protocol = pProtocol_,
        responseCode = pResponseCode_,
        recordData = Lude.mempty,
        responseStatus = pResponseStatus_
      }

-- | The name of the resource record set that you submitted a request for.
--
-- /Note:/ Consider using 'recordName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsRecordName :: Lens.Lens' TestDNSAnswerResponse Lude.Text
tdarsRecordName = Lens.lens (recordName :: TestDNSAnswerResponse -> Lude.Text) (\s a -> s {recordName = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsRecordName "Use generic-lens or generic-optics with 'recordName' instead." #-}

-- | The Amazon Route 53 name server used to respond to the request.
--
-- /Note:/ Consider using 'nameserver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsNameserver :: Lens.Lens' TestDNSAnswerResponse Lude.Text
tdarsNameserver = Lens.lens (nameserver :: TestDNSAnswerResponse -> Lude.Text) (\s a -> s {nameserver = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsNameserver "Use generic-lens or generic-optics with 'nameserver' instead." #-}

-- | The type of the resource record set that you submitted a request for.
--
-- /Note:/ Consider using 'recordType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsRecordType :: Lens.Lens' TestDNSAnswerResponse RecordType
tdarsRecordType = Lens.lens (recordType :: TestDNSAnswerResponse -> RecordType) (\s a -> s {recordType = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsRecordType "Use generic-lens or generic-optics with 'recordType' instead." #-}

-- | The protocol that Amazon Route 53 used to respond to the request, either @UDP@ or @TCP@ .
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsProtocol :: Lens.Lens' TestDNSAnswerResponse Lude.Text
tdarsProtocol = Lens.lens (protocol :: TestDNSAnswerResponse -> Lude.Text) (\s a -> s {protocol = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | A code that indicates whether the request is valid or not. The most common response code is @NOERROR@ , meaning that the request is valid. If the response is not valid, Amazon Route 53 returns a response code that describes the error. For a list of possible response codes, see <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES> on the IANA website.
--
-- /Note:/ Consider using 'responseCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsResponseCode :: Lens.Lens' TestDNSAnswerResponse Lude.Text
tdarsResponseCode = Lens.lens (responseCode :: TestDNSAnswerResponse -> Lude.Text) (\s a -> s {responseCode = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsResponseCode "Use generic-lens or generic-optics with 'responseCode' instead." #-}

-- | A list that contains values that Amazon Route 53 returned for this resource record set.
--
-- /Note:/ Consider using 'recordData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsRecordData :: Lens.Lens' TestDNSAnswerResponse [Lude.Text]
tdarsRecordData = Lens.lens (recordData :: TestDNSAnswerResponse -> [Lude.Text]) (\s a -> s {recordData = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsRecordData "Use generic-lens or generic-optics with 'recordData' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsResponseStatus :: Lens.Lens' TestDNSAnswerResponse Lude.Int
tdarsResponseStatus = Lens.lens (responseStatus :: TestDNSAnswerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
