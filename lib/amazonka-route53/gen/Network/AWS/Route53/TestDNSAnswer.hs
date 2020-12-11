{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    tdaEDNS0ClientSubnetMask,
    tdaHostedZoneId,
    tdaRecordName,
    tdaRecordType,

    -- * Destructuring the response
    TestDNSAnswerResponse (..),
    mkTestDNSAnswerResponse,

    -- ** Response lenses
    tdarsResponseStatus,
    tdarsNameserver,
    tdarsRecordName,
    tdarsRecordType,
    tdarsRecordData,
    tdarsResponseCode,
    tdarsProtocol,
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
  { resolverIP ::
      Lude.Maybe Lude.Text,
    eDNS0ClientSubnetIP :: Lude.Maybe Lude.Text,
    eDNS0ClientSubnetMask :: Lude.Maybe Lude.Text,
    hostedZoneId :: ResourceId,
    recordName :: Lude.Text,
    recordType :: RecordType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestDNSAnswer' with the minimum fields required to make a request.
--
-- * 'eDNS0ClientSubnetIP' - If the resolver that you specified for resolverip supports EDNS0, specify the IPv4 or IPv6 address of a client in the applicable location, for example, @192.0.2.44@ or @2001:db8:85a3::8a2e:370:7334@ .
-- * 'eDNS0ClientSubnetMask' - If you specify an IP address for @edns0clientsubnetip@ , you can optionally specify the number of bits of the IP address that you want the checking tool to include in the DNS query. For example, if you specify @192.0.2.44@ for @edns0clientsubnetip@ and @24@ for @edns0clientsubnetmask@ , the checking tool will simulate a request from 192.0.2.0/24. The default value is 24 bits for IPv4 addresses and 64 bits for IPv6 addresses.
--
-- The range of valid values depends on whether @edns0clientsubnetip@ is an IPv4 or an IPv6 address:
--
--     * __IPv4__ : Specify a value between 0 and 32
--
--
--     * __IPv6__ : Specify a value between 0 and 128
--
--
-- * 'hostedZoneId' - The ID of the hosted zone that you want Amazon Route 53 to simulate a query for.
-- * 'recordName' - The name of the resource record set that you want Amazon Route 53 to simulate a query for.
-- * 'recordType' - The type of the resource record set.
-- * 'resolverIP' - If you want to simulate a request from a specific DNS resolver, specify the IP address for that resolver. If you omit this value, @TestDnsAnswer@ uses the IP address of a DNS resolver in the AWS US East (N. Virginia) Region (@us-east-1@ ).
mkTestDNSAnswer ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'recordName'
  Lude.Text ->
  -- | 'recordType'
  RecordType ->
  TestDNSAnswer
mkTestDNSAnswer pHostedZoneId_ pRecordName_ pRecordType_ =
  TestDNSAnswer'
    { resolverIP = Lude.Nothing,
      eDNS0ClientSubnetIP = Lude.Nothing,
      eDNS0ClientSubnetMask = Lude.Nothing,
      hostedZoneId = pHostedZoneId_,
      recordName = pRecordName_,
      recordType = pRecordType_
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

-- | The ID of the hosted zone that you want Amazon Route 53 to simulate a query for.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdaHostedZoneId :: Lens.Lens' TestDNSAnswer ResourceId
tdaHostedZoneId = Lens.lens (hostedZoneId :: TestDNSAnswer -> ResourceId) (\s a -> s {hostedZoneId = a} :: TestDNSAnswer)
{-# DEPRECATED tdaHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The name of the resource record set that you want Amazon Route 53 to simulate a query for.
--
-- /Note:/ Consider using 'recordName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdaRecordName :: Lens.Lens' TestDNSAnswer Lude.Text
tdaRecordName = Lens.lens (recordName :: TestDNSAnswer -> Lude.Text) (\s a -> s {recordName = a} :: TestDNSAnswer)
{-# DEPRECATED tdaRecordName "Use generic-lens or generic-optics with 'recordName' instead." #-}

-- | The type of the resource record set.
--
-- /Note:/ Consider using 'recordType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdaRecordType :: Lens.Lens' TestDNSAnswer RecordType
tdaRecordType = Lens.lens (recordType :: TestDNSAnswer -> RecordType) (\s a -> s {recordType = a} :: TestDNSAnswer)
{-# DEPRECATED tdaRecordType "Use generic-lens or generic-optics with 'recordType' instead." #-}

instance Lude.AWSRequest TestDNSAnswer where
  type Rs TestDNSAnswer = TestDNSAnswerResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          TestDNSAnswerResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "Nameserver")
            Lude.<*> (x Lude..@ "RecordName")
            Lude.<*> (x Lude..@ "RecordType")
            Lude.<*> ( x Lude..@? "RecordData" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "RecordDataEntry"
                     )
            Lude.<*> (x Lude..@ "ResponseCode")
            Lude.<*> (x Lude..@ "Protocol")
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
        "edns0clientsubnetmask" Lude.=: eDNS0ClientSubnetMask,
        "hostedzoneid" Lude.=: hostedZoneId,
        "recordname" Lude.=: recordName,
        "recordtype" Lude.=: recordType
      ]

-- | A complex type that contains the response to a @TestDNSAnswer@ request.
--
-- /See:/ 'mkTestDNSAnswerResponse' smart constructor.
data TestDNSAnswerResponse = TestDNSAnswerResponse'
  { responseStatus ::
      Lude.Int,
    nameserver :: Lude.Text,
    recordName :: Lude.Text,
    recordType :: RecordType,
    recordData :: [Lude.Text],
    responseCode :: Lude.Text,
    protocol :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestDNSAnswerResponse' with the minimum fields required to make a request.
--
-- * 'nameserver' - The Amazon Route 53 name server used to respond to the request.
-- * 'protocol' - The protocol that Amazon Route 53 used to respond to the request, either @UDP@ or @TCP@ .
-- * 'recordData' - A list that contains values that Amazon Route 53 returned for this resource record set.
-- * 'recordName' - The name of the resource record set that you submitted a request for.
-- * 'recordType' - The type of the resource record set that you submitted a request for.
-- * 'responseCode' - A code that indicates whether the request is valid or not. The most common response code is @NOERROR@ , meaning that the request is valid. If the response is not valid, Amazon Route 53 returns a response code that describes the error. For a list of possible response codes, see <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES> on the IANA website.
-- * 'responseStatus' - The response status code.
mkTestDNSAnswerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'nameserver'
  Lude.Text ->
  -- | 'recordName'
  Lude.Text ->
  -- | 'recordType'
  RecordType ->
  -- | 'responseCode'
  Lude.Text ->
  -- | 'protocol'
  Lude.Text ->
  TestDNSAnswerResponse
mkTestDNSAnswerResponse
  pResponseStatus_
  pNameserver_
  pRecordName_
  pRecordType_
  pResponseCode_
  pProtocol_ =
    TestDNSAnswerResponse'
      { responseStatus = pResponseStatus_,
        nameserver = pNameserver_,
        recordName = pRecordName_,
        recordType = pRecordType_,
        recordData = Lude.mempty,
        responseCode = pResponseCode_,
        protocol = pProtocol_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsResponseStatus :: Lens.Lens' TestDNSAnswerResponse Lude.Int
tdarsResponseStatus = Lens.lens (responseStatus :: TestDNSAnswerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Route 53 name server used to respond to the request.
--
-- /Note:/ Consider using 'nameserver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsNameserver :: Lens.Lens' TestDNSAnswerResponse Lude.Text
tdarsNameserver = Lens.lens (nameserver :: TestDNSAnswerResponse -> Lude.Text) (\s a -> s {nameserver = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsNameserver "Use generic-lens or generic-optics with 'nameserver' instead." #-}

-- | The name of the resource record set that you submitted a request for.
--
-- /Note:/ Consider using 'recordName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsRecordName :: Lens.Lens' TestDNSAnswerResponse Lude.Text
tdarsRecordName = Lens.lens (recordName :: TestDNSAnswerResponse -> Lude.Text) (\s a -> s {recordName = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsRecordName "Use generic-lens or generic-optics with 'recordName' instead." #-}

-- | The type of the resource record set that you submitted a request for.
--
-- /Note:/ Consider using 'recordType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsRecordType :: Lens.Lens' TestDNSAnswerResponse RecordType
tdarsRecordType = Lens.lens (recordType :: TestDNSAnswerResponse -> RecordType) (\s a -> s {recordType = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsRecordType "Use generic-lens or generic-optics with 'recordType' instead." #-}

-- | A list that contains values that Amazon Route 53 returned for this resource record set.
--
-- /Note:/ Consider using 'recordData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsRecordData :: Lens.Lens' TestDNSAnswerResponse [Lude.Text]
tdarsRecordData = Lens.lens (recordData :: TestDNSAnswerResponse -> [Lude.Text]) (\s a -> s {recordData = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsRecordData "Use generic-lens or generic-optics with 'recordData' instead." #-}

-- | A code that indicates whether the request is valid or not. The most common response code is @NOERROR@ , meaning that the request is valid. If the response is not valid, Amazon Route 53 returns a response code that describes the error. For a list of possible response codes, see <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES> on the IANA website.
--
-- /Note:/ Consider using 'responseCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsResponseCode :: Lens.Lens' TestDNSAnswerResponse Lude.Text
tdarsResponseCode = Lens.lens (responseCode :: TestDNSAnswerResponse -> Lude.Text) (\s a -> s {responseCode = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsResponseCode "Use generic-lens or generic-optics with 'responseCode' instead." #-}

-- | The protocol that Amazon Route 53 used to respond to the request, either @UDP@ or @TCP@ .
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdarsProtocol :: Lens.Lens' TestDNSAnswerResponse Lude.Text
tdarsProtocol = Lens.lens (protocol :: TestDNSAnswerResponse -> Lude.Text) (\s a -> s {protocol = a} :: TestDNSAnswerResponse)
{-# DEPRECATED tdarsProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}
