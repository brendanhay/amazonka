{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      TestDNSAnswer (..)
    , mkTestDNSAnswer
    -- ** Request lenses
    , tdnsaHostedZoneId
    , tdnsaRecordName
    , tdnsaRecordType
    , tdnsaEDNS0ClientSubnetIP
    , tdnsaEDNS0ClientSubnetMask
    , tdnsaResolverIP

    -- * Destructuring the response
    , TestDNSAnswerResponse (..)
    , mkTestDNSAnswerResponse
    -- ** Response lenses
    , tdnsarrsNameserver
    , tdnsarrsRecordName
    , tdnsarrsRecordType
    , tdnsarrsRecordData
    , tdnsarrsResponseCode
    , tdnsarrsProtocol
    , tdnsarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | Gets the value that Amazon Route 53 returns in response to a DNS request for a specified record name and type. You can optionally specify the IP address of a DNS resolver, an EDNS0 client subnet IP address, and a subnet mask. 
--
-- /See:/ 'mkTestDNSAnswer' smart constructor.
data TestDNSAnswer = TestDNSAnswer'
  { hostedZoneId :: Types.ResourceId
    -- ^ The ID of the hosted zone that you want Amazon Route 53 to simulate a query for.
  , recordName :: Types.RecordName
    -- ^ The name of the resource record set that you want Amazon Route 53 to simulate a query for.
  , recordType :: Types.RecordType
    -- ^ The type of the resource record set.
  , eDNS0ClientSubnetIP :: Core.Maybe Types.IPAddress
    -- ^ If the resolver that you specified for resolverip supports EDNS0, specify the IPv4 or IPv6 address of a client in the applicable location, for example, @192.0.2.44@ or @2001:db8:85a3::8a2e:370:7334@ .
  , eDNS0ClientSubnetMask :: Core.Maybe Types.SubnetMask
    -- ^ If you specify an IP address for @edns0clientsubnetip@ , you can optionally specify the number of bits of the IP address that you want the checking tool to include in the DNS query. For example, if you specify @192.0.2.44@ for @edns0clientsubnetip@ and @24@ for @edns0clientsubnetmask@ , the checking tool will simulate a request from 192.0.2.0/24. The default value is 24 bits for IPv4 addresses and 64 bits for IPv6 addresses.
--
-- The range of valid values depends on whether @edns0clientsubnetip@ is an IPv4 or an IPv6 address:
--
--     * __IPv4__ : Specify a value between 0 and 32
--
--
--     * __IPv6__ : Specify a value between 0 and 128
--
--
  , resolverIP :: Core.Maybe Types.IPAddress
    -- ^ If you want to simulate a request from a specific DNS resolver, specify the IP address for that resolver. If you omit this value, @TestDnsAnswer@ uses the IP address of a DNS resolver in the AWS US East (N. Virginia) Region (@us-east-1@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestDNSAnswer' value with any optional fields omitted.
mkTestDNSAnswer
    :: Types.ResourceId -- ^ 'hostedZoneId'
    -> Types.RecordName -- ^ 'recordName'
    -> Types.RecordType -- ^ 'recordType'
    -> TestDNSAnswer
mkTestDNSAnswer hostedZoneId recordName recordType
  = TestDNSAnswer'{hostedZoneId, recordName, recordType,
                   eDNS0ClientSubnetIP = Core.Nothing,
                   eDNS0ClientSubnetMask = Core.Nothing, resolverIP = Core.Nothing}

-- | The ID of the hosted zone that you want Amazon Route 53 to simulate a query for.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsaHostedZoneId :: Lens.Lens' TestDNSAnswer Types.ResourceId
tdnsaHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE tdnsaHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

-- | The name of the resource record set that you want Amazon Route 53 to simulate a query for.
--
-- /Note:/ Consider using 'recordName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsaRecordName :: Lens.Lens' TestDNSAnswer Types.RecordName
tdnsaRecordName = Lens.field @"recordName"
{-# INLINEABLE tdnsaRecordName #-}
{-# DEPRECATED recordName "Use generic-lens or generic-optics with 'recordName' instead"  #-}

-- | The type of the resource record set.
--
-- /Note:/ Consider using 'recordType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsaRecordType :: Lens.Lens' TestDNSAnswer Types.RecordType
tdnsaRecordType = Lens.field @"recordType"
{-# INLINEABLE tdnsaRecordType #-}
{-# DEPRECATED recordType "Use generic-lens or generic-optics with 'recordType' instead"  #-}

-- | If the resolver that you specified for resolverip supports EDNS0, specify the IPv4 or IPv6 address of a client in the applicable location, for example, @192.0.2.44@ or @2001:db8:85a3::8a2e:370:7334@ .
--
-- /Note:/ Consider using 'eDNS0ClientSubnetIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsaEDNS0ClientSubnetIP :: Lens.Lens' TestDNSAnswer (Core.Maybe Types.IPAddress)
tdnsaEDNS0ClientSubnetIP = Lens.field @"eDNS0ClientSubnetIP"
{-# INLINEABLE tdnsaEDNS0ClientSubnetIP #-}
{-# DEPRECATED eDNS0ClientSubnetIP "Use generic-lens or generic-optics with 'eDNS0ClientSubnetIP' instead"  #-}

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
tdnsaEDNS0ClientSubnetMask :: Lens.Lens' TestDNSAnswer (Core.Maybe Types.SubnetMask)
tdnsaEDNS0ClientSubnetMask = Lens.field @"eDNS0ClientSubnetMask"
{-# INLINEABLE tdnsaEDNS0ClientSubnetMask #-}
{-# DEPRECATED eDNS0ClientSubnetMask "Use generic-lens or generic-optics with 'eDNS0ClientSubnetMask' instead"  #-}

-- | If you want to simulate a request from a specific DNS resolver, specify the IP address for that resolver. If you omit this value, @TestDnsAnswer@ uses the IP address of a DNS resolver in the AWS US East (N. Virginia) Region (@us-east-1@ ).
--
-- /Note:/ Consider using 'resolverIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsaResolverIP :: Lens.Lens' TestDNSAnswer (Core.Maybe Types.IPAddress)
tdnsaResolverIP = Lens.field @"resolverIP"
{-# INLINEABLE tdnsaResolverIP #-}
{-# DEPRECATED resolverIP "Use generic-lens or generic-optics with 'resolverIP' instead"  #-}

instance Core.ToQuery TestDNSAnswer where
        toQuery TestDNSAnswer{..}
          = Core.toQueryPair "hostedzoneid" hostedZoneId Core.<>
              Core.toQueryPair "recordname" recordName
              Core.<> Core.toQueryPair "recordtype" recordType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "edns0clientsubnetip")
                eDNS0ClientSubnetIP
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "edns0clientsubnetmask")
                eDNS0ClientSubnetMask
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "resolverip") resolverIP

instance Core.ToHeaders TestDNSAnswer where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest TestDNSAnswer where
        type Rs TestDNSAnswer = TestDNSAnswerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2013-04-01/testdnsanswer",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 TestDNSAnswerResponse' Core.<$>
                   (x Core..@ "Nameserver") Core.<*> x Core..@ "RecordName" Core.<*>
                     x Core..@ "RecordType"
                     Core.<*>
                     x Core..@ "RecordData" Core..@! Core.mempty Core..<@>
                       Core.parseXMLList "RecordDataEntry"
                     Core.<*> x Core..@ "ResponseCode"
                     Core.<*> x Core..@ "Protocol"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response to a @TestDNSAnswer@ request. 
--
-- /See:/ 'mkTestDNSAnswerResponse' smart constructor.
data TestDNSAnswerResponse = TestDNSAnswerResponse'
  { nameserver :: Types.Nameserver
    -- ^ The Amazon Route 53 name server used to respond to the request.
  , recordName :: Types.RecordName
    -- ^ The name of the resource record set that you submitted a request for.
  , recordType :: Types.RecordType
    -- ^ The type of the resource record set that you submitted a request for.
  , recordData :: [Types.RecordDataEntry]
    -- ^ A list that contains values that Amazon Route 53 returned for this resource record set.
  , responseCode :: Types.ResponseCode
    -- ^ A code that indicates whether the request is valid or not. The most common response code is @NOERROR@ , meaning that the request is valid. If the response is not valid, Amazon Route 53 returns a response code that describes the error. For a list of possible response codes, see <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES> on the IANA website. 
  , protocol :: Types.TransportProtocol
    -- ^ The protocol that Amazon Route 53 used to respond to the request, either @UDP@ or @TCP@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestDNSAnswerResponse' value with any optional fields omitted.
mkTestDNSAnswerResponse
    :: Types.Nameserver -- ^ 'nameserver'
    -> Types.RecordName -- ^ 'recordName'
    -> Types.RecordType -- ^ 'recordType'
    -> Types.ResponseCode -- ^ 'responseCode'
    -> Types.TransportProtocol -- ^ 'protocol'
    -> Core.Int -- ^ 'responseStatus'
    -> TestDNSAnswerResponse
mkTestDNSAnswerResponse nameserver recordName recordType
  responseCode protocol responseStatus
  = TestDNSAnswerResponse'{nameserver, recordName, recordType,
                           recordData = Core.mempty, responseCode, protocol, responseStatus}

-- | The Amazon Route 53 name server used to respond to the request.
--
-- /Note:/ Consider using 'nameserver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsarrsNameserver :: Lens.Lens' TestDNSAnswerResponse Types.Nameserver
tdnsarrsNameserver = Lens.field @"nameserver"
{-# INLINEABLE tdnsarrsNameserver #-}
{-# DEPRECATED nameserver "Use generic-lens or generic-optics with 'nameserver' instead"  #-}

-- | The name of the resource record set that you submitted a request for.
--
-- /Note:/ Consider using 'recordName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsarrsRecordName :: Lens.Lens' TestDNSAnswerResponse Types.RecordName
tdnsarrsRecordName = Lens.field @"recordName"
{-# INLINEABLE tdnsarrsRecordName #-}
{-# DEPRECATED recordName "Use generic-lens or generic-optics with 'recordName' instead"  #-}

-- | The type of the resource record set that you submitted a request for.
--
-- /Note:/ Consider using 'recordType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsarrsRecordType :: Lens.Lens' TestDNSAnswerResponse Types.RecordType
tdnsarrsRecordType = Lens.field @"recordType"
{-# INLINEABLE tdnsarrsRecordType #-}
{-# DEPRECATED recordType "Use generic-lens or generic-optics with 'recordType' instead"  #-}

-- | A list that contains values that Amazon Route 53 returned for this resource record set.
--
-- /Note:/ Consider using 'recordData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsarrsRecordData :: Lens.Lens' TestDNSAnswerResponse [Types.RecordDataEntry]
tdnsarrsRecordData = Lens.field @"recordData"
{-# INLINEABLE tdnsarrsRecordData #-}
{-# DEPRECATED recordData "Use generic-lens or generic-optics with 'recordData' instead"  #-}

-- | A code that indicates whether the request is valid or not. The most common response code is @NOERROR@ , meaning that the request is valid. If the response is not valid, Amazon Route 53 returns a response code that describes the error. For a list of possible response codes, see <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES> on the IANA website. 
--
-- /Note:/ Consider using 'responseCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsarrsResponseCode :: Lens.Lens' TestDNSAnswerResponse Types.ResponseCode
tdnsarrsResponseCode = Lens.field @"responseCode"
{-# INLINEABLE tdnsarrsResponseCode #-}
{-# DEPRECATED responseCode "Use generic-lens or generic-optics with 'responseCode' instead"  #-}

-- | The protocol that Amazon Route 53 used to respond to the request, either @UDP@ or @TCP@ . 
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsarrsProtocol :: Lens.Lens' TestDNSAnswerResponse Types.TransportProtocol
tdnsarrsProtocol = Lens.field @"protocol"
{-# INLINEABLE tdnsarrsProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdnsarrsResponseStatus :: Lens.Lens' TestDNSAnswerResponse Core.Int
tdnsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE tdnsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
