{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.TestDNSAnswer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the value that Amazon Route 53 returns in response to a DNS request for a specified record name and type. You can optionally specify the IP address of a DNS resolver, an EDNS0 client subnet IP address, and a subnet mask.
--
--
module Network.AWS.Route53.TestDNSAnswer
    (
    -- * Creating a Request
      testDNSAnswer
    , TestDNSAnswer
    -- * Request Lenses
    , tdaResolverIP
    , tdaEDNS0ClientSubnetIP
    , tdaEDNS0ClientSubnetMask
    , tdaHostedZoneId
    , tdaRecordName
    , tdaRecordType

    -- * Destructuring the Response
    , testDNSAnswerResponse
    , TestDNSAnswerResponse
    -- * Response Lenses
    , tdarsResponseStatus
    , tdarsNameserver
    , tdarsRecordName
    , tdarsRecordType
    , tdarsRecordData
    , tdarsResponseCode
    , tdarsProtocol
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | Gets the value that Amazon Route 53 returns in response to a DNS request for a specified record name and type. You can optionally specify the IP address of a DNS resolver, an EDNS0 client subnet IP address, and a subnet mask.
--
--
--
-- /See:/ 'testDNSAnswer' smart constructor.
data TestDNSAnswer = TestDNSAnswer'
  { _tdaResolverIP            :: !(Maybe Text)
  , _tdaEDNS0ClientSubnetIP   :: !(Maybe Text)
  , _tdaEDNS0ClientSubnetMask :: !(Maybe Text)
  , _tdaHostedZoneId          :: !ResourceId
  , _tdaRecordName            :: !Text
  , _tdaRecordType            :: !RecordType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestDNSAnswer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdaResolverIP' - If you want to simulate a request from a specific DNS resolver, specify the IP address for that resolver. If you omit this value, @TestDnsAnswer@ uses the IP address of a DNS resolver in the AWS US East (N. Virginia) Region (@us-east-1@ ).
--
-- * 'tdaEDNS0ClientSubnetIP' - If the resolver that you specified for resolverip supports EDNS0, specify the IPv4 or IPv6 address of a client in the applicable location, for example, @192.0.2.44@ or @2001:db8:85a3::8a2e:370:7334@ .
--
-- * 'tdaEDNS0ClientSubnetMask' - If you specify an IP address for @edns0clientsubnetip@ , you can optionally specify the number of bits of the IP address that you want the checking tool to include in the DNS query. For example, if you specify @192.0.2.44@ for @edns0clientsubnetip@ and @24@ for @edns0clientsubnetmask@ , the checking tool will simulate a request from 192.0.2.0/24. The default value is 24 bits for IPv4 addresses and 64 bits for IPv6 addresses.
--
-- * 'tdaHostedZoneId' - The ID of the hosted zone that you want Amazon Route 53 to simulate a query for.
--
-- * 'tdaRecordName' - The name of the resource record set that you want Amazon Route 53 to simulate a query for.
--
-- * 'tdaRecordType' - The type of the resource record set.
testDNSAnswer
    :: ResourceId -- ^ 'tdaHostedZoneId'
    -> Text -- ^ 'tdaRecordName'
    -> RecordType -- ^ 'tdaRecordType'
    -> TestDNSAnswer
testDNSAnswer pHostedZoneId_ pRecordName_ pRecordType_ =
  TestDNSAnswer'
    { _tdaResolverIP = Nothing
    , _tdaEDNS0ClientSubnetIP = Nothing
    , _tdaEDNS0ClientSubnetMask = Nothing
    , _tdaHostedZoneId = pHostedZoneId_
    , _tdaRecordName = pRecordName_
    , _tdaRecordType = pRecordType_
    }


-- | If you want to simulate a request from a specific DNS resolver, specify the IP address for that resolver. If you omit this value, @TestDnsAnswer@ uses the IP address of a DNS resolver in the AWS US East (N. Virginia) Region (@us-east-1@ ).
tdaResolverIP :: Lens' TestDNSAnswer (Maybe Text)
tdaResolverIP = lens _tdaResolverIP (\ s a -> s{_tdaResolverIP = a})

-- | If the resolver that you specified for resolverip supports EDNS0, specify the IPv4 or IPv6 address of a client in the applicable location, for example, @192.0.2.44@ or @2001:db8:85a3::8a2e:370:7334@ .
tdaEDNS0ClientSubnetIP :: Lens' TestDNSAnswer (Maybe Text)
tdaEDNS0ClientSubnetIP = lens _tdaEDNS0ClientSubnetIP (\ s a -> s{_tdaEDNS0ClientSubnetIP = a})

-- | If you specify an IP address for @edns0clientsubnetip@ , you can optionally specify the number of bits of the IP address that you want the checking tool to include in the DNS query. For example, if you specify @192.0.2.44@ for @edns0clientsubnetip@ and @24@ for @edns0clientsubnetmask@ , the checking tool will simulate a request from 192.0.2.0/24. The default value is 24 bits for IPv4 addresses and 64 bits for IPv6 addresses.
tdaEDNS0ClientSubnetMask :: Lens' TestDNSAnswer (Maybe Text)
tdaEDNS0ClientSubnetMask = lens _tdaEDNS0ClientSubnetMask (\ s a -> s{_tdaEDNS0ClientSubnetMask = a})

-- | The ID of the hosted zone that you want Amazon Route 53 to simulate a query for.
tdaHostedZoneId :: Lens' TestDNSAnswer ResourceId
tdaHostedZoneId = lens _tdaHostedZoneId (\ s a -> s{_tdaHostedZoneId = a})

-- | The name of the resource record set that you want Amazon Route 53 to simulate a query for.
tdaRecordName :: Lens' TestDNSAnswer Text
tdaRecordName = lens _tdaRecordName (\ s a -> s{_tdaRecordName = a})

-- | The type of the resource record set.
tdaRecordType :: Lens' TestDNSAnswer RecordType
tdaRecordType = lens _tdaRecordType (\ s a -> s{_tdaRecordType = a})

instance AWSRequest TestDNSAnswer where
        type Rs TestDNSAnswer = TestDNSAnswerResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 TestDNSAnswerResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Nameserver") <*>
                     (x .@ "RecordName")
                     <*> (x .@ "RecordType")
                     <*>
                     (x .@? "RecordData" .!@ mempty >>=
                        parseXMLList "RecordDataEntry")
                     <*> (x .@ "ResponseCode")
                     <*> (x .@ "Protocol"))

instance Hashable TestDNSAnswer where

instance NFData TestDNSAnswer where

instance ToHeaders TestDNSAnswer where
        toHeaders = const mempty

instance ToPath TestDNSAnswer where
        toPath = const "/2013-04-01/testdnsanswer"

instance ToQuery TestDNSAnswer where
        toQuery TestDNSAnswer'{..}
          = mconcat
              ["resolverip" =: _tdaResolverIP,
               "edns0clientsubnetip" =: _tdaEDNS0ClientSubnetIP,
               "edns0clientsubnetmask" =: _tdaEDNS0ClientSubnetMask,
               "hostedzoneid" =: _tdaHostedZoneId,
               "recordname" =: _tdaRecordName,
               "recordtype" =: _tdaRecordType]

-- | A complex type that contains the response to a @TestDNSAnswer@ request.
--
--
--
-- /See:/ 'testDNSAnswerResponse' smart constructor.
data TestDNSAnswerResponse = TestDNSAnswerResponse'
  { _tdarsResponseStatus :: !Int
  , _tdarsNameserver     :: !Text
  , _tdarsRecordName     :: !Text
  , _tdarsRecordType     :: !RecordType
  , _tdarsRecordData     :: ![Text]
  , _tdarsResponseCode   :: !Text
  , _tdarsProtocol       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestDNSAnswerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdarsResponseStatus' - -- | The response status code.
--
-- * 'tdarsNameserver' - The Amazon Route 53 name server used to respond to the request.
--
-- * 'tdarsRecordName' - The name of the resource record set that you submitted a request for.
--
-- * 'tdarsRecordType' - The type of the resource record set that you submitted a request for.
--
-- * 'tdarsRecordData' - A list that contains values that Amazon Route 53 returned for this resource record set.
--
-- * 'tdarsResponseCode' - A code that indicates whether the request is valid or not. The most common response code is @NOERROR@ , meaning that the request is valid. If the response is not valid, Amazon Route 53 returns a response code that describes the error. For a list of possible response codes, see <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES> on the IANA website.
--
-- * 'tdarsProtocol' - The protocol that Amazon Route 53 used to respond to the request, either @UDP@ or @TCP@ .
testDNSAnswerResponse
    :: Int -- ^ 'tdarsResponseStatus'
    -> Text -- ^ 'tdarsNameserver'
    -> Text -- ^ 'tdarsRecordName'
    -> RecordType -- ^ 'tdarsRecordType'
    -> Text -- ^ 'tdarsResponseCode'
    -> Text -- ^ 'tdarsProtocol'
    -> TestDNSAnswerResponse
testDNSAnswerResponse pResponseStatus_ pNameserver_ pRecordName_ pRecordType_ pResponseCode_ pProtocol_ =
  TestDNSAnswerResponse'
    { _tdarsResponseStatus = pResponseStatus_
    , _tdarsNameserver = pNameserver_
    , _tdarsRecordName = pRecordName_
    , _tdarsRecordType = pRecordType_
    , _tdarsRecordData = mempty
    , _tdarsResponseCode = pResponseCode_
    , _tdarsProtocol = pProtocol_
    }


-- | -- | The response status code.
tdarsResponseStatus :: Lens' TestDNSAnswerResponse Int
tdarsResponseStatus = lens _tdarsResponseStatus (\ s a -> s{_tdarsResponseStatus = a})

-- | The Amazon Route 53 name server used to respond to the request.
tdarsNameserver :: Lens' TestDNSAnswerResponse Text
tdarsNameserver = lens _tdarsNameserver (\ s a -> s{_tdarsNameserver = a})

-- | The name of the resource record set that you submitted a request for.
tdarsRecordName :: Lens' TestDNSAnswerResponse Text
tdarsRecordName = lens _tdarsRecordName (\ s a -> s{_tdarsRecordName = a})

-- | The type of the resource record set that you submitted a request for.
tdarsRecordType :: Lens' TestDNSAnswerResponse RecordType
tdarsRecordType = lens _tdarsRecordType (\ s a -> s{_tdarsRecordType = a})

-- | A list that contains values that Amazon Route 53 returned for this resource record set.
tdarsRecordData :: Lens' TestDNSAnswerResponse [Text]
tdarsRecordData = lens _tdarsRecordData (\ s a -> s{_tdarsRecordData = a}) . _Coerce

-- | A code that indicates whether the request is valid or not. The most common response code is @NOERROR@ , meaning that the request is valid. If the response is not valid, Amazon Route 53 returns a response code that describes the error. For a list of possible response codes, see <http://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-6 DNS RCODES> on the IANA website.
tdarsResponseCode :: Lens' TestDNSAnswerResponse Text
tdarsResponseCode = lens _tdarsResponseCode (\ s a -> s{_tdarsResponseCode = a})

-- | The protocol that Amazon Route 53 used to respond to the request, either @UDP@ or @TCP@ .
tdarsProtocol :: Lens' TestDNSAnswerResponse Text
tdarsProtocol = lens _tdarsProtocol (\ s a -> s{_tdarsProtocol = a})

instance NFData TestDNSAnswerResponse where
