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
-- Module      : Network.AWS.EC2.CreateDHCPOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a set of DHCP options for your VPC. After creating the set, you must associate it with the VPC, causing all existing and new instances that you launch in the VPC to use this set of DHCP options. The following are the individual DHCP options you can specify. For more information about the options, see <http://www.ietf.org/rfc/rfc2132.txt RFC 2132> .
--
--
--     * @domain-name-servers@ - The IP addresses of up to four domain name servers, or AmazonProvidedDNS. The default DHCP option set specifies AmazonProvidedDNS. If specifying more than one domain name server, specify the IP addresses in a single parameter, separated by commas. If you want your instance to receive a custom DNS hostname as specified in @domain-name@ , you must set @domain-name-servers@ to a custom DNS server.
--
--     * @domain-name@ - If you're using AmazonProvidedDNS in @us-east-1@ , specify @ec2.internal@ . If you're using AmazonProvidedDNS in another region, specify @region.compute.internal@ (for example, @ap-northeast-1.compute.internal@ ). Otherwise, specify a domain name (for example, @MyCompany.com@ ). This value is used to complete unqualified DNS hostnames. __Important__ : Some Linux operating systems accept multiple domain names separated by spaces. However, Windows and other Linux operating systems treat the value as a single domain, which results in unexpected behavior. If your DHCP options set is associated with a VPC that has instances with multiple operating systems, specify only one domain name.
--
--     * @ntp-servers@ - The IP addresses of up to four Network Time Protocol (NTP) servers.
--
--     * @netbios-name-servers@ - The IP addresses of up to four NetBIOS name servers.
--
--     * @netbios-node-type@ - The NetBIOS node type (1, 2, 4, or 8). We recommend that you specify 2 (broadcast and multicast are not currently supported). For more information about these node types, see <http://www.ietf.org/rfc/rfc2132.txt RFC 2132> .
--
--
--
-- Your VPC automatically starts out with a set of DHCP options that includes only a DNS server that we provide (AmazonProvidedDNS). If you create a set of options, and if your VPC has an Internet gateway, make sure to set the @domain-name-servers@ option either to @AmazonProvidedDNS@ or to a domain name server of your choice. For more information about DHCP options, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.CreateDHCPOptions
    (
    -- * Creating a Request
      createDHCPOptions
    , CreateDHCPOptions
    -- * Request Lenses
    , cdoDryRun
    , cdoDHCPConfigurations

    -- * Destructuring the Response
    , createDHCPOptionsResponse
    , CreateDHCPOptionsResponse
    -- * Response Lenses
    , cdorsDHCPOptions
    , cdorsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateDhcpOptions.
--
--
--
-- /See:/ 'createDHCPOptions' smart constructor.
data CreateDHCPOptions = CreateDHCPOptions'
  { _cdoDryRun             :: !(Maybe Bool)
  , _cdoDHCPConfigurations :: ![NewDHCPConfiguration]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDHCPOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdoDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cdoDHCPConfigurations' - A DHCP configuration option.
createDHCPOptions
    :: CreateDHCPOptions
createDHCPOptions =
  CreateDHCPOptions' {_cdoDryRun = Nothing, _cdoDHCPConfigurations = mempty}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cdoDryRun :: Lens' CreateDHCPOptions (Maybe Bool)
cdoDryRun = lens _cdoDryRun (\ s a -> s{_cdoDryRun = a})

-- | A DHCP configuration option.
cdoDHCPConfigurations :: Lens' CreateDHCPOptions [NewDHCPConfiguration]
cdoDHCPConfigurations = lens _cdoDHCPConfigurations (\ s a -> s{_cdoDHCPConfigurations = a}) . _Coerce

instance AWSRequest CreateDHCPOptions where
        type Rs CreateDHCPOptions = CreateDHCPOptionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateDHCPOptionsResponse' <$>
                   (x .@? "dhcpOptions") <*> (pure (fromEnum s)))

instance Hashable CreateDHCPOptions where

instance NFData CreateDHCPOptions where

instance ToHeaders CreateDHCPOptions where
        toHeaders = const mempty

instance ToPath CreateDHCPOptions where
        toPath = const "/"

instance ToQuery CreateDHCPOptions where
        toQuery CreateDHCPOptions'{..}
          = mconcat
              ["Action" =: ("CreateDhcpOptions" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _cdoDryRun,
               toQueryList "DhcpConfiguration"
                 _cdoDHCPConfigurations]

-- | Contains the output of CreateDhcpOptions.
--
--
--
-- /See:/ 'createDHCPOptionsResponse' smart constructor.
data CreateDHCPOptionsResponse = CreateDHCPOptionsResponse'
  { _cdorsDHCPOptions    :: !(Maybe DHCPOptions)
  , _cdorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDHCPOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdorsDHCPOptions' - A set of DHCP options.
--
-- * 'cdorsResponseStatus' - -- | The response status code.
createDHCPOptionsResponse
    :: Int -- ^ 'cdorsResponseStatus'
    -> CreateDHCPOptionsResponse
createDHCPOptionsResponse pResponseStatus_ =
  CreateDHCPOptionsResponse'
    {_cdorsDHCPOptions = Nothing, _cdorsResponseStatus = pResponseStatus_}


-- | A set of DHCP options.
cdorsDHCPOptions :: Lens' CreateDHCPOptionsResponse (Maybe DHCPOptions)
cdorsDHCPOptions = lens _cdorsDHCPOptions (\ s a -> s{_cdorsDHCPOptions = a})

-- | -- | The response status code.
cdorsResponseStatus :: Lens' CreateDHCPOptionsResponse Int
cdorsResponseStatus = lens _cdorsResponseStatus (\ s a -> s{_cdorsResponseStatus = a})

instance NFData CreateDHCPOptionsResponse where
