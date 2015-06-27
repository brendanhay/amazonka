{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.CreateDHCPOptions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a set of DHCP options for your VPC. After creating the set, you
-- must associate it with the VPC, causing all existing and new instances
-- that you launch in the VPC to use this set of DHCP options. The
-- following are the individual DHCP options you can specify. For more
-- information about the options, see
-- <http://www.ietf.org/rfc/rfc2132.txt RFC 2132>.
--
-- -   @domain-name-servers@ - The IP addresses of up to four domain name
--     servers, or @AmazonProvidedDNS@. The default DHCP option set
--     specifies @AmazonProvidedDNS@. If specifying more than one domain
--     name server, specify the IP addresses in a single parameter,
--     separated by commas.
-- -   @domain-name@ - If you\'re using AmazonProvidedDNS in @us-east-1@,
--     specify @ec2.internal@. If you\'re using AmazonProvidedDNS in
--     another region, specify @region.compute.internal@ (for example,
--     @ap-northeast-1.compute.internal@). Otherwise, specify a domain name
--     (for example, @MyCompany.com@). __Important__: Some Linux operating
--     systems accept multiple domain names separated by spaces. However,
--     Windows and other Linux operating systems treat the value as a
--     single domain, which results in unexpected behavior. If your DHCP
--     options set is associated with a VPC that has instances with
--     multiple operating systems, specify only one domain name.
-- -   @ntp-servers@ - The IP addresses of up to four Network Time Protocol
--     (NTP) servers.
-- -   @netbios-name-servers@ - The IP addresses of up to four NetBIOS name
--     servers.
-- -   @netbios-node-type@ - The NetBIOS node type (1, 2, 4, or 8). We
--     recommend that you specify 2 (broadcast and multicast are not
--     currently supported). For more information about these node types,
--     see <http://www.ietf.org/rfc/rfc2132.txt RFC 2132>.
--
-- Your VPC automatically starts out with a set of DHCP options that
-- includes only a DNS server that we provide (AmazonProvidedDNS). If you
-- create a set of options, and if your VPC has an Internet gateway, make
-- sure to set the @domain-name-servers@ option either to
-- @AmazonProvidedDNS@ or to a domain name server of your choice. For more
-- information about DHCP options, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html DHCP Options Sets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateDHCPOptions.html>
module Network.AWS.EC2.CreateDHCPOptions
    (
    -- * Request
      CreateDHCPOptions
    -- ** Request constructor
    , createDHCPOptions
    -- ** Request lenses
    , cdoDryRun
    , cdoDHCPConfigurations

    -- * Response
    , CreateDHCPOptionsResponse
    -- ** Response constructor
    , createDHCPOptionsResponse
    -- ** Response lenses
    , cdorDHCPOptions
    , cdorStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createDHCPOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdoDryRun'
--
-- * 'cdoDHCPConfigurations'
data CreateDHCPOptions = CreateDHCPOptions'
    { _cdoDryRun             :: !(Maybe Bool)
    , _cdoDHCPConfigurations :: ![NewDHCPConfiguration]
    } deriving (Eq,Read,Show)

-- | 'CreateDHCPOptions' smart constructor.
createDHCPOptions :: CreateDHCPOptions
createDHCPOptions =
    CreateDHCPOptions'
    { _cdoDryRun = Nothing
    , _cdoDHCPConfigurations = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cdoDryRun :: Lens' CreateDHCPOptions (Maybe Bool)
cdoDryRun = lens _cdoDryRun (\ s a -> s{_cdoDryRun = a});

-- | A DHCP configuration option.
cdoDHCPConfigurations :: Lens' CreateDHCPOptions [NewDHCPConfiguration]
cdoDHCPConfigurations = lens _cdoDHCPConfigurations (\ s a -> s{_cdoDHCPConfigurations = a});

instance AWSRequest CreateDHCPOptions where
        type Sv CreateDHCPOptions = EC2
        type Rs CreateDHCPOptions = CreateDHCPOptionsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateDHCPOptionsResponse' <$>
                   (x .@? "dhcpOptions") <*> (pure (fromEnum s)))

instance ToHeaders CreateDHCPOptions where
        toHeaders = const mempty

instance ToPath CreateDHCPOptions where
        toPath = const "/"

instance ToQuery CreateDHCPOptions where
        toQuery CreateDHCPOptions'{..}
          = mconcat
              ["Action" =: ("CreateDHCPOptions" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _cdoDryRun,
               toQueryList "item" _cdoDHCPConfigurations]

-- | /See:/ 'createDHCPOptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdorDHCPOptions'
--
-- * 'cdorStatus'
data CreateDHCPOptionsResponse = CreateDHCPOptionsResponse'
    { _cdorDHCPOptions :: !(Maybe DHCPOptions)
    , _cdorStatus      :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateDHCPOptionsResponse' smart constructor.
createDHCPOptionsResponse :: Int -> CreateDHCPOptionsResponse
createDHCPOptionsResponse pStatus =
    CreateDHCPOptionsResponse'
    { _cdorDHCPOptions = Nothing
    , _cdorStatus = pStatus
    }

-- | A set of DHCP options.
cdorDHCPOptions :: Lens' CreateDHCPOptionsResponse (Maybe DHCPOptions)
cdorDHCPOptions = lens _cdorDHCPOptions (\ s a -> s{_cdorDHCPOptions = a});

-- | FIXME: Undocumented member.
cdorStatus :: Lens' CreateDHCPOptionsResponse Int
cdorStatus = lens _cdorStatus (\ s a -> s{_cdorStatus = a});
