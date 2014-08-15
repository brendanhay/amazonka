{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a set of DHCP options for your VPC. After creating the set, you
-- must associate it with the VPC, causing all existing and new instances that
-- you launch in the VPC to use this set of DHCP options. The following are
-- the individual DHCP options you can specify. For more information about the
-- options, see RFC 2132. domain-name-servers - The IP addresses of up to four
-- domain name servers, or AmazonProvidedDNS. The default DHCP option set
-- specifies AmazonProvidedDNS. If specifying more than one domain name
-- server, specify the IP addresses in a single parameter, separated by
-- commas. domain-name - If you're using AmazonProvidedDNS in us-east-1,
-- specify ec2.internal. If you're using AmazonProvidedDNS in another region,
-- specify region.compute.internal (for example,
-- ap-northeast-1.compute.internal). Otherwise, specify a domain name (for
-- example, MyCompany.com). If specifying more than one domain name, separate
-- them with spaces. ntp-servers - The IP addresses of up to four Network Time
-- Protocol (NTP) servers. netbios-name-servers - The IP addresses of up to
-- four NetBIOS name servers. netbios-node-type - The NetBIOS node type (1, 2,
-- 4, or 8). We recommend that you specify 2 (broadcast and multicast are not
-- currently supported). For more information about these node types, see RFC
-- 2132. For more information about DHCP options, see DHCP Options Sets in the
-- Amazon Virtual Private Cloud User Guide. Example This example creates a set
-- of DHCP options with a domain name example.com and two DNS servers
-- (10.2.5.1 and 10.2.5.2). The DNS servers' IP addresses are specified in a
-- single parameter, separated by commas, to preserve the order in which they
-- are specified. https://ec2.amazonaws.com/?Action=CreateDhcpOptions
-- &amp;DhcpConfiguration.1.Key=domain-name
-- &amp;DhcpConfiguration.1.Value.1=example.com
-- &amp;DhcpConfiguration.2.Key=domain-name-servers
-- &amp;DhcpConfiguration.2.Value.1=10.2.5.1,10.2.5.2 &amp;AUTHPARAMS
-- &lt;CreateDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;dhcpOptions&gt;
-- &lt;dhcpOptionsId&gt;dopt-7a8b9c2d&lt;/dhcpOptionsId&gt;
-- &lt;dhcpConfigurationSet&gt; &lt;item&gt;
-- &lt;key&gt;domain-name&lt;/key&gt; &lt;valueSet&gt; &lt;item&gt;
-- &lt;value&gt;example.com&lt;/value&gt; &lt;/item&gt; &lt;/valueSet&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;key&gt;domain-name-servers&lt;/key&gt;
-- &lt;valueSet&gt; &lt;item&gt; &lt;value&gt;10.2.5.1&lt;/value&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;value&gt;10.2.5.2&lt;/value&gt;
-- &lt;/item&gt; &lt;/valueSet&gt; &lt;/item&gt; &lt;/dhcpConfigurationSet&gt;
-- &lt;tagSet/&gt; &lt;/dhcpOptions&gt; &lt;/CreateDhcpOptionsResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateDhcpOptions where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateDhcpOptions' request.
createDhcpOptions :: [DhcpConfiguration] -- ^ '_cdorDhcpConfigurations'
                  -> CreateDhcpOptions
createDhcpOptions p1 = CreateDhcpOptions
    { _cdorDhcpConfigurations = p1
    , _cdorDryRun = Nothing
    }

data CreateDhcpOptions = CreateDhcpOptions
    { _cdorDhcpConfigurations :: [DhcpConfiguration]
      -- ^ A DHCP configuration option.
    , _cdorDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''CreateDhcpOptions

instance ToQuery CreateDhcpOptions where
    toQuery = genericQuery def

data CreateDhcpOptionsResponse = CreateDhcpOptionsResponse
    { _cdosDhcpOptions :: Maybe DhcpOptions
      -- ^ A set of DHCP options.
    } deriving (Show, Generic)

makeLenses ''CreateDhcpOptionsResponse

instance FromXML CreateDhcpOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDhcpOptions where
    type Sv CreateDhcpOptions = EC2
    type Rs CreateDhcpOptions = CreateDhcpOptionsResponse

    request = post "CreateDhcpOptions"
    response _ = xmlResponse
