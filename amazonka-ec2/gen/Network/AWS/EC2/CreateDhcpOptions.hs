{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.CreateDhcpOptions
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
-- 2132. Your VPC automatically starts out with a set of DHCP options that
-- includes only a DNS server that we provide (AmazonProvidedDNS). If you
-- create a set of options, and if your VPC has an Internet gateway, make sure
-- to set the domain-name-servers option either to AmazonProvidedDNS or to a
-- domain name server of your choice. For more information about DHCP options,
-- see DHCP Options Sets in the Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.CreateDhcpOptions
    (
    -- * Request
      CreateDhcpOptions
    -- ** Request constructor
    , createDhcpOptions
    -- ** Request lenses
    , cdoDhcpConfigurations
    , cdoDryRun

    -- * Response
    , CreateDhcpOptionsResult
    -- ** Response constructor
    , createDhcpOptionsResult
    -- ** Response lenses
    , cdorDhcpOptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CreateDhcpOptions = CreateDhcpOptions
    { _cdoDhcpConfigurations :: [NewDhcpConfiguration]
    , _cdoDryRun             :: Maybe Bool
    } (Eq, Show, Generic)

-- | 'CreateDhcpOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdoDhcpConfigurations' @::@ ['NewDhcpConfiguration']
--
-- * 'cdoDryRun' @::@ 'Maybe' 'Bool'
--
createDhcpOptions :: CreateDhcpOptions
createDhcpOptions = CreateDhcpOptions
    { _cdoDryRun             = Nothing
    , _cdoDhcpConfigurations = mempty
    }

-- | A DHCP configuration option.
cdoDhcpConfigurations :: Lens' CreateDhcpOptions [NewDhcpConfiguration]
cdoDhcpConfigurations =
    lens _cdoDhcpConfigurations (\s a -> s { _cdoDhcpConfigurations = a })

cdoDryRun :: Lens' CreateDhcpOptions (Maybe Bool)
cdoDryRun = lens _cdoDryRun (\s a -> s { _cdoDryRun = a })
instance ToQuery CreateDhcpOptions

instance ToPath CreateDhcpOptions where
    toPath = const "/"

newtype CreateDhcpOptionsResult = CreateDhcpOptionsResult
    { _cdorDhcpOptions :: Maybe DhcpOptions
    } (Eq, Show, Generic)

-- | 'CreateDhcpOptionsResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdorDhcpOptions' @::@ 'Maybe' 'DhcpOptions'
--
createDhcpOptionsResult :: CreateDhcpOptionsResult
createDhcpOptionsResult = CreateDhcpOptionsResult
    { _cdorDhcpOptions = Nothing
    }

-- | A set of DHCP options.
cdorDhcpOptions :: Lens' CreateDhcpOptionsResult (Maybe DhcpOptions)
cdorDhcpOptions = lens _cdorDhcpOptions (\s a -> s { _cdorDhcpOptions = a })

instance FromXML CreateDhcpOptionsResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateDhcpOptionsResult"

instance AWSRequest CreateDhcpOptions where
    type Sv CreateDhcpOptions = EC2
    type Rs CreateDhcpOptions = CreateDhcpOptionsResult

    request  = post "CreateDhcpOptions"
    response = xmlResponse $ \h x -> CreateDhcpOptionsResult
        <$> x %| "dhcpOptions"
