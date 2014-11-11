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

-- Module      : Network.AWS.EC2.AssociateDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates a set of DHCP options (that you've previously created) with the
-- specified VPC, or associates no DHCP options with the VPC. After you
-- associate the options with the VPC, any existing instances and all new
-- instances that you launch in that VPC use the options. You don't need to
-- restart or relaunch the instances. They automatically pick up the changes
-- within a few hours, depending on how frequently the instance renews its
-- DHCP lease. You can explicitly renew the lease using the operating system
-- on the instance. For more information, see DHCP Options Sets in the Amazon
-- Virtual Private Cloud User Guide.
module Network.AWS.EC2.AssociateDhcpOptions
    (
    -- * Request
      AssociateDhcpOptions
    -- ** Request constructor
    , associateDhcpOptions
    -- ** Request lenses
    , adoDhcpOptionsId
    , adoDryRun
    , adoVpcId

    -- * Response
    , AssociateDhcpOptionsResponse
    -- ** Response constructor
    , associateDhcpOptionsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data AssociateDhcpOptions = AssociateDhcpOptions
    { _adoDhcpOptionsId :: Text
    , _adoDryRun        :: Maybe Bool
    , _adoVpcId         :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AssociateDhcpOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adoDhcpOptionsId' @::@ 'Text'
--
-- * 'adoDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'adoVpcId' @::@ 'Text'
--
associateDhcpOptions :: Text -- ^ 'adoDhcpOptionsId'
                     -> Text -- ^ 'adoVpcId'
                     -> AssociateDhcpOptions
associateDhcpOptions p1 p2 = AssociateDhcpOptions
    { _adoDhcpOptionsId = p1
    , _adoVpcId         = p2
    , _adoDryRun        = Nothing
    }

-- | The ID of the DHCP options set, or default to associate no DHCP options
-- with the VPC.
adoDhcpOptionsId :: Lens' AssociateDhcpOptions Text
adoDhcpOptionsId = lens _adoDhcpOptionsId (\s a -> s { _adoDhcpOptionsId = a })

adoDryRun :: Lens' AssociateDhcpOptions (Maybe Bool)
adoDryRun = lens _adoDryRun (\s a -> s { _adoDryRun = a })

-- | The ID of the VPC.
adoVpcId :: Lens' AssociateDhcpOptions Text
adoVpcId = lens _adoVpcId (\s a -> s { _adoVpcId = a })
instance ToQuery AssociateDhcpOptions

instance ToPath AssociateDhcpOptions where
    toPath = const "/"

data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AssociateDhcpOptionsResponse' constructor.
associateDhcpOptionsResponse :: AssociateDhcpOptionsResponse
associateDhcpOptionsResponse = AssociateDhcpOptionsResponse
instance FromXML AssociateDhcpOptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AssociateDhcpOptionsResponse"

instance AWSRequest AssociateDhcpOptions where
    type Sv AssociateDhcpOptions = EC2
    type Rs AssociateDhcpOptions = AssociateDhcpOptionsResponse

    request  = post "AssociateDhcpOptions"
    response = nullaryResponse AssociateDhcpOptionsResponse
