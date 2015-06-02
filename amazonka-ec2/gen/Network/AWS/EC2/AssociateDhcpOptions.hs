{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AssociateDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Associates a set of DHCP options (that you've previously created) with the
-- specified VPC, or associates no DHCP options with the VPC.
--
-- After you associate the options with the VPC, any existing instances and all
-- new instances that you launch in that VPC use the options. You don't need to
-- restart or relaunch the instances. They automatically pick up the changes
-- within a few hours, depending on how frequently the instance renews its DHCP
-- lease. You can explicitly renew the lease using the operating system on the
-- instance.
--
-- For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual PrivateCloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateDhcpOptions.html>
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
import qualified GHC.Exts

data AssociateDhcpOptions = AssociateDhcpOptions
    { _adoDhcpOptionsId :: Text
    , _adoDryRun        :: Maybe Bool
    , _adoVpcId         :: Text
    } deriving (Eq, Ord, Read, Show)

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

-- | The ID of the DHCP options set, or 'default' to associate no DHCP options with
-- the VPC.
adoDhcpOptionsId :: Lens' AssociateDhcpOptions Text
adoDhcpOptionsId = lens _adoDhcpOptionsId (\s a -> s { _adoDhcpOptionsId = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
adoDryRun :: Lens' AssociateDhcpOptions (Maybe Bool)
adoDryRun = lens _adoDryRun (\s a -> s { _adoDryRun = a })

-- | The ID of the VPC.
adoVpcId :: Lens' AssociateDhcpOptions Text
adoVpcId = lens _adoVpcId (\s a -> s { _adoVpcId = a })

data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AssociateDhcpOptionsResponse' constructor.
associateDhcpOptionsResponse :: AssociateDhcpOptionsResponse
associateDhcpOptionsResponse = AssociateDhcpOptionsResponse

instance ToPath AssociateDhcpOptions where
    toPath = const "/"

instance ToQuery AssociateDhcpOptions where
    toQuery AssociateDhcpOptions{..} = mconcat
        [ "DhcpOptionsId" =? _adoDhcpOptionsId
        , "DryRun"        =? _adoDryRun
        , "VpcId"         =? _adoVpcId
        ]

instance ToHeaders AssociateDhcpOptions

instance AWSRequest AssociateDhcpOptions where
    type Sv AssociateDhcpOptions = EC2
    type Rs AssociateDhcpOptions = AssociateDhcpOptionsResponse

    request  = post "AssociateDhcpOptions"
    response = nullResponse AssociateDhcpOptionsResponse
