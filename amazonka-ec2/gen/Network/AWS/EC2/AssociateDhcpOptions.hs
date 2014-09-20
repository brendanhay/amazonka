{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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

-- | Associates a set of DHCP options (that you've previously created) with the
-- specified VPC, or associates no DHCP options with the VPC. After you
-- associate the options with the VPC, any existing instances and all new
-- instances that you launch in that VPC use the options. You don't need to
-- restart or relaunch the instances. They automatically pick up the changes
-- within a few hours, depending on how frequently the instance renews its
-- DHCP lease. You can explicitly renew the lease using the operating system
-- on the instance. For more information, see DHCP Options Sets in the Amazon
-- Virtual Private Cloud User Guide. Example 1 This example associates the
-- DHCP options with the ID dopt-7a8b9c2d with the VPC with the ID
-- vpc-1a2b3c4d. https://ec2.amazonaws.com/?Action=AssociateDhcpOptions
-- &amp;DhcpOptionsId=dopt-7a8b9c2d &amp;VpcId=vpc-1a2b3c4d &amp;AUTHPARAMS
-- &lt;AssociateDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AssociateDhcpOptionsResponse&gt;
-- Example 2 This example changes the VPC with the ID vpc-1a2b3c4d to have no
-- associated DHCP options set.
-- https://ec2.amazonaws.com/?Action=AssociateDhcpOptions
-- &amp;DhcpOptionsId=default &amp;VpcId=vpc-1a2b3c4d &amp;AUTHPARAMS
-- &lt;AssociateDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AssociateDhcpOptionsResponse&gt;.
module Network.AWS.EC2.AssociateDhcpOptions
    (
    -- * Request
      AssociateDhcpOptions
    -- ** Request constructor
    , associateDhcpOptions
    -- ** Request lenses
    , adoDhcpOptionsId
    , adoVpcId

    -- * Response
    , AssociateDhcpOptionsResponse
    -- ** Response constructor
    , associateDhcpOptionsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data AssociateDhcpOptions = AssociateDhcpOptions
    { _adoDhcpOptionsId :: Text
    , _adoVpcId :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssociateDhcpOptions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DhcpOptionsId ::@ @Text@
--
-- * @VpcId ::@ @Text@
--
associateDhcpOptions :: Text -- ^ 'adoDhcpOptionsId'
                     -> Text -- ^ 'adoVpcId'
                     -> AssociateDhcpOptions
associateDhcpOptions p1 p2 = AssociateDhcpOptions
    { _adoDhcpOptionsId = p1
    , _adoVpcId = p2
    }

-- | The ID of the DHCP options set, or default to associate no DHCP options
-- with the VPC.
adoDhcpOptionsId :: Lens' AssociateDhcpOptions Text
adoDhcpOptionsId =
    lens _adoDhcpOptionsId (\s a -> s { _adoDhcpOptionsId = a })

-- | The ID of the VPC.
adoVpcId :: Lens' AssociateDhcpOptions Text
adoVpcId = lens _adoVpcId (\s a -> s { _adoVpcId = a })

instance ToQuery AssociateDhcpOptions where
    toQuery = genericQuery def

data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssociateDhcpOptionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
associateDhcpOptionsResponse :: AssociateDhcpOptionsResponse
associateDhcpOptionsResponse = AssociateDhcpOptionsResponse

instance AWSRequest AssociateDhcpOptions where
    type Sv AssociateDhcpOptions = EC2
    type Rs AssociateDhcpOptions = AssociateDhcpOptionsResponse

    request = post "AssociateDhcpOptions"
    response _ = nullaryResponse AssociateDhcpOptionsResponse
