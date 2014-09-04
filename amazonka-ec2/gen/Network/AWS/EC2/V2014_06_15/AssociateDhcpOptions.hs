{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.AssociateDhcpOptions
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
module Network.AWS.EC2.V2014_06_15.AssociateDhcpOptions
    (
    -- * Request
      AssociateDhcpOptions
    -- ** Request constructor
    , associateDhcpOptions
    -- ** Request lenses
    , adorDhcpOptionsId
    , adorVpcId

    -- * Response
    , AssociateDhcpOptionsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AssociateDhcpOptions' request.
associateDhcpOptions :: Text -- ^ 'adorDhcpOptionsId'
                     -> Text -- ^ 'adorVpcId'
                     -> AssociateDhcpOptions
associateDhcpOptions p1 p2 = AssociateDhcpOptions
    { _adorDhcpOptionsId = p1
    , _adorVpcId = p2
    }
{-# INLINE associateDhcpOptions #-}

data AssociateDhcpOptions = AssociateDhcpOptions
    { _adorDhcpOptionsId :: Text
      -- ^ The ID of the DHCP options set, or default to associate no DHCP
      -- options with the VPC.
    , _adorVpcId :: Text
      -- ^ The ID of the VPC.
    } deriving (Show, Generic)

-- | The ID of the DHCP options set, or default to associate no DHCP options
-- with the VPC.
adorDhcpOptionsId :: Lens' AssociateDhcpOptions Text
adorDhcpOptionsId f x =
    f (_adorDhcpOptionsId x) <&> \y -> x { _adorDhcpOptionsId = y }
{-# INLINE adorDhcpOptionsId #-}

-- | The ID of the VPC.
adorVpcId :: Lens' AssociateDhcpOptions Text
adorVpcId f x =
    f (_adorVpcId x) <&> \y -> x { _adorVpcId = y }
{-# INLINE adorVpcId #-}

instance ToQuery AssociateDhcpOptions where
    toQuery = genericQuery def

data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AssociateDhcpOptions where
    type Sv AssociateDhcpOptions = EC2
    type Rs AssociateDhcpOptions = AssociateDhcpOptionsResponse

    request = post "AssociateDhcpOptions"
    response _ = nullaryResponse AssociateDhcpOptionsResponse
