{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DetachInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Detaches an Internet gateway from a VPC, disabling connectivity between the
-- Internet and the VPC. The VPC must not contain any running instances with
-- Elastic IP addresses. Example The example detaches the specified Internet
-- gateway from the specified VPC.
-- https://ec2.amazonaws.com/?Action=DetachInternetGateway
-- &amp;InternetGatewayId=igw-eaad4883 &amp;VpcId=vpc-11ad4878 &amp;AUTHPARAMS
-- &lt;DetachInternetGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DetachInternetGatewayResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DetachInternetGateway
    (
    -- * Request
      DetachInternetGateway
    -- ** Request constructor
    , detachInternetGateway
    -- ** Request lenses
    , diguInternetGatewayId
    , diguVpcId

    -- * Response
    , DetachInternetGatewayResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DetachInternetGateway' request.
detachInternetGateway :: Text -- ^ 'diguInternetGatewayId'
                      -> Text -- ^ 'diguVpcId'
                      -> DetachInternetGateway
detachInternetGateway p1 p2 = DetachInternetGateway
    { _diguInternetGatewayId = p1
    , _diguVpcId = p2
    }
{-# INLINE detachInternetGateway #-}

data DetachInternetGateway = DetachInternetGateway
    { _diguInternetGatewayId :: Text
      -- ^ The ID of the Internet gateway.
    , _diguVpcId :: Text
      -- ^ The ID of the VPC.
    } deriving (Show, Generic)

-- | The ID of the Internet gateway.
diguInternetGatewayId :: Lens' DetachInternetGateway (Text)
diguInternetGatewayId f x =
    f (_diguInternetGatewayId x)
        <&> \y -> x { _diguInternetGatewayId = y }
{-# INLINE diguInternetGatewayId #-}

-- | The ID of the VPC.
diguVpcId :: Lens' DetachInternetGateway (Text)
diguVpcId f x =
    f (_diguVpcId x)
        <&> \y -> x { _diguVpcId = y }
{-# INLINE diguVpcId #-}

instance ToQuery DetachInternetGateway where
    toQuery = genericQuery def

data DetachInternetGatewayResponse = DetachInternetGatewayResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DetachInternetGateway where
    type Sv DetachInternetGateway = EC2
    type Rs DetachInternetGateway = DetachInternetGatewayResponse

    request = post "DetachInternetGateway"
    response _ = nullaryResponse DetachInternetGatewayResponse
