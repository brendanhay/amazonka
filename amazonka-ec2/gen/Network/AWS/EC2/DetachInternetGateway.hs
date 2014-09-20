{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DetachInternetGateway
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
module Network.AWS.EC2.DetachInternetGateway
    (
    -- * Request
      DetachInternetGateway
    -- ** Request constructor
    , detachInternetGateway
    -- ** Request lenses
    , dig2InternetGatewayId
    , dig2VpcId

    -- * Response
    , DetachInternetGatewayResponse
    -- ** Response constructor
    , detachInternetGatewayResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DetachInternetGateway = DetachInternetGateway
    { _dig2InternetGatewayId :: Text
    , _dig2VpcId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachInternetGateway' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InternetGatewayId ::@ @Text@
--
-- * @VpcId ::@ @Text@
--
detachInternetGateway :: Text -- ^ 'dig2InternetGatewayId'
                      -> Text -- ^ 'dig2VpcId'
                      -> DetachInternetGateway
detachInternetGateway p1 p2 = DetachInternetGateway
    { _dig2InternetGatewayId = p1
    , _dig2VpcId = p2
    }

-- | The ID of the Internet gateway.
dig2InternetGatewayId :: Lens' DetachInternetGateway Text
dig2InternetGatewayId =
    lens _dig2InternetGatewayId (\s a -> s { _dig2InternetGatewayId = a })

-- | The ID of the VPC.
dig2VpcId :: Lens' DetachInternetGateway Text
dig2VpcId = lens _dig2VpcId (\s a -> s { _dig2VpcId = a })

instance ToQuery DetachInternetGateway where
    toQuery = genericQuery def

data DetachInternetGatewayResponse = DetachInternetGatewayResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachInternetGatewayResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
detachInternetGatewayResponse :: DetachInternetGatewayResponse
detachInternetGatewayResponse = DetachInternetGatewayResponse

instance AWSRequest DetachInternetGateway where
    type Sv DetachInternetGateway = EC2
    type Rs DetachInternetGateway = DetachInternetGatewayResponse

    request = post "DetachInternetGateway"
    response _ = nullaryResponse DetachInternetGatewayResponse
