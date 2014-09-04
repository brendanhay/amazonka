{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.AttachInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches an Internet gateway to a VPC, enabling connectivity between the
-- Internet and the VPC. For more information about your VPC and Internet
-- gateway, see the Amazon Virtual Private Cloud User Guide. Example This
-- example attaches the Internet gateway with the ID igw-eaad4883 to the VPC
-- with the ID vpc-11ad4878.
-- https://ec2.amazonaws.com/?Action=AttachInternetGateway
-- &amp;InternetGatewayId=igw-eaad4883 &amp;VpcId=vpc-11ad4878 &amp;AUTHPARAMS
-- &lt;AttachInternetGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AttachInternetGatewayResponse&gt;.
module Network.AWS.EC2.V2014_06_15.AttachInternetGateway
    (
    -- * Request
      AttachInternetGateway
    -- ** Request constructor
    , attachInternetGateway
    -- ** Request lenses
    , aigrInternetGatewayId
    , aigrVpcId

    -- * Response
    , AttachInternetGatewayResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AttachInternetGateway' request.
attachInternetGateway :: Text -- ^ 'aigrInternetGatewayId'
                      -> Text -- ^ 'aigrVpcId'
                      -> AttachInternetGateway
attachInternetGateway p1 p2 = AttachInternetGateway
    { _aigrInternetGatewayId = p1
    , _aigrVpcId = p2
    }
{-# INLINE attachInternetGateway #-}

data AttachInternetGateway = AttachInternetGateway
    { _aigrInternetGatewayId :: Text
      -- ^ The ID of the Internet gateway.
    , _aigrVpcId :: Text
      -- ^ The ID of the VPC.
    } deriving (Show, Generic)

-- | The ID of the Internet gateway.
aigrInternetGatewayId :: Lens' AttachInternetGateway (Text)
aigrInternetGatewayId f x =
    f (_aigrInternetGatewayId x)
        <&> \y -> x { _aigrInternetGatewayId = y }
{-# INLINE aigrInternetGatewayId #-}

-- | The ID of the VPC.
aigrVpcId :: Lens' AttachInternetGateway (Text)
aigrVpcId f x =
    f (_aigrVpcId x)
        <&> \y -> x { _aigrVpcId = y }
{-# INLINE aigrVpcId #-}

instance ToQuery AttachInternetGateway where
    toQuery = genericQuery def

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AttachInternetGateway where
    type Sv AttachInternetGateway = EC2
    type Rs AttachInternetGateway = AttachInternetGatewayResponse

    request = post "AttachInternetGateway"
    response _ = nullaryResponse AttachInternetGatewayResponse
