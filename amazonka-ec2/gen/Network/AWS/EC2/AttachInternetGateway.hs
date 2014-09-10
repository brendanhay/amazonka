{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AttachInternetGateway
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
module Network.AWS.EC2.AttachInternetGateway
    (
    -- * Request
      AttachInternetGateway
    -- ** Request constructor
    , mkAttachInternetGateway
    -- ** Request lenses
    , aigInternetGatewayId
    , aigVpcId

    -- * Response
    , AttachInternetGatewayResponse
    -- ** Response constructor
    , mkAttachInternetGatewayResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data AttachInternetGateway = AttachInternetGateway
    { _aigInternetGatewayId :: !Text
    , _aigVpcId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachInternetGateway' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InternetGatewayId ::@ @Text@
--
-- * @VpcId ::@ @Text@
--
mkAttachInternetGateway :: Text -- ^ 'aigInternetGatewayId'
                        -> Text -- ^ 'aigVpcId'
                        -> AttachInternetGateway
mkAttachInternetGateway p1 p2 = AttachInternetGateway
    { _aigInternetGatewayId = p1
    , _aigVpcId = p2
    }

-- | The ID of the Internet gateway.
aigInternetGatewayId :: Lens' AttachInternetGateway Text
aigInternetGatewayId =
    lens _aigInternetGatewayId (\s a -> s { _aigInternetGatewayId = a })

-- | The ID of the VPC.
aigVpcId :: Lens' AttachInternetGateway Text
aigVpcId = lens _aigVpcId (\s a -> s { _aigVpcId = a })

instance ToQuery AttachInternetGateway where
    toQuery = genericQuery def

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachInternetGatewayResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkAttachInternetGatewayResponse :: AttachInternetGatewayResponse
mkAttachInternetGatewayResponse = AttachInternetGatewayResponse

instance AWSRequest AttachInternetGateway where
    type Sv AttachInternetGateway = EC2
    type Rs AttachInternetGateway = AttachInternetGatewayResponse

    request = post "AttachInternetGateway"
    response _ = nullaryResponse AttachInternetGatewayResponse
