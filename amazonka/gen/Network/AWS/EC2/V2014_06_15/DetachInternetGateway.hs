{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DetachInternetGatewayResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DetachInternetGateway where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DetachInternetGateway' request.
detachInternetGateway :: Text -- ^ '_digrVpcId'
                      -> Text -- ^ '_digrInternetGatewayId'
                      -> DetachInternetGateway
detachInternetGateway p1 p2 = DetachInternetGateway
    { _digrVpcId = p1
    , _digrInternetGatewayId = p2
    , _digrDryRun = Nothing
    }

data DetachInternetGateway = DetachInternetGateway
    { _digrVpcId :: Text
      -- ^ The ID of the VPC.
    , _digrInternetGatewayId :: Text
      -- ^ The ID of the Internet gateway.
    , _digrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

makeLenses ''DetachInternetGateway

instance ToQuery DetachInternetGateway where
    toQuery = genericToQuery def

data DetachInternetGatewayResponse = DetachInternetGatewayResponse
    deriving (Eq, Show, Generic)

makeLenses ''DetachInternetGatewayResponse

instance AWSRequest DetachInternetGateway where
    type Sv DetachInternetGateway = EC2
    type Rs DetachInternetGateway = DetachInternetGatewayResponse

    request = post "DetachInternetGateway"
    response _ _ = return (Right DetachInternetGatewayResponse)
