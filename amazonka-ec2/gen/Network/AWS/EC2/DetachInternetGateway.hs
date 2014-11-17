{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
-- Elastic IP addresses.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachInternetGateway.html>
module Network.AWS.EC2.DetachInternetGateway
    (
    -- * Request
      DetachInternetGateway
    -- ** Request constructor
    , detachInternetGateway
    -- ** Request lenses
    , digDryRun
    , digInternetGatewayId
    , digVpcId

    -- * Response
    , DetachInternetGatewayResponse
    -- ** Response constructor
    , detachInternetGatewayResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DetachInternetGateway = DetachInternetGateway
    { _digDryRun            :: Maybe Bool
    , _digInternetGatewayId :: Text
    , _digVpcId             :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DetachInternetGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'digDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'digInternetGatewayId' @::@ 'Text'
--
-- * 'digVpcId' @::@ 'Text'
--
detachInternetGateway :: Text -- ^ 'digInternetGatewayId'
                      -> Text -- ^ 'digVpcId'
                      -> DetachInternetGateway
detachInternetGateway p1 p2 = DetachInternetGateway
    { _digInternetGatewayId = p1
    , _digVpcId             = p2
    , _digDryRun            = Nothing
    }

digDryRun :: Lens' DetachInternetGateway (Maybe Bool)
digDryRun = lens _digDryRun (\s a -> s { _digDryRun = a })

-- | The ID of the Internet gateway.
digInternetGatewayId :: Lens' DetachInternetGateway Text
digInternetGatewayId =
    lens _digInternetGatewayId (\s a -> s { _digInternetGatewayId = a })

-- | The ID of the VPC.
digVpcId :: Lens' DetachInternetGateway Text
digVpcId = lens _digVpcId (\s a -> s { _digVpcId = a })

data DetachInternetGatewayResponse = DetachInternetGatewayResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DetachInternetGatewayResponse' constructor.
detachInternetGatewayResponse :: DetachInternetGatewayResponse
detachInternetGatewayResponse = DetachInternetGatewayResponse

instance AWSRequest DetachInternetGateway where
    type Sv DetachInternetGateway = EC2
    type Rs DetachInternetGateway = DetachInternetGatewayResponse

    request  = post "DetachInternetGateway"
    response = nullResponse DetachInternetGatewayResponse

instance ToPath DetachInternetGateway where
    toPath = const "/"

instance ToHeaders DetachInternetGateway

instance ToQuery DetachInternetGateway
