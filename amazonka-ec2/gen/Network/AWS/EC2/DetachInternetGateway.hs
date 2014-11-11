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
module Network.AWS.EC2.DetachInternetGateway
    (
    -- * Request
      DetachInternetGateway
    -- ** Request constructor
    , detachInternetGateway
    -- ** Request lenses
    , dig2DryRun
    , dig2InternetGatewayId
    , dig2VpcId

    -- * Response
    , DetachInternetGatewayResponse
    -- ** Response constructor
    , detachInternetGatewayResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DetachInternetGateway = DetachInternetGateway
    { _dig2DryRun            :: Maybe Bool
    , _dig2InternetGatewayId :: Text
    , _dig2VpcId             :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DetachInternetGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dig2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dig2InternetGatewayId' @::@ 'Text'
--
-- * 'dig2VpcId' @::@ 'Text'
--
detachInternetGateway :: Text -- ^ 'dig2InternetGatewayId'
                      -> Text -- ^ 'dig2VpcId'
                      -> DetachInternetGateway
detachInternetGateway p1 p2 = DetachInternetGateway
    { _dig2InternetGatewayId = p1
    , _dig2VpcId             = p2
    , _dig2DryRun            = Nothing
    }

dig2DryRun :: Lens' DetachInternetGateway (Maybe Bool)
dig2DryRun = lens _dig2DryRun (\s a -> s { _dig2DryRun = a })

-- | The ID of the Internet gateway.
dig2InternetGatewayId :: Lens' DetachInternetGateway Text
dig2InternetGatewayId =
    lens _dig2InternetGatewayId (\s a -> s { _dig2InternetGatewayId = a })

-- | The ID of the VPC.
dig2VpcId :: Lens' DetachInternetGateway Text
dig2VpcId = lens _dig2VpcId (\s a -> s { _dig2VpcId = a })
instance ToQuery DetachInternetGateway

instance ToPath DetachInternetGateway where
    toPath = const "/"

data DetachInternetGatewayResponse = DetachInternetGatewayResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DetachInternetGatewayResponse' constructor.
detachInternetGatewayResponse :: DetachInternetGatewayResponse
detachInternetGatewayResponse = DetachInternetGatewayResponse
instance FromXML DetachInternetGatewayResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DetachInternetGatewayResponse"

instance AWSRequest DetachInternetGateway where
    type Sv DetachInternetGateway = EC2
    type Rs DetachInternetGateway = DetachInternetGatewayResponse

    request  = post "DetachInternetGateway"
    response = nullaryResponse DetachInternetGatewayResponse
