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

-- Module      : Network.AWS.EC2.CreateInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an Internet gateway for use with a VPC. After creating the Internet
-- gateway, you attach it to a VPC using AttachInternetGateway. For more
-- information about your VPC and Internet gateway, see the Amazon Virtual
-- Private Cloud User Guide.
module Network.AWS.EC2.CreateInternetGateway
    (
    -- * Request
      CreateInternetGateway
    -- ** Request constructor
    , createInternetGateway
    -- ** Request lenses
    , cigDryRun

    -- * Response
    , CreateInternetGatewayResult
    -- ** Response constructor
    , createInternetGatewayResult
    -- ** Response lenses
    , cigrInternetGateway
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

newtype CreateInternetGateway = CreateInternetGateway
    { _cigDryRun :: Maybe Bool
    } (Eq, Ord, Show, Generic)

-- | 'CreateInternetGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cigDryRun' @::@ 'Maybe' 'Bool'
--
createInternetGateway :: CreateInternetGateway
createInternetGateway = CreateInternetGateway
    { _cigDryRun = Nothing
    }

cigDryRun :: Lens' CreateInternetGateway (Maybe Bool)
cigDryRun = lens _cigDryRun (\s a -> s { _cigDryRun = a })
instance ToQuery CreateInternetGateway

instance ToPath CreateInternetGateway where
    toPath = const "/"

newtype CreateInternetGatewayResult = CreateInternetGatewayResult
    { _cigrInternetGateway :: Maybe InternetGateway
    } (Eq, Show, Generic)

-- | 'CreateInternetGatewayResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cigrInternetGateway' @::@ 'Maybe' 'InternetGateway'
--
createInternetGatewayResult :: CreateInternetGatewayResult
createInternetGatewayResult = CreateInternetGatewayResult
    { _cigrInternetGateway = Nothing
    }

-- | Information about the Internet gateway.
cigrInternetGateway :: Lens' CreateInternetGatewayResult (Maybe InternetGateway)
cigrInternetGateway =
    lens _cigrInternetGateway (\s a -> s { _cigrInternetGateway = a })

instance FromXML CreateInternetGatewayResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateInternetGatewayResult"

instance AWSRequest CreateInternetGateway where
    type Sv CreateInternetGateway = EC2
    type Rs CreateInternetGateway = CreateInternetGatewayResult

    request  = post "CreateInternetGateway"
    response = xmlResponse $ \h x -> CreateInternetGatewayResult
        <$> x %| "internetGateway"
