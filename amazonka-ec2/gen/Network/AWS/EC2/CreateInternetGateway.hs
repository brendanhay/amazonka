{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates an Internet gateway for use with a VPC. After creating the Internet
-- gateway, you attach it to a VPC using 'AttachInternetGateway'.
--
-- For more information about your VPC and Internet gateway, see the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/ AmazonVirtual Private Cloud User Guide>.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateInternetGateway.html>
module Network.AWS.EC2.CreateInternetGateway
    (
    -- * Request
      CreateInternetGateway
    -- ** Request constructor
    , createInternetGateway
    -- ** Request lenses
    , cigDryRun

    -- * Response
    , CreateInternetGatewayResponse
    -- ** Response constructor
    , createInternetGatewayResponse
    -- ** Response lenses
    , cigrInternetGateway
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

newtype CreateInternetGateway = CreateInternetGateway
    { _cigDryRun :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

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

newtype CreateInternetGatewayResponse = CreateInternetGatewayResponse
    { _cigrInternetGateway :: Maybe InternetGateway
    } deriving (Eq, Read, Show)

-- | 'CreateInternetGatewayResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cigrInternetGateway' @::@ 'Maybe' 'InternetGateway'
--
createInternetGatewayResponse :: CreateInternetGatewayResponse
createInternetGatewayResponse = CreateInternetGatewayResponse
    { _cigrInternetGateway = Nothing
    }

-- | Information about the Internet gateway.
cigrInternetGateway :: Lens' CreateInternetGatewayResponse (Maybe InternetGateway)
cigrInternetGateway =
    lens _cigrInternetGateway (\s a -> s { _cigrInternetGateway = a })

instance ToPath CreateInternetGateway where
    toPath = const "/"

instance ToQuery CreateInternetGateway where
    toQuery CreateInternetGateway{..} = mconcat
        [ "dryRun" =? _cigDryRun
        ]

instance ToHeaders CreateInternetGateway

instance AWSRequest CreateInternetGateway where
    type Sv CreateInternetGateway = EC2
    type Rs CreateInternetGateway = CreateInternetGatewayResponse

    request  = post "CreateInternetGateway"
    response = xmlResponse

instance FromXML CreateInternetGatewayResponse where
    parseXML x = CreateInternetGatewayResponse
        <$> x .@? "internetGateway"
