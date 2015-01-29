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

-- Module      : Network.AWS.EC2.EnableVpcClassicLink
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

-- | Enables a VPC for ClassicLink. You can then link EC2-Classic instances to
-- your ClassicLink-enabled VPC to allow communication over private IP
-- addresses. You cannot enable your VPC for ClassicLink if any of your VPC's
-- route tables have existing routes for address ranges within the '10.0.0.0/8' IP
-- address range, excluding local routes for VPCs in the '10.0.0.0/16' and '10.1.0.0/16' IP address ranges. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the Amazon
-- Elastic Compute Cloud User Guide for Linux.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVpcClassicLink.html>
module Network.AWS.EC2.EnableVpcClassicLink
    (
    -- * Request
      EnableVpcClassicLink
    -- ** Request constructor
    , enableVpcClassicLink
    -- ** Request lenses
    , evclDryRun
    , evclVpcId

    -- * Response
    , EnableVpcClassicLinkResponse
    -- ** Response constructor
    , enableVpcClassicLinkResponse
    -- ** Response lenses
    , evclrReturn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data EnableVpcClassicLink = EnableVpcClassicLink
    { _evclDryRun :: Maybe Bool
    , _evclVpcId  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'EnableVpcClassicLink' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'evclDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'evclVpcId' @::@ 'Text'
--
enableVpcClassicLink :: Text -- ^ 'evclVpcId'
                     -> EnableVpcClassicLink
enableVpcClassicLink p1 = EnableVpcClassicLink
    { _evclVpcId  = p1
    , _evclDryRun = Nothing
    }

evclDryRun :: Lens' EnableVpcClassicLink (Maybe Bool)
evclDryRun = lens _evclDryRun (\s a -> s { _evclDryRun = a })

-- | The ID of the VPC.
evclVpcId :: Lens' EnableVpcClassicLink Text
evclVpcId = lens _evclVpcId (\s a -> s { _evclVpcId = a })

newtype EnableVpcClassicLinkResponse = EnableVpcClassicLinkResponse
    { _evclrReturn :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'EnableVpcClassicLinkResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'evclrReturn' @::@ 'Maybe' 'Bool'
--
enableVpcClassicLinkResponse :: EnableVpcClassicLinkResponse
enableVpcClassicLinkResponse = EnableVpcClassicLinkResponse
    { _evclrReturn = Nothing
    }

-- | Returns 'true' if the request succeeds; otherwise, it returns an error.
evclrReturn :: Lens' EnableVpcClassicLinkResponse (Maybe Bool)
evclrReturn = lens _evclrReturn (\s a -> s { _evclrReturn = a })

instance ToPath EnableVpcClassicLink where
    toPath = const "/"

instance ToQuery EnableVpcClassicLink where
    toQuery EnableVpcClassicLink{..} = mconcat
        [ "DryRun" =? _evclDryRun
        , "VpcId"  =? _evclVpcId
        ]

instance ToHeaders EnableVpcClassicLink

instance AWSRequest EnableVpcClassicLink where
    type Sv EnableVpcClassicLink = EC2
    type Rs EnableVpcClassicLink = EnableVpcClassicLinkResponse

    request  = post "EnableVpcClassicLink"
    response = xmlResponse

instance FromXML EnableVpcClassicLinkResponse where
    parseXML x = EnableVpcClassicLinkResponse
        <$> x .@? "return"
