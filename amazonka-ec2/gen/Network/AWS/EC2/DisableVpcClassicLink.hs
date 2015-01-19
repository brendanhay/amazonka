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

-- Module      : Network.AWS.EC2.DisableVpcClassicLink
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

-- | Disables ClassicLink for a VPC. You cannot disable ClassicLink for a VPC that
-- has EC2-Classic instances linked to it.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisableVpcClassicLink.html>
module Network.AWS.EC2.DisableVpcClassicLink
    (
    -- * Request
      DisableVpcClassicLink
    -- ** Request constructor
    , disableVpcClassicLink
    -- ** Request lenses
    , dvcl1DryRun
    , dvcl1VpcId

    -- * Response
    , DisableVpcClassicLinkResponse
    -- ** Response constructor
    , disableVpcClassicLinkResponse
    -- ** Response lenses
    , dvclrReturn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DisableVpcClassicLink = DisableVpcClassicLink
    { _dvcl1DryRun :: Maybe Bool
    , _dvcl1VpcId  :: Text
    } deriving (Eq, Ord, Show)

-- | 'DisableVpcClassicLink' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvcl1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvcl1VpcId' @::@ 'Text'
--
disableVpcClassicLink :: Text -- ^ 'dvcl1VpcId'
                      -> DisableVpcClassicLink
disableVpcClassicLink p1 = DisableVpcClassicLink
    { _dvcl1VpcId  = p1
    , _dvcl1DryRun = Nothing
    }

dvcl1DryRun :: Lens' DisableVpcClassicLink (Maybe Bool)
dvcl1DryRun = lens _dvcl1DryRun (\s a -> s { _dvcl1DryRun = a })

-- | The ID of the VPC.
dvcl1VpcId :: Lens' DisableVpcClassicLink Text
dvcl1VpcId = lens _dvcl1VpcId (\s a -> s { _dvcl1VpcId = a })

newtype DisableVpcClassicLinkResponse = DisableVpcClassicLinkResponse
    { _dvclrReturn :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'DisableVpcClassicLinkResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvclrReturn' @::@ 'Maybe' 'Bool'
--
disableVpcClassicLinkResponse :: DisableVpcClassicLinkResponse
disableVpcClassicLinkResponse = DisableVpcClassicLinkResponse
    { _dvclrReturn = Nothing
    }

-- | Returns 'true' if the request succeeds; otherwise, it returns an error.
dvclrReturn :: Lens' DisableVpcClassicLinkResponse (Maybe Bool)
dvclrReturn = lens _dvclrReturn (\s a -> s { _dvclrReturn = a })

instance ToPath DisableVpcClassicLink where
    toPath = const "/"

instance ToQuery DisableVpcClassicLink where
    toQuery DisableVpcClassicLink{..} = mconcat
        [ "dryRun" =? _dvcl1DryRun
        , "vpcId"  =? _dvcl1VpcId
        ]

instance ToHeaders DisableVpcClassicLink

instance AWSRequest DisableVpcClassicLink where
    type Sv DisableVpcClassicLink = EC2
    type Rs DisableVpcClassicLink = DisableVpcClassicLinkResponse

    request  = post "DisableVpcClassicLink"
    response = xmlResponse

instance FromXML DisableVpcClassicLinkResponse where
    parseXML x = DisableVpcClassicLinkResponse
        <$> x .@? "return"
