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

-- Module      : Network.AWS.EC2.RequestSpotFleet
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

-- | Creates a Spot fleet request.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet.html Spot Fleets> in the /Amazon Elastic Compute CloudUser Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-RequestSpotFleet.html>
module Network.AWS.EC2.RequestSpotFleet
    (
    -- * Request
      RequestSpotFleet
    -- ** Request constructor
    , requestSpotFleet
    -- ** Request lenses
    , rsfDryRun
    , rsfSpotFleetRequestConfig

    -- * Response
    , RequestSpotFleetResponse
    -- ** Response constructor
    , requestSpotFleetResponse
    -- ** Response lenses
    , rsfrSpotFleetRequestId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data RequestSpotFleet = RequestSpotFleet
    { _rsfDryRun                 :: Maybe Bool
    , _rsfSpotFleetRequestConfig :: SpotFleetRequestConfigData
    } deriving (Eq, Read, Show)

-- | 'RequestSpotFleet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsfDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'rsfSpotFleetRequestConfig' @::@ 'SpotFleetRequestConfigData'
--
requestSpotFleet :: SpotFleetRequestConfigData -- ^ 'rsfSpotFleetRequestConfig'
                 -> RequestSpotFleet
requestSpotFleet p1 = RequestSpotFleet
    { _rsfSpotFleetRequestConfig = p1
    , _rsfDryRun                 = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
rsfDryRun :: Lens' RequestSpotFleet (Maybe Bool)
rsfDryRun = lens _rsfDryRun (\s a -> s { _rsfDryRun = a })

-- | The configuration for the Spot fleet request.
rsfSpotFleetRequestConfig :: Lens' RequestSpotFleet SpotFleetRequestConfigData
rsfSpotFleetRequestConfig =
    lens _rsfSpotFleetRequestConfig
        (\s a -> s { _rsfSpotFleetRequestConfig = a })

newtype RequestSpotFleetResponse = RequestSpotFleetResponse
    { _rsfrSpotFleetRequestId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'RequestSpotFleetResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsfrSpotFleetRequestId' @::@ 'Text'
--
requestSpotFleetResponse :: Text -- ^ 'rsfrSpotFleetRequestId'
                         -> RequestSpotFleetResponse
requestSpotFleetResponse p1 = RequestSpotFleetResponse
    { _rsfrSpotFleetRequestId = p1
    }

-- | The ID of the Spot fleet request.
rsfrSpotFleetRequestId :: Lens' RequestSpotFleetResponse Text
rsfrSpotFleetRequestId =
    lens _rsfrSpotFleetRequestId (\s a -> s { _rsfrSpotFleetRequestId = a })

instance ToPath RequestSpotFleet where
    toPath = const "/"

instance ToQuery RequestSpotFleet where
    toQuery RequestSpotFleet{..} = mconcat
        [ "DryRun"                 =? _rsfDryRun
        , "SpotFleetRequestConfig" =? _rsfSpotFleetRequestConfig
        ]

instance ToHeaders RequestSpotFleet

instance AWSRequest RequestSpotFleet where
    type Sv RequestSpotFleet = EC2
    type Rs RequestSpotFleet = RequestSpotFleetResponse

    request  = post "RequestSpotFleet"
    response = xmlResponse

instance FromXML RequestSpotFleetResponse where
    parseXML x = RequestSpotFleetResponse
        <$> x .@  "spotFleetRequestId"
