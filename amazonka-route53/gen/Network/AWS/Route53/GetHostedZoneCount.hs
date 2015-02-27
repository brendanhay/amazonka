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

-- Module      : Network.AWS.Route53.GetHostedZoneCount
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

-- | To retrieve a count of all your hosted zones, send a 'GET' request to the '2013-04-01/hostedzonecount' resource.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHostedZoneCount.html>
module Network.AWS.Route53.GetHostedZoneCount
    (
    -- * Request
      GetHostedZoneCount
    -- ** Request constructor
    , getHostedZoneCount

    -- * Response
    , GetHostedZoneCountResponse
    -- ** Response constructor
    , getHostedZoneCountResponse
    -- ** Response lenses
    , ghzcrHostedZoneCount
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data GetHostedZoneCount = GetHostedZoneCount
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'GetHostedZoneCount' constructor.
getHostedZoneCount :: GetHostedZoneCount
getHostedZoneCount = GetHostedZoneCount

newtype GetHostedZoneCountResponse = GetHostedZoneCountResponse
    { _ghzcrHostedZoneCount :: Integer
    } deriving (Eq, Ord, Read, Show, Enum, Num, Integral, Real)

-- | 'GetHostedZoneCountResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghzcrHostedZoneCount' @::@ 'Integer'
--
getHostedZoneCountResponse :: Integer -- ^ 'ghzcrHostedZoneCount'
                           -> GetHostedZoneCountResponse
getHostedZoneCountResponse p1 = GetHostedZoneCountResponse
    { _ghzcrHostedZoneCount = p1
    }

-- | The number of hosted zones associated with the current AWS account.
ghzcrHostedZoneCount :: Lens' GetHostedZoneCountResponse Integer
ghzcrHostedZoneCount =
    lens _ghzcrHostedZoneCount (\s a -> s { _ghzcrHostedZoneCount = a })

instance ToPath GetHostedZoneCount where
    toPath = const "/2013-04-01/hostedzonecount"

instance ToQuery GetHostedZoneCount where
    toQuery = const mempty

instance ToHeaders GetHostedZoneCount

instance ToXMLRoot GetHostedZoneCount where
    toXMLRoot = const (namespaced ns "GetHostedZoneCount" [])

instance ToXML GetHostedZoneCount

instance AWSRequest GetHostedZoneCount where
    type Sv GetHostedZoneCount = Route53
    type Rs GetHostedZoneCount = GetHostedZoneCountResponse

    request  = get
    response = xmlResponse

instance FromXML GetHostedZoneCountResponse where
    parseXML x = GetHostedZoneCountResponse
        <$> x .@  "HostedZoneCount"
