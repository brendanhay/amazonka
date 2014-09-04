{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.GetHostedZone
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve the delegation set for a hosted zone, send a GET request to the
-- 2013-04-01/hostedzone/hosted zone ID resource. The delegation set is the
-- four Route 53 name servers that were assigned to the hosted zone when you
-- created it.
module Network.AWS.Route53.V2013_04_01.GetHostedZone
    (
    -- * Request
      GetHostedZone
    -- ** Request constructor
    , getHostedZone
    -- ** Request lenses
    , ghzrId

    -- * Response
    , GetHostedZoneResponse
    -- ** Response lenses
    , ghzsDelegationSet
    , ghzsHostedZone
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetHostedZone' request.
getHostedZone :: Text -- ^ 'ghzrId'
              -> GetHostedZone
getHostedZone p1 = GetHostedZone
    { _ghzrId = p1
    }
{-# INLINE getHostedZone #-}

data GetHostedZone = GetHostedZone
    { _ghzrId :: Text
      -- ^ The ID of the hosted zone for which you want to get a list of the
      -- name servers in the delegation set.
    } deriving (Show, Generic)

-- | The ID of the hosted zone for which you want to get a list of the name
-- servers in the delegation set.
ghzrId :: Lens' GetHostedZone (Text)
ghzrId f x =
    f (_ghzrId x)
        <&> \y -> x { _ghzrId = y }
{-# INLINE ghzrId #-}

instance ToPath GetHostedZone where
    toPath GetHostedZone{..} = mconcat
        [ "/2013-04-01/hostedzone/"
        , toBS _ghzrId
        ]

instance ToQuery GetHostedZone

instance ToHeaders GetHostedZone

instance ToXML GetHostedZone where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetHostedZoneRequest"

data GetHostedZoneResponse = GetHostedZoneResponse
    { _ghzsDelegationSet :: DelegationSet
      -- ^ A complex type that contains information about the name servers
      -- for the specified hosted zone.
    , _ghzsHostedZone :: HostedZone
      -- ^ A complex type that contains the information about the specified
      -- hosted zone.
    } deriving (Show, Generic)

-- | A complex type that contains information about the name servers for the
-- specified hosted zone.
ghzsDelegationSet :: Lens' GetHostedZoneResponse (DelegationSet)
ghzsDelegationSet f x =
    f (_ghzsDelegationSet x)
        <&> \y -> x { _ghzsDelegationSet = y }
{-# INLINE ghzsDelegationSet #-}

-- | A complex type that contains the information about the specified hosted
-- zone.
ghzsHostedZone :: Lens' GetHostedZoneResponse (HostedZone)
ghzsHostedZone f x =
    f (_ghzsHostedZone x)
        <&> \y -> x { _ghzsHostedZone = y }
{-# INLINE ghzsHostedZone #-}

instance FromXML GetHostedZoneResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetHostedZone where
    type Sv GetHostedZone = Route53
    type Rs GetHostedZone = GetHostedZoneResponse

    request = get
    response _ = xmlResponse
