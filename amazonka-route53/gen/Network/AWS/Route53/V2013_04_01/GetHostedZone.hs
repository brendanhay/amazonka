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
    , mkGetHostedZoneRequest
    -- ** Request lenses
    , ghzrId

    -- * Response
    , GetHostedZoneResponse
    -- ** Response lenses
    , ghzsHostedZone
    , ghzsDelegationSet
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHostedZone' request.
mkGetHostedZoneRequest :: Text -- ^ 'ghzrId'
                       -> GetHostedZone
mkGetHostedZoneRequest p1 = GetHostedZone
    { _ghzrId = p1
    }
{-# INLINE mkGetHostedZoneRequest #-}

newtype GetHostedZone = GetHostedZone
    { _ghzrId :: Text
      -- ^ The ID of the hosted zone for which you want to get a list of the
      -- name servers in the delegation set.
    } deriving (Show, Generic)

-- | The ID of the hosted zone for which you want to get a list of the name
-- servers in the delegation set.
ghzrId :: Lens' GetHostedZone (Text)
ghzrId = lens _ghzrId (\s a -> s { _ghzrId = a })
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
    { _ghzsHostedZone :: HostedZone
      -- ^ A complex type that contains the information about the specified
      -- hosted zone.
    , _ghzsDelegationSet :: DelegationSet
      -- ^ A complex type that contains information about the name servers
      -- for the specified hosted zone.
    } deriving (Show, Generic)

-- | A complex type that contains the information about the specified hosted
-- zone.
ghzsHostedZone :: Lens' GetHostedZoneResponse (HostedZone)
ghzsHostedZone = lens _ghzsHostedZone (\s a -> s { _ghzsHostedZone = a })
{-# INLINE ghzsHostedZone #-}

-- | A complex type that contains information about the name servers for the
-- specified hosted zone.
ghzsDelegationSet :: Lens' GetHostedZoneResponse (DelegationSet)
ghzsDelegationSet = lens _ghzsDelegationSet (\s a -> s { _ghzsDelegationSet = a })
{-# INLINE ghzsDelegationSet #-}

instance FromXML GetHostedZoneResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetHostedZone where
    type Sv GetHostedZone = Route53
    type Rs GetHostedZone = GetHostedZoneResponse

    request = get
    response _ = xmlResponse
