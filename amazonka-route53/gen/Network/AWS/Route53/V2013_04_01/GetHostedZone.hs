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
    , mkGetHostedZone
    -- ** Request lenses
    , ghzId

    -- * Response
    , GetHostedZoneResponse
    -- ** Response lenses
    , ghzrsHostedZone
    , ghzrsDelegationSet
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The input for a GetHostedZone request.
newtype GetHostedZone = GetHostedZone
    { _ghzId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetHostedZone' request.
mkGetHostedZone :: Text -- ^ 'ghzId'
                -> GetHostedZone
mkGetHostedZone p1 = GetHostedZone
    { _ghzId = p1
    }
{-# INLINE mkGetHostedZone #-}

-- | The ID of the hosted zone for which you want to get a list of the name
-- servers in the delegation set.
ghzId :: Lens' GetHostedZone Text
ghzId = lens _ghzId (\s a -> s { _ghzId = a })
{-# INLINE ghzId #-}

instance ToPath GetHostedZone where
    toPath GetHostedZone{..} = mconcat
        [ "/2013-04-01/hostedzone/"
        , toBS _ghzId
        ]

instance ToQuery GetHostedZone

instance ToHeaders GetHostedZone

instance ToXML GetHostedZone where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetHostedZoneRequest"

-- | A complex type containing information about the specified hosted zone.
data GetHostedZoneResponse = GetHostedZoneResponse
    { _ghzrsHostedZone :: HostedZone
    , _ghzrsDelegationSet :: DelegationSet
    } deriving (Show, Generic)

-- | A complex type that contains the information about the specified hosted
-- zone.
ghzrsHostedZone :: Lens' GetHostedZoneResponse HostedZone
ghzrsHostedZone = lens _ghzrsHostedZone (\s a -> s { _ghzrsHostedZone = a })
{-# INLINE ghzrsHostedZone #-}

-- | A complex type that contains information about the name servers for the
-- specified hosted zone.
ghzrsDelegationSet :: Lens' GetHostedZoneResponse DelegationSet
ghzrsDelegationSet =
    lens _ghzrsDelegationSet (\s a -> s { _ghzrsDelegationSet = a })
{-# INLINE ghzrsDelegationSet #-}

instance FromXML GetHostedZoneResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetHostedZone where
    type Sv GetHostedZone = Route53
    type Rs GetHostedZone = GetHostedZoneResponse

    request = get
    response _ = xmlResponse
