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

-- Module      : Network.AWS.Route53.GetHostedZone
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
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHostedZone.html>
module Network.AWS.Route53.GetHostedZone
    (
    -- * Request
      GetHostedZone
    -- ** Request constructor
    , getHostedZone
    -- ** Request lenses
    , ghzId

    -- * Response
    , GetHostedZoneResponse
    -- ** Response constructor
    , getHostedZoneResponse
    -- ** Response lenses
    , ghzrDelegationSet
    , ghzrHostedZone
    , ghzrVPCs
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

newtype GetHostedZone = GetHostedZone
    { _ghzId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetHostedZone' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghzId' @::@ 'Text'
--
getHostedZone :: Text -- ^ 'ghzId'
              -> GetHostedZone
getHostedZone p1 = GetHostedZone
    { _ghzId = p1
    }

-- | The ID of the hosted zone for which you want to get a list of the name
-- servers in the delegation set.
ghzId :: Lens' GetHostedZone Text
ghzId = lens _ghzId (\s a -> s { _ghzId = a })

data GetHostedZoneResponse = GetHostedZoneResponse
    { _ghzrDelegationSet :: Maybe DelegationSet
    , _ghzrHostedZone    :: HostedZone
    , _ghzrVPCs          :: List1 "VPC" VPC
    } deriving (Eq, Show)

-- | 'GetHostedZoneResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghzrDelegationSet' @::@ 'Maybe' 'DelegationSet'
--
-- * 'ghzrHostedZone' @::@ 'HostedZone'
--
-- * 'ghzrVPCs' @::@ 'NonEmpty' 'VPC'
--
getHostedZoneResponse :: HostedZone -- ^ 'ghzrHostedZone'
                      -> NonEmpty VPC -- ^ 'ghzrVPCs'
                      -> GetHostedZoneResponse
getHostedZoneResponse p1 p2 = GetHostedZoneResponse
    { _ghzrHostedZone    = p1
    , _ghzrVPCs          = withIso _List1 (const id) p2
    , _ghzrDelegationSet = Nothing
    }

-- | A complex type that contains information about the name servers for the
-- specified hosted zone.
ghzrDelegationSet :: Lens' GetHostedZoneResponse (Maybe DelegationSet)
ghzrDelegationSet =
    lens _ghzrDelegationSet (\s a -> s { _ghzrDelegationSet = a })

-- | A complex type that contains the information about the specified hosted
-- zone.
ghzrHostedZone :: Lens' GetHostedZoneResponse HostedZone
ghzrHostedZone = lens _ghzrHostedZone (\s a -> s { _ghzrHostedZone = a })

-- | A complex type that contains information about VPCs associated with the
-- specified hosted zone.
ghzrVPCs :: Lens' GetHostedZoneResponse (NonEmpty VPC)
ghzrVPCs = lens _ghzrVPCs (\s a -> s { _ghzrVPCs = a }) . _List1

instance ToPath GetHostedZone where
    toPath GetHostedZone{..} = mconcat
        [ "/2013-04-01/hostedzone/"
        , toText _ghzId
        ]

instance ToQuery GetHostedZone where
    toQuery = const mempty

instance ToHeaders GetHostedZone

instance ToXMLRoot GetHostedZone where
    toXMLRoot = const (element "GetHostedZone" [])

instance ToXML GetHostedZone

instance AWSRequest GetHostedZone where
    type Sv GetHostedZone = Route53
    type Rs GetHostedZone = GetHostedZoneResponse

    request  = get
    response = xmlResponse

instance FromXML GetHostedZoneResponse where
    parseXML x = GetHostedZoneResponse
        <$> x .@? "DelegationSet"
        <*> x .@  "HostedZone"
        <*> x .@  "VPCs"
