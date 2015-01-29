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

-- Module      : Network.AWS.Route53.ChangeResourceRecordSets
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Use this action to create or change your authoritative DNS information. To
-- use this action, send a 'POST' request to the '2013-04-01/hostedzone//hosted ZoneID//rrset resource. The request body must include an XML document with a 'ChangeResourceRecordSetsRequest' element.
--
-- Changes are a list of change items and are considered transactional. For
-- more information on transactional changes, also known as change batches, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/RRSchanges.html#RRSchanges_API Creating, Changing, and Deleting Resource Record Sets Using the Route 53 API> in the /Amazon Route 53 Developer Guide/.
--
-- Due to the nature of transactional changes, you cannot delete the same
-- resource record set more than once in a single change batch. If you attempt
-- to delete the same change batch more than once, Route 53 returns an 'InvalidChangeBatch' error. In response to a 'ChangeResourceRecordSets' request, your DNS data is
-- changed on all Route 53 DNS servers. Initially, the status of a change is 'PENDING'. This means the change has not yet propagated to all the authoritative Route
-- 53 DNS servers. When the change is propagated to all hosts, the change
-- returns a status of 'INSYNC'.
--
-- Note the following limitations on a 'ChangeResourceRecordSets' request:
--
-- - A request cannot contain more than 100 Change elements.
--
-- - A request cannot contain more than 1000 ResourceRecord elements.
--
-- The sum of the number of characters (including spaces) in all 'Value' elements
-- in a request cannot exceed 32,000 characters.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html>
module Network.AWS.Route53.ChangeResourceRecordSets
    (
    -- * Request
      ChangeResourceRecordSets
    -- ** Request constructor
    , changeResourceRecordSets
    -- ** Request lenses
    , crrsChangeBatch
    , crrsHostedZoneId

    -- * Response
    , ChangeResourceRecordSetsResponse
    -- ** Response constructor
    , changeResourceRecordSetsResponse
    -- ** Response lenses
    , crrsrChangeInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data ChangeResourceRecordSets = ChangeResourceRecordSets
    { _crrsChangeBatch  :: ChangeBatch
    , _crrsHostedZoneId :: Text
    } deriving (Eq, Read, Show)

-- | 'ChangeResourceRecordSets' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrsChangeBatch' @::@ 'ChangeBatch'
--
-- * 'crrsHostedZoneId' @::@ 'Text'
--
changeResourceRecordSets :: Text -- ^ 'crrsHostedZoneId'
                         -> ChangeBatch -- ^ 'crrsChangeBatch'
                         -> ChangeResourceRecordSets
changeResourceRecordSets p1 p2 = ChangeResourceRecordSets
    { _crrsHostedZoneId = p1
    , _crrsChangeBatch  = p2
    }

-- | A complex type that contains an optional comment and the 'Changes' element.
crrsChangeBatch :: Lens' ChangeResourceRecordSets ChangeBatch
crrsChangeBatch = lens _crrsChangeBatch (\s a -> s { _crrsChangeBatch = a })

-- | The ID of the hosted zone that contains the resource record sets that you
-- want to change.
crrsHostedZoneId :: Lens' ChangeResourceRecordSets Text
crrsHostedZoneId = lens _crrsHostedZoneId (\s a -> s { _crrsHostedZoneId = a })

newtype ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse
    { _crrsrChangeInfo :: ChangeInfo
    } deriving (Eq, Read, Show)

-- | 'ChangeResourceRecordSetsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrsrChangeInfo' @::@ 'ChangeInfo'
--
changeResourceRecordSetsResponse :: ChangeInfo -- ^ 'crrsrChangeInfo'
                                 -> ChangeResourceRecordSetsResponse
changeResourceRecordSetsResponse p1 = ChangeResourceRecordSetsResponse
    { _crrsrChangeInfo = p1
    }

-- | A complex type that contains information about changes made to your hosted
-- zone.
--
-- This element contains an ID that you use when performing a 'GetChange' action
-- to get detailed information about the change.
crrsrChangeInfo :: Lens' ChangeResourceRecordSetsResponse ChangeInfo
crrsrChangeInfo = lens _crrsrChangeInfo (\s a -> s { _crrsrChangeInfo = a })

instance ToPath ChangeResourceRecordSets where
    toPath ChangeResourceRecordSets{..} = mconcat
        [ "/2013-04-01/hostedzone/"
        , toText _crrsHostedZoneId
        , "/rrset/"
        ]

instance ToQuery ChangeResourceRecordSets where
    toQuery = const mempty

instance ToHeaders ChangeResourceRecordSets

instance ToXMLRoot ChangeResourceRecordSets where
    toXMLRoot ChangeResourceRecordSets{..} = namespaced ns "ChangeResourceRecordSets"
        [ "ChangeBatch" =@ _crrsChangeBatch
        ]

instance ToXML ChangeResourceRecordSets

instance AWSRequest ChangeResourceRecordSets where
    type Sv ChangeResourceRecordSets = Route53
    type Rs ChangeResourceRecordSets = ChangeResourceRecordSetsResponse

    request  = post
    response = xmlResponse

instance FromXML ChangeResourceRecordSetsResponse where
    parseXML x = ChangeResourceRecordSetsResponse
        <$> x .@  "ChangeInfo"
