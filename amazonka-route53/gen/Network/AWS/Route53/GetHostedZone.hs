{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.GetHostedZone
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

-- | To retrieve the delegation set for a hosted zone, send a @GET@ request
-- to the @2013-04-01\/hostedzone\/hosted zone ID@ resource. The delegation
-- set is the four Route 53 name servers that were assigned to the hosted
-- zone when you created it.
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
    , ghzrVPCs
    , ghzrDelegationSet
    , ghzrStatus
    , ghzrHostedZone
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | The input for a GetHostedZone request.
--
-- /See:/ 'getHostedZone' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghzId'
newtype GetHostedZone = GetHostedZone'
    { _ghzId :: Text
    } deriving (Eq,Read,Show)

-- | 'GetHostedZone' smart constructor.
getHostedZone :: Text -> GetHostedZone
getHostedZone pId =
    GetHostedZone'
    { _ghzId = pId
    }

-- | The ID of the hosted zone for which you want to get a list of the name
-- servers in the delegation set.
ghzId :: Lens' GetHostedZone Text
ghzId = lens _ghzId (\ s a -> s{_ghzId = a});

instance AWSRequest GetHostedZone where
        type Sv GetHostedZone = Route53
        type Rs GetHostedZone = GetHostedZoneResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetHostedZoneResponse' <$>
                   (x .@? "VPCs" .!@ mempty >>=
                      may (parseXMLList1 "VPC"))
                     <*> (x .@? "DelegationSet")
                     <*> (pure s)
                     <*> (x .@ "HostedZone"))

instance ToHeaders GetHostedZone where
        toHeaders = const mempty

instance ToPath GetHostedZone where
        toPath GetHostedZone'{..}
          = mconcat ["/2013-04-01/hostedzone/", toText _ghzId]

instance ToQuery GetHostedZone where
        toQuery = const mempty

-- | A complex type containing information about the specified hosted zone.
--
-- /See:/ 'getHostedZoneResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghzrVPCs'
--
-- * 'ghzrDelegationSet'
--
-- * 'ghzrStatus'
--
-- * 'ghzrHostedZone'
data GetHostedZoneResponse = GetHostedZoneResponse'
    { _ghzrVPCs          :: !(Maybe (List1 VPC))
    , _ghzrDelegationSet :: !(Maybe DelegationSet)
    , _ghzrStatus        :: !Status
    , _ghzrHostedZone    :: !HostedZone
    } deriving (Eq,Show)

-- | 'GetHostedZoneResponse' smart constructor.
getHostedZoneResponse :: Status -> HostedZone -> GetHostedZoneResponse
getHostedZoneResponse pStatus pHostedZone =
    GetHostedZoneResponse'
    { _ghzrVPCs = Nothing
    , _ghzrDelegationSet = Nothing
    , _ghzrStatus = pStatus
    , _ghzrHostedZone = pHostedZone
    }

-- | A complex type that contains information about VPCs associated with the
-- specified hosted zone.
ghzrVPCs :: Lens' GetHostedZoneResponse (Maybe (NonEmpty VPC))
ghzrVPCs = lens _ghzrVPCs (\ s a -> s{_ghzrVPCs = a}) . mapping _List1;

-- | A complex type that contains information about the name servers for the
-- specified hosted zone.
ghzrDelegationSet :: Lens' GetHostedZoneResponse (Maybe DelegationSet)
ghzrDelegationSet = lens _ghzrDelegationSet (\ s a -> s{_ghzrDelegationSet = a});

-- | FIXME: Undocumented member.
ghzrStatus :: Lens' GetHostedZoneResponse Status
ghzrStatus = lens _ghzrStatus (\ s a -> s{_ghzrStatus = a});

-- | A complex type that contains the information about the specified hosted
-- zone.
ghzrHostedZone :: Lens' GetHostedZoneResponse HostedZone
ghzrHostedZone = lens _ghzrHostedZone (\ s a -> s{_ghzrHostedZone = a});
