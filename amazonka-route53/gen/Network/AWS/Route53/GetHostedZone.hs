{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHostedZone
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve the delegation set for a hosted zone, send a @GET@ request
-- to the @2013-04-01\/hostedzone\/hosted zone ID@ resource. The delegation
-- set is the four Route 53 name servers that were assigned to the hosted
-- zone when you created it.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHostedZone.html AWS API Reference> for GetHostedZone.
module Network.AWS.Route53.GetHostedZone
    (
    -- * Creating a Request
      GetHostedZone
    , getHostedZone
    -- * Request Lenses
    , ghzId

    -- * Destructuring the Response
    , GetHostedZoneResponse
    , getHostedZoneResponse
    -- * Response Lenses
    , ghzrsVPCs
    , ghzrsDelegationSet
    , ghzrsStatus
    , ghzrsHostedZone
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetHostedZone' smart constructor.
getHostedZone :: Text -> GetHostedZone
getHostedZone pId_ =
    GetHostedZone'
    { _ghzId = pId_
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
                     <*> (pure (fromEnum s))
                     <*> (x .@ "HostedZone"))

instance ToHeaders GetHostedZone where
        toHeaders = const mempty

instance ToPath GetHostedZone where
        toPath GetHostedZone'{..}
          = mconcat ["/2013-04-01/hostedzone/", toBS _ghzId]

instance ToQuery GetHostedZone where
        toQuery = const mempty

-- | A complex type containing information about the specified hosted zone.
--
-- /See:/ 'getHostedZoneResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghzrsVPCs'
--
-- * 'ghzrsDelegationSet'
--
-- * 'ghzrsStatus'
--
-- * 'ghzrsHostedZone'
data GetHostedZoneResponse = GetHostedZoneResponse'
    { _ghzrsVPCs          :: !(Maybe (List1 VPC))
    , _ghzrsDelegationSet :: !(Maybe DelegationSet)
    , _ghzrsStatus        :: !Int
    , _ghzrsHostedZone    :: !HostedZone
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetHostedZoneResponse' smart constructor.
getHostedZoneResponse :: Int -> HostedZone -> GetHostedZoneResponse
getHostedZoneResponse pStatus_ pHostedZone_ =
    GetHostedZoneResponse'
    { _ghzrsVPCs = Nothing
    , _ghzrsDelegationSet = Nothing
    , _ghzrsStatus = pStatus_
    , _ghzrsHostedZone = pHostedZone_
    }

-- | A complex type that contains information about VPCs associated with the
-- specified hosted zone.
ghzrsVPCs :: Lens' GetHostedZoneResponse (Maybe (NonEmpty VPC))
ghzrsVPCs = lens _ghzrsVPCs (\ s a -> s{_ghzrsVPCs = a}) . mapping _List1;

-- | A complex type that contains information about the name servers for the
-- specified hosted zone.
ghzrsDelegationSet :: Lens' GetHostedZoneResponse (Maybe DelegationSet)
ghzrsDelegationSet = lens _ghzrsDelegationSet (\ s a -> s{_ghzrsDelegationSet = a});

-- | Undocumented member.
ghzrsStatus :: Lens' GetHostedZoneResponse Int
ghzrsStatus = lens _ghzrsStatus (\ s a -> s{_ghzrsStatus = a});

-- | A complex type that contains the information about the specified hosted
-- zone.
ghzrsHostedZone :: Lens' GetHostedZoneResponse HostedZone
ghzrsHostedZone = lens _ghzrsHostedZone (\ s a -> s{_ghzrsHostedZone = a});
