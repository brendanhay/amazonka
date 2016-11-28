{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHostedZone
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the delegation set for a hosted zone, including the four name servers assigned to the hosted zone. Send a @GET@ request to the @//Amazon Route 53 API version/ /hostedzone//hosted zone ID/ @ resource.
--
--
module Network.AWS.Route53.GetHostedZone
    (
    -- * Creating a Request
      getHostedZone
    , GetHostedZone
    -- * Request Lenses
    , ghzId

    -- * Destructuring the Response
    , getHostedZoneResponse
    , GetHostedZoneResponse
    -- * Response Lenses
    , ghzrsVPCs
    , ghzrsDelegationSet
    , ghzrsResponseStatus
    , ghzrsHostedZone
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | The input for a GetHostedZone request.
--
--
--
-- /See:/ 'getHostedZone' smart constructor.
newtype GetHostedZone = GetHostedZone'
    { _ghzId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetHostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghzId' - The ID of the hosted zone for which you want to get a list of the name servers in the delegation set.
getHostedZone
    :: Text -- ^ 'ghzId'
    -> GetHostedZone
getHostedZone pId_ =
    GetHostedZone'
    { _ghzId = pId_
    }

-- | The ID of the hosted zone for which you want to get a list of the name servers in the delegation set.
ghzId :: Lens' GetHostedZone Text
ghzId = lens _ghzId (\ s a -> s{_ghzId = a});

instance AWSRequest GetHostedZone where
        type Rs GetHostedZone = GetHostedZoneResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetHostedZoneResponse' <$>
                   (x .@? "VPCs" .!@ mempty >>=
                      may (parseXMLList1 "VPC"))
                     <*> (x .@? "DelegationSet")
                     <*> (pure (fromEnum s))
                     <*> (x .@ "HostedZone"))

instance Hashable GetHostedZone

instance NFData GetHostedZone

instance ToHeaders GetHostedZone where
        toHeaders = const mempty

instance ToPath GetHostedZone where
        toPath GetHostedZone'{..}
          = mconcat ["/2013-04-01/hostedzone/", toBS _ghzId]

instance ToQuery GetHostedZone where
        toQuery = const mempty

-- | A complex type containing the response information for the hosted zone.
--
--
--
-- /See:/ 'getHostedZoneResponse' smart constructor.
data GetHostedZoneResponse = GetHostedZoneResponse'
    { _ghzrsVPCs           :: !(Maybe (List1 VPC))
    , _ghzrsDelegationSet  :: !(Maybe DelegationSet)
    , _ghzrsResponseStatus :: !Int
    , _ghzrsHostedZone     :: !HostedZone
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetHostedZoneResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghzrsVPCs' - A complex type that contains information about VPCs associated with the specified hosted zone.
--
-- * 'ghzrsDelegationSet' - A complex type that describes the name servers for this hosted zone.
--
-- * 'ghzrsResponseStatus' - -- | The response status code.
--
-- * 'ghzrsHostedZone' - A complex type that contains general information about the hosted zone.
getHostedZoneResponse
    :: Int -- ^ 'ghzrsResponseStatus'
    -> HostedZone -- ^ 'ghzrsHostedZone'
    -> GetHostedZoneResponse
getHostedZoneResponse pResponseStatus_ pHostedZone_ =
    GetHostedZoneResponse'
    { _ghzrsVPCs = Nothing
    , _ghzrsDelegationSet = Nothing
    , _ghzrsResponseStatus = pResponseStatus_
    , _ghzrsHostedZone = pHostedZone_
    }

-- | A complex type that contains information about VPCs associated with the specified hosted zone.
ghzrsVPCs :: Lens' GetHostedZoneResponse (Maybe (NonEmpty VPC))
ghzrsVPCs = lens _ghzrsVPCs (\ s a -> s{_ghzrsVPCs = a}) . mapping _List1;

-- | A complex type that describes the name servers for this hosted zone.
ghzrsDelegationSet :: Lens' GetHostedZoneResponse (Maybe DelegationSet)
ghzrsDelegationSet = lens _ghzrsDelegationSet (\ s a -> s{_ghzrsDelegationSet = a});

-- | -- | The response status code.
ghzrsResponseStatus :: Lens' GetHostedZoneResponse Int
ghzrsResponseStatus = lens _ghzrsResponseStatus (\ s a -> s{_ghzrsResponseStatus = a});

-- | A complex type that contains general information about the hosted zone.
ghzrsHostedZone :: Lens' GetHostedZoneResponse HostedZone
ghzrsHostedZone = lens _ghzrsHostedZone (\ s a -> s{_ghzrsHostedZone = a});

instance NFData GetHostedZoneResponse
