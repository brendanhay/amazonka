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
-- Module      : Network.AWS.Route53.ListHostedZones
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a list of your hosted zones, send a 'GET' request to the
-- '2013-04-01\/hostedzone' resource. The response to this request includes
-- a 'HostedZones' element with zero, one, or multiple 'HostedZone' child
-- elements. By default, the list of hosted zones is displayed on a single
-- page. You can control the length of the page that is displayed by using
-- the 'MaxItems' parameter. You can use the 'Marker' parameter to control
-- the hosted zone that the list begins with.
--
-- Amazon Route 53 returns a maximum of 100 items. If you set MaxItems to a
-- value greater than 100, Amazon Route 53 returns only the first 100.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHostedZones.html AWS API Reference> for ListHostedZones.
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListHostedZones
    (
    -- * Creating a Request
      listHostedZones
    , ListHostedZones
    -- * Request Lenses
    , lhzDelegationSetId
    , lhzMarker
    , lhzMaxItems

    -- * Destructuring the Response
    , listHostedZonesResponse
    , ListHostedZonesResponse
    -- * Response Lenses
    , lhzrsMarker
    , lhzrsNextMarker
    , lhzrsResponseStatus
    , lhzrsHostedZones
    , lhzrsIsTruncated
    , lhzrsMaxItems
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | To retrieve a list of your hosted zones, send a 'GET' request to the
-- '2013-04-01\/hostedzone' resource. The response to this request includes
-- a 'HostedZones' element with zero or more 'HostedZone' child elements.
-- By default, the list of hosted zones is displayed on a single page. You
-- can control the length of the page that is displayed by using the
-- 'MaxItems' parameter. You can use the 'Marker' parameter to control the
-- hosted zone that the list begins with. For more information about
-- listing hosted zones, see
-- <http://docs.amazonwebservices.com/Route53/latest/DeveloperGuide/ListInfoOnHostedZone.html Listing the Hosted Zones for an AWS Account>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Route 53 returns a maximum of 100 items. If you set 'MaxItems' to a
-- value greater than 100, Route 53 returns only the first 100.
--
-- /See:/ 'listHostedZones' smart constructor.
data ListHostedZones = ListHostedZones'
    { _lhzDelegationSetId :: !(Maybe Text)
    , _lhzMarker          :: !(Maybe Text)
    , _lhzMaxItems        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListHostedZones' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhzDelegationSetId'
--
-- * 'lhzMarker'
--
-- * 'lhzMaxItems'
listHostedZones
    :: ListHostedZones
listHostedZones =
    ListHostedZones'
    { _lhzDelegationSetId = Nothing
    , _lhzMarker = Nothing
    , _lhzMaxItems = Nothing
    }

-- | Undocumented member.
lhzDelegationSetId :: Lens' ListHostedZones (Maybe Text)
lhzDelegationSetId = lens _lhzDelegationSetId (\ s a -> s{_lhzDelegationSetId = a});

-- | If the request returned more than one page of results, submit another
-- request and specify the value of 'NextMarker' from the last response in
-- the 'marker' parameter to get the next page of results.
lhzMarker :: Lens' ListHostedZones (Maybe Text)
lhzMarker = lens _lhzMarker (\ s a -> s{_lhzMarker = a});

-- | Specify the maximum number of hosted zones to return per page of
-- results.
lhzMaxItems :: Lens' ListHostedZones (Maybe Text)
lhzMaxItems = lens _lhzMaxItems (\ s a -> s{_lhzMaxItems = a});

instance AWSPager ListHostedZones where
        page rq rs
          | stop (rs ^. lhzrsNextMarker) = Nothing
          | stop (rs ^. lhzrsHostedZones) = Nothing
          | otherwise =
            Just $ rq & lhzMarker .~ rs ^. lhzrsNextMarker

instance AWSRequest ListHostedZones where
        type Rs ListHostedZones = ListHostedZonesResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListHostedZonesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "NextMarker") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "HostedZones" .!@ mempty >>=
                        parseXMLList "HostedZone")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "MaxItems"))

instance ToHeaders ListHostedZones where
        toHeaders = const mempty

instance ToPath ListHostedZones where
        toPath = const "/2013-04-01/hostedzone"

instance ToQuery ListHostedZones where
        toQuery ListHostedZones'{..}
          = mconcat
              ["delegationsetid" =: _lhzDelegationSetId,
               "marker" =: _lhzMarker, "maxitems" =: _lhzMaxItems]

-- | A complex type that contains the response for the request.
--
-- /See:/ 'listHostedZonesResponse' smart constructor.
data ListHostedZonesResponse = ListHostedZonesResponse'
    { _lhzrsMarker         :: !(Maybe Text)
    , _lhzrsNextMarker     :: !(Maybe Text)
    , _lhzrsResponseStatus :: !Int
    , _lhzrsHostedZones    :: ![HostedZone]
    , _lhzrsIsTruncated    :: !Bool
    , _lhzrsMaxItems       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListHostedZonesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhzrsMarker'
--
-- * 'lhzrsNextMarker'
--
-- * 'lhzrsResponseStatus'
--
-- * 'lhzrsHostedZones'
--
-- * 'lhzrsIsTruncated'
--
-- * 'lhzrsMaxItems'
listHostedZonesResponse
    :: Int -- ^ 'lhzrsResponseStatus'
    -> Bool -- ^ 'lhzrsIsTruncated'
    -> Text -- ^ 'lhzrsMaxItems'
    -> ListHostedZonesResponse
listHostedZonesResponse pResponseStatus_ pIsTruncated_ pMaxItems_ =
    ListHostedZonesResponse'
    { _lhzrsMarker = Nothing
    , _lhzrsNextMarker = Nothing
    , _lhzrsResponseStatus = pResponseStatus_
    , _lhzrsHostedZones = mempty
    , _lhzrsIsTruncated = pIsTruncated_
    , _lhzrsMaxItems = pMaxItems_
    }

-- | If the request returned more than one page of results, submit another
-- request and specify the value of 'NextMarker' from the last response in
-- the 'marker' parameter to get the next page of results.
lhzrsMarker :: Lens' ListHostedZonesResponse (Maybe Text)
lhzrsMarker = lens _lhzrsMarker (\ s a -> s{_lhzrsMarker = a});

-- | Indicates where to continue listing hosted zones. If
-- ListHostedZonesResponse$IsTruncated is 'true', make another request to
-- 'ListHostedZones' and include the value of the 'NextMarker' element in
-- the 'Marker' element to get the next page of results.
lhzrsNextMarker :: Lens' ListHostedZonesResponse (Maybe Text)
lhzrsNextMarker = lens _lhzrsNextMarker (\ s a -> s{_lhzrsNextMarker = a});

-- | The response status code.
lhzrsResponseStatus :: Lens' ListHostedZonesResponse Int
lhzrsResponseStatus = lens _lhzrsResponseStatus (\ s a -> s{_lhzrsResponseStatus = a});

-- | A complex type that contains information about the hosted zones
-- associated with the current AWS account.
lhzrsHostedZones :: Lens' ListHostedZonesResponse [HostedZone]
lhzrsHostedZones = lens _lhzrsHostedZones (\ s a -> s{_lhzrsHostedZones = a}) . _Coerce;

-- | A flag indicating whether there are more hosted zones to be listed. If
-- your results were truncated, you can make a follow-up request for the
-- next page of results by using the 'Marker' element.
--
-- Valid Values: 'true' | 'false'
lhzrsIsTruncated :: Lens' ListHostedZonesResponse Bool
lhzrsIsTruncated = lens _lhzrsIsTruncated (\ s a -> s{_lhzrsIsTruncated = a});

-- | The maximum number of hosted zones to be included in the response body.
-- If the number of hosted zones associated with this AWS account exceeds
-- 'MaxItems', the value of ListHostedZonesResponse$IsTruncated in the
-- response is 'true'. Call 'ListHostedZones' again and specify the value
-- of ListHostedZonesResponse$NextMarker in the
-- ListHostedZonesRequest$Marker element to get the next page of results.
lhzrsMaxItems :: Lens' ListHostedZonesResponse Text
lhzrsMaxItems = lens _lhzrsMaxItems (\ s a -> s{_lhzrsMaxItems = a});
