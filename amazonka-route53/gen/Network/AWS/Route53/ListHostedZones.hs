{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListHostedZones
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a list of your hosted zones, send a @GET@ request to the
-- @2013-04-01\/hostedzone@ resource. The response to this request includes
-- a @HostedZones@ element with zero, one, or multiple @HostedZone@ child
-- elements. By default, the list of hosted zones is displayed on a single
-- page. You can control the length of the page that is displayed by using
-- the @MaxItems@ parameter. You can use the @Marker@ parameter to control
-- the hosted zone that the list begins with.
--
-- Amazon Route 53 returns a maximum of 100 items. If you set MaxItems to a
-- value greater than 100, Amazon Route 53 returns only the first 100.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHostedZones.html>
module Network.AWS.Route53.ListHostedZones
    (
    -- * Request
      ListHostedZones
    -- ** Request constructor
    , listHostedZones
    -- ** Request lenses
    , lhzDelegationSetId
    , lhzMaxItems
    , lhzMarker

    -- * Response
    , ListHostedZonesResponse
    -- ** Response constructor
    , listHostedZonesResponse
    -- ** Response lenses
    , lhzrsNextMarker
    , lhzrsStatus
    , lhzrsHostedZones
    , lhzrsMarker
    , lhzrsIsTruncated
    , lhzrsMaxItems
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | To retrieve a list of your hosted zones, send a @GET@ request to the
-- @2013-04-01\/hostedzone@ resource. The response to this request includes
-- a @HostedZones@ element with zero or more @HostedZone@ child elements.
-- By default, the list of hosted zones is displayed on a single page. You
-- can control the length of the page that is displayed by using the
-- @MaxItems@ parameter. You can use the @Marker@ parameter to control the
-- hosted zone that the list begins with. For more information about
-- listing hosted zones, see
-- <http://docs.amazonwebservices.com/Route53/latest/DeveloperGuide/ListInfoOnHostedZone.html Listing the Hosted Zones for an AWS Account>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Route 53 returns a maximum of 100 items. If you set @MaxItems@ to a
-- value greater than 100, Route 53 returns only the first 100.
--
-- /See:/ 'listHostedZones' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhzDelegationSetId'
--
-- * 'lhzMaxItems'
--
-- * 'lhzMarker'
data ListHostedZones = ListHostedZones'
    { _lhzDelegationSetId :: !(Maybe Text)
    , _lhzMaxItems        :: !(Maybe Text)
    , _lhzMarker          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListHostedZones' smart constructor.
listHostedZones :: ListHostedZones
listHostedZones =
    ListHostedZones'
    { _lhzDelegationSetId = Nothing
    , _lhzMaxItems = Nothing
    , _lhzMarker = Nothing
    }

-- | FIXME: Undocumented member.
lhzDelegationSetId :: Lens' ListHostedZones (Maybe Text)
lhzDelegationSetId = lens _lhzDelegationSetId (\ s a -> s{_lhzDelegationSetId = a});

-- | Specify the maximum number of hosted zones to return per page of
-- results.
lhzMaxItems :: Lens' ListHostedZones (Maybe Text)
lhzMaxItems = lens _lhzMaxItems (\ s a -> s{_lhzMaxItems = a});

-- | If the request returned more than one page of results, submit another
-- request and specify the value of @NextMarker@ from the last response in
-- the @marker@ parameter to get the next page of results.
lhzMarker :: Lens' ListHostedZones (Maybe Text)
lhzMarker = lens _lhzMarker (\ s a -> s{_lhzMarker = a});

instance AWSPager ListHostedZones where
        page rq rs
          | stop (rs ^. lhzrsIsTruncated) = Nothing
          | isNothing (rs ^. lhzrsNextMarker) = Nothing
          | otherwise =
            Just $ rq & lhzMarker .~ rs ^. lhzrsNextMarker

instance AWSRequest ListHostedZones where
        type Sv ListHostedZones = Route53
        type Rs ListHostedZones = ListHostedZonesResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListHostedZonesResponse' <$>
                   (x .@? "NextMarker") <*> (pure (fromEnum s)) <*>
                     (x .@? "HostedZones" .!@ mempty >>=
                        parseXMLList "HostedZone")
                     <*> (x .@ "Marker")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "MaxItems"))

instance ToHeaders ListHostedZones where
        toHeaders = const mempty

instance ToPath ListHostedZones where
        toPath = const ["2013-04-01", "hostedzone"]

instance ToQuery ListHostedZones where
        toQuery ListHostedZones'{..}
          = mconcat
              ["delegationsetid" =: _lhzDelegationSetId,
               "maxitems" =: _lhzMaxItems, "marker" =: _lhzMarker]

-- | A complex type that contains the response for the request.
--
-- /See:/ 'listHostedZonesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhzrsNextMarker'
--
-- * 'lhzrsStatus'
--
-- * 'lhzrsHostedZones'
--
-- * 'lhzrsMarker'
--
-- * 'lhzrsIsTruncated'
--
-- * 'lhzrsMaxItems'
data ListHostedZonesResponse = ListHostedZonesResponse'
    { _lhzrsNextMarker  :: !(Maybe Text)
    , _lhzrsStatus      :: !Int
    , _lhzrsHostedZones :: ![HostedZone]
    , _lhzrsMarker      :: !Text
    , _lhzrsIsTruncated :: !Bool
    , _lhzrsMaxItems    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListHostedZonesResponse' smart constructor.
listHostedZonesResponse :: Int -> Text -> Bool -> Text -> ListHostedZonesResponse
listHostedZonesResponse pStatus_ pMarker_ pIsTruncated_ pMaxItems_ =
    ListHostedZonesResponse'
    { _lhzrsNextMarker = Nothing
    , _lhzrsStatus = pStatus_
    , _lhzrsHostedZones = mempty
    , _lhzrsMarker = pMarker_
    , _lhzrsIsTruncated = pIsTruncated_
    , _lhzrsMaxItems = pMaxItems_
    }

-- | Indicates where to continue listing hosted zones. If
-- ListHostedZonesResponse$IsTruncated is @true@, make another request to
-- @ListHostedZones@ and include the value of the @NextMarker@ element in
-- the @Marker@ element to get the next page of results.
lhzrsNextMarker :: Lens' ListHostedZonesResponse (Maybe Text)
lhzrsNextMarker = lens _lhzrsNextMarker (\ s a -> s{_lhzrsNextMarker = a});

-- | FIXME: Undocumented member.
lhzrsStatus :: Lens' ListHostedZonesResponse Int
lhzrsStatus = lens _lhzrsStatus (\ s a -> s{_lhzrsStatus = a});

-- | A complex type that contains information about the hosted zones
-- associated with the current AWS account.
lhzrsHostedZones :: Lens' ListHostedZonesResponse [HostedZone]
lhzrsHostedZones = lens _lhzrsHostedZones (\ s a -> s{_lhzrsHostedZones = a}) . _Coerce;

-- | If the request returned more than one page of results, submit another
-- request and specify the value of @NextMarker@ from the last response in
-- the @marker@ parameter to get the next page of results.
lhzrsMarker :: Lens' ListHostedZonesResponse Text
lhzrsMarker = lens _lhzrsMarker (\ s a -> s{_lhzrsMarker = a});

-- | A flag indicating whether there are more hosted zones to be listed. If
-- your results were truncated, you can make a follow-up request for the
-- next page of results by using the @Marker@ element.
--
-- Valid Values: @true@ | @false@
lhzrsIsTruncated :: Lens' ListHostedZonesResponse Bool
lhzrsIsTruncated = lens _lhzrsIsTruncated (\ s a -> s{_lhzrsIsTruncated = a});

-- | The maximum number of hosted zones to be included in the response body.
-- If the number of hosted zones associated with this AWS account exceeds
-- @MaxItems@, the value of ListHostedZonesResponse$IsTruncated in the
-- response is @true@. Call @ListHostedZones@ again and specify the value
-- of ListHostedZonesResponse$NextMarker in the
-- ListHostedZonesRequest$Marker element to get the next page of results.
lhzrsMaxItems :: Lens' ListHostedZonesResponse Text
lhzrsMaxItems = lens _lhzrsMaxItems (\ s a -> s{_lhzrsMaxItems = a});
