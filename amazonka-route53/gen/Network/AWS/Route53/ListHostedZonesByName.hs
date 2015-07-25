{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListHostedZonesByName
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a list of your hosted zones in lexicographic order, send a
-- @GET@ request to the @2013-04-01\/hostedzonesbyname@ resource. The
-- response to this request includes a @HostedZones@ element with zero or
-- more @HostedZone@ child elements lexicographically ordered by DNS name.
-- By default, the list of hosted zones is displayed on a single page. You
-- can control the length of the page that is displayed by using the
-- @MaxItems@ parameter. You can use the @DNSName@ and @HostedZoneId@
-- parameters to control the hosted zone that the list begins with.
--
-- Amazon Route 53 returns a maximum of 100 items. If you set MaxItems to a
-- value greater than 100, Amazon Route 53 returns only the first 100.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHostedZonesByName.html>
module Network.AWS.Route53.ListHostedZonesByName
    (
    -- * Request
      ListHostedZonesByName
    -- ** Request constructor
    , listHostedZonesByName
    -- ** Request lenses
    , lhzbnHostedZoneId
    , lhzbnMaxItems
    , lhzbnDNSName

    -- * Response
    , ListHostedZonesByNameResponse
    -- ** Response constructor
    , listHostedZonesByNameResponse
    -- ** Response lenses
    , lhzbnrsHostedZoneId
    , lhzbnrsNextHostedZoneId
    , lhzbnrsDNSName
    , lhzbnrsNextDNSName
    , lhzbnrsStatus
    , lhzbnrsHostedZones
    , lhzbnrsIsTruncated
    , lhzbnrsMaxItems
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | To retrieve a list of your hosted zones in lexicographic order, send a
-- @GET@ request to the @2013-04-01\/hostedzonesbyname@ resource. The
-- response to this request includes a @HostedZones@ element with zero or
-- more @HostedZone@ child elements lexicographically ordered by DNS name.
-- By default, the list of hosted zones is displayed on a single page. You
-- can control the length of the page that is displayed by using the
-- @MaxItems@ parameter. You can use the @DNSName@ and @HostedZoneId@
-- parameters to control the hosted zone that the list begins with.
--
-- For more information about listing hosted zones, see
-- <http://docs.amazonwebservices.com/Route53/latest/DeveloperGuide/ListInfoOnHostedZone.html Listing the Hosted Zones for an AWS Account>
-- in the /Amazon Route 53 Developer Guide/.
--
-- /See:/ 'listHostedZonesByName' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhzbnHostedZoneId'
--
-- * 'lhzbnMaxItems'
--
-- * 'lhzbnDNSName'
data ListHostedZonesByName = ListHostedZonesByName'
    { _lhzbnHostedZoneId :: !(Maybe Text)
    , _lhzbnMaxItems     :: !(Maybe Text)
    , _lhzbnDNSName      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListHostedZonesByName' smart constructor.
listHostedZonesByName :: ListHostedZonesByName
listHostedZonesByName =
    ListHostedZonesByName'
    { _lhzbnHostedZoneId = Nothing
    , _lhzbnMaxItems = Nothing
    , _lhzbnDNSName = Nothing
    }

-- | If the request returned more than one page of results, submit another
-- request and specify the value of @NextDNSName@ and @NextHostedZoneId@
-- from the last response in the @DNSName@ and @HostedZoneId@ parameters to
-- get the next page of results.
lhzbnHostedZoneId :: Lens' ListHostedZonesByName (Maybe Text)
lhzbnHostedZoneId = lens _lhzbnHostedZoneId (\ s a -> s{_lhzbnHostedZoneId = a});

-- | Specify the maximum number of hosted zones to return per page of
-- results.
lhzbnMaxItems :: Lens' ListHostedZonesByName (Maybe Text)
lhzbnMaxItems = lens _lhzbnMaxItems (\ s a -> s{_lhzbnMaxItems = a});

-- | The first name in the lexicographic ordering of domain names that you
-- want the @ListHostedZonesByNameRequest@ request to list.
--
-- If the request returned more than one page of results, submit another
-- request and specify the value of @NextDNSName@ and @NextHostedZoneId@
-- from the last response in the @DNSName@ and @HostedZoneId@ parameters to
-- get the next page of results.
lhzbnDNSName :: Lens' ListHostedZonesByName (Maybe Text)
lhzbnDNSName = lens _lhzbnDNSName (\ s a -> s{_lhzbnDNSName = a});

instance AWSRequest ListHostedZonesByName where
        type Sv ListHostedZonesByName = Route53
        type Rs ListHostedZonesByName =
             ListHostedZonesByNameResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListHostedZonesByNameResponse' <$>
                   (x .@? "HostedZoneId") <*> (x .@? "NextHostedZoneId")
                     <*> (x .@? "DNSName")
                     <*> (x .@? "NextDNSName")
                     <*> (pure (fromEnum s))
                     <*>
                     (x .@? "HostedZones" .!@ mempty >>=
                        parseXMLList "HostedZone")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "MaxItems"))

instance ToHeaders ListHostedZonesByName where
        toHeaders = const mempty

instance ToPath ListHostedZonesByName where
        toPath = const "/2013-04-01/hostedzonesbyname"

instance ToQuery ListHostedZonesByName where
        toQuery ListHostedZonesByName'{..}
          = mconcat
              ["hostedzoneid" =: _lhzbnHostedZoneId,
               "maxitems" =: _lhzbnMaxItems,
               "dnsname" =: _lhzbnDNSName]

-- | A complex type that contains the response for the request.
--
-- /See:/ 'listHostedZonesByNameResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhzbnrsHostedZoneId'
--
-- * 'lhzbnrsNextHostedZoneId'
--
-- * 'lhzbnrsDNSName'
--
-- * 'lhzbnrsNextDNSName'
--
-- * 'lhzbnrsStatus'
--
-- * 'lhzbnrsHostedZones'
--
-- * 'lhzbnrsIsTruncated'
--
-- * 'lhzbnrsMaxItems'
data ListHostedZonesByNameResponse = ListHostedZonesByNameResponse'
    { _lhzbnrsHostedZoneId     :: !(Maybe Text)
    , _lhzbnrsNextHostedZoneId :: !(Maybe Text)
    , _lhzbnrsDNSName          :: !(Maybe Text)
    , _lhzbnrsNextDNSName      :: !(Maybe Text)
    , _lhzbnrsStatus           :: !Int
    , _lhzbnrsHostedZones      :: ![HostedZone]
    , _lhzbnrsIsTruncated      :: !Bool
    , _lhzbnrsMaxItems         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListHostedZonesByNameResponse' smart constructor.
listHostedZonesByNameResponse :: Int -> Bool -> Text -> ListHostedZonesByNameResponse
listHostedZonesByNameResponse pStatus_ pIsTruncated_ pMaxItems_ =
    ListHostedZonesByNameResponse'
    { _lhzbnrsHostedZoneId = Nothing
    , _lhzbnrsNextHostedZoneId = Nothing
    , _lhzbnrsDNSName = Nothing
    , _lhzbnrsNextDNSName = Nothing
    , _lhzbnrsStatus = pStatus_
    , _lhzbnrsHostedZones = mempty
    , _lhzbnrsIsTruncated = pIsTruncated_
    , _lhzbnrsMaxItems = pMaxItems_
    }

-- | The @HostedZoneId@ value sent in the request.
lhzbnrsHostedZoneId :: Lens' ListHostedZonesByNameResponse (Maybe Text)
lhzbnrsHostedZoneId = lens _lhzbnrsHostedZoneId (\ s a -> s{_lhzbnrsHostedZoneId = a});

-- | If ListHostedZonesByNameResponse$IsTruncated is @true@, there are more
-- hosted zones associated with the current AWS account. To get the next
-- page of results, make another request to @ListHostedZonesByName@.
-- Specify the value of ListHostedZonesByNameResponse$NextDNSName in the
-- ListHostedZonesByNameRequest$DNSName element and
-- ListHostedZonesByNameResponse$NextHostedZoneId in the
-- ListHostedZonesByNameRequest$HostedZoneId element.
lhzbnrsNextHostedZoneId :: Lens' ListHostedZonesByNameResponse (Maybe Text)
lhzbnrsNextHostedZoneId = lens _lhzbnrsNextHostedZoneId (\ s a -> s{_lhzbnrsNextHostedZoneId = a});

-- | The @DNSName@ value sent in the request.
lhzbnrsDNSName :: Lens' ListHostedZonesByNameResponse (Maybe Text)
lhzbnrsDNSName = lens _lhzbnrsDNSName (\ s a -> s{_lhzbnrsDNSName = a});

-- | If ListHostedZonesByNameResponse$IsTruncated is @true@, there are more
-- hosted zones associated with the current AWS account. To get the next
-- page of results, make another request to @ListHostedZonesByName@.
-- Specify the value of ListHostedZonesByNameResponse$NextDNSName in the
-- ListHostedZonesByNameRequest$DNSName element and
-- ListHostedZonesByNameResponse$NextHostedZoneId in the
-- ListHostedZonesByNameRequest$HostedZoneId element.
lhzbnrsNextDNSName :: Lens' ListHostedZonesByNameResponse (Maybe Text)
lhzbnrsNextDNSName = lens _lhzbnrsNextDNSName (\ s a -> s{_lhzbnrsNextDNSName = a});

-- | FIXME: Undocumented member.
lhzbnrsStatus :: Lens' ListHostedZonesByNameResponse Int
lhzbnrsStatus = lens _lhzbnrsStatus (\ s a -> s{_lhzbnrsStatus = a});

-- | A complex type that contains information about the hosted zones
-- associated with the current AWS account.
lhzbnrsHostedZones :: Lens' ListHostedZonesByNameResponse [HostedZone]
lhzbnrsHostedZones = lens _lhzbnrsHostedZones (\ s a -> s{_lhzbnrsHostedZones = a}) . _Coerce;

-- | A flag indicating whether there are more hosted zones to be listed. If
-- your results were truncated, you can make a follow-up request for the
-- next page of results by using the @NextDNSName@ and @NextHostedZoneId@
-- elements.
--
-- Valid Values: @true@ | @false@
lhzbnrsIsTruncated :: Lens' ListHostedZonesByNameResponse Bool
lhzbnrsIsTruncated = lens _lhzbnrsIsTruncated (\ s a -> s{_lhzbnrsIsTruncated = a});

-- | The maximum number of hosted zones to be included in the response body.
-- If the number of hosted zones associated with this AWS account exceeds
-- @MaxItems@, the value of ListHostedZonesByNameResponse$IsTruncated in
-- the response is @true@. Call @ListHostedZonesByName@ again and specify
-- the value of ListHostedZonesByNameResponse$NextDNSName and
-- ListHostedZonesByNameResponse$NextHostedZoneId elements respectively to
-- get the next page of results.
lhzbnrsMaxItems :: Lens' ListHostedZonesByNameResponse Text
lhzbnrsMaxItems = lens _lhzbnrsMaxItems (\ s a -> s{_lhzbnrsMaxItems = a});
