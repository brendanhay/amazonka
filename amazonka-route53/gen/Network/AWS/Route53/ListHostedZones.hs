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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of the public and private hosted zones that are associated with the current AWS account. The response includes a @HostedZones@ child element for each hosted zone.
--
--
-- Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of hosted zones, you can use the @maxitems@ parameter to list them in groups of up to 100.
--
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

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request to retrieve a list of the public and private hosted zones that are associated with the current AWS account.
--
--
--
-- /See:/ 'listHostedZones' smart constructor.
data ListHostedZones = ListHostedZones'
  { _lhzDelegationSetId :: !(Maybe ResourceId)
  , _lhzMarker          :: !(Maybe Text)
  , _lhzMaxItems        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHostedZones' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhzDelegationSetId' - If you're using reusable delegation sets and you want to list all of the hosted zones that are associated with a reusable delegation set, specify the ID of that reusable delegation set.
--
-- * 'lhzMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more hosted zones. To get more hosted zones, submit another @ListHostedZones@ request.  For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first hosted zone that Amazon Route 53 will return if you submit another request. If the value of @IsTruncated@ in the previous response was @false@ , there are no more hosted zones to get.
--
-- * 'lhzMaxItems' - (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If you have more than @maxitems@ hosted zones, the value of @IsTruncated@ in the response is @true@ , and the value of @NextMarker@ is the hosted zone ID of the first hosted zone that Amazon Route 53 will return if you submit another request.
listHostedZones
    :: ListHostedZones
listHostedZones =
  ListHostedZones'
    { _lhzDelegationSetId = Nothing
    , _lhzMarker = Nothing
    , _lhzMaxItems = Nothing
    }


-- | If you're using reusable delegation sets and you want to list all of the hosted zones that are associated with a reusable delegation set, specify the ID of that reusable delegation set.
lhzDelegationSetId :: Lens' ListHostedZones (Maybe ResourceId)
lhzDelegationSetId = lens _lhzDelegationSetId (\ s a -> s{_lhzDelegationSetId = a})

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more hosted zones. To get more hosted zones, submit another @ListHostedZones@ request.  For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first hosted zone that Amazon Route 53 will return if you submit another request. If the value of @IsTruncated@ in the previous response was @false@ , there are no more hosted zones to get.
lhzMarker :: Lens' ListHostedZones (Maybe Text)
lhzMarker = lens _lhzMarker (\ s a -> s{_lhzMarker = a})

-- | (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If you have more than @maxitems@ hosted zones, the value of @IsTruncated@ in the response is @true@ , and the value of @NextMarker@ is the hosted zone ID of the first hosted zone that Amazon Route 53 will return if you submit another request.
lhzMaxItems :: Lens' ListHostedZones (Maybe Text)
lhzMaxItems = lens _lhzMaxItems (\ s a -> s{_lhzMaxItems = a})

instance AWSPager ListHostedZones where
        page rq rs
          | stop (rs ^. lhzrsIsTruncated) = Nothing
          | isNothing (rs ^. lhzrsNextMarker) = Nothing
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

instance Hashable ListHostedZones where

instance NFData ListHostedZones where

instance ToHeaders ListHostedZones where
        toHeaders = const mempty

instance ToPath ListHostedZones where
        toPath = const "/2013-04-01/hostedzone"

instance ToQuery ListHostedZones where
        toQuery ListHostedZones'{..}
          = mconcat
              ["delegationsetid" =: _lhzDelegationSetId,
               "marker" =: _lhzMarker, "maxitems" =: _lhzMaxItems]

-- | /See:/ 'listHostedZonesResponse' smart constructor.
data ListHostedZonesResponse = ListHostedZonesResponse'
  { _lhzrsMarker         :: !(Maybe Text)
  , _lhzrsNextMarker     :: !(Maybe Text)
  , _lhzrsResponseStatus :: !Int
  , _lhzrsHostedZones    :: ![HostedZone]
  , _lhzrsIsTruncated    :: !Bool
  , _lhzrsMaxItems       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHostedZonesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhzrsMarker' - For the second and subsequent calls to @ListHostedZones@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
--
-- * 'lhzrsNextMarker' - If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first hosted zone in the next group of hosted zones. Submit another @ListHostedZones@ request, and specify the value of @NextMarker@ from the response in the @marker@ parameter. This element is present only if @IsTruncated@ is @true@ .
--
-- * 'lhzrsResponseStatus' - -- | The response status code.
--
-- * 'lhzrsHostedZones' - A complex type that contains general information about the hosted zone.
--
-- * 'lhzrsIsTruncated' - A flag indicating whether there are more hosted zones to be listed. If the response was truncated, you can get more hosted zones by submitting another @ListHostedZones@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
--
-- * 'lhzrsMaxItems' - The value that you specified for the @maxitems@ parameter in the call to @ListHostedZones@ that produced the current response.
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


-- | For the second and subsequent calls to @ListHostedZones@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
lhzrsMarker :: Lens' ListHostedZonesResponse (Maybe Text)
lhzrsMarker = lens _lhzrsMarker (\ s a -> s{_lhzrsMarker = a})

-- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first hosted zone in the next group of hosted zones. Submit another @ListHostedZones@ request, and specify the value of @NextMarker@ from the response in the @marker@ parameter. This element is present only if @IsTruncated@ is @true@ .
lhzrsNextMarker :: Lens' ListHostedZonesResponse (Maybe Text)
lhzrsNextMarker = lens _lhzrsNextMarker (\ s a -> s{_lhzrsNextMarker = a})

-- | -- | The response status code.
lhzrsResponseStatus :: Lens' ListHostedZonesResponse Int
lhzrsResponseStatus = lens _lhzrsResponseStatus (\ s a -> s{_lhzrsResponseStatus = a})

-- | A complex type that contains general information about the hosted zone.
lhzrsHostedZones :: Lens' ListHostedZonesResponse [HostedZone]
lhzrsHostedZones = lens _lhzrsHostedZones (\ s a -> s{_lhzrsHostedZones = a}) . _Coerce

-- | A flag indicating whether there are more hosted zones to be listed. If the response was truncated, you can get more hosted zones by submitting another @ListHostedZones@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
lhzrsIsTruncated :: Lens' ListHostedZonesResponse Bool
lhzrsIsTruncated = lens _lhzrsIsTruncated (\ s a -> s{_lhzrsIsTruncated = a})

-- | The value that you specified for the @maxitems@ parameter in the call to @ListHostedZones@ that produced the current response.
lhzrsMaxItems :: Lens' ListHostedZonesResponse Text
lhzrsMaxItems = lens _lhzrsMaxItems (\ s a -> s{_lhzrsMaxItems = a})

instance NFData ListHostedZonesResponse where
