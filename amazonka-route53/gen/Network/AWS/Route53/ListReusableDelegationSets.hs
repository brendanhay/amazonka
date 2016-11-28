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
-- Module      : Network.AWS.Route53.ListReusableDelegationSets
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a list of your reusable delegation sets, send a @GET@ request to the @/2013-04-01/delegationset@ resource. The response to this request includes a @DelegationSets@ element with zero, one, or multiple @DelegationSet@ child elements. By default, the list of delegation sets is displayed on a single page. You can control the length of the page that is displayed by using the @MaxItems@ parameter. You can use the @Marker@ parameter to control the delegation set that the list begins with.
--
--
module Network.AWS.Route53.ListReusableDelegationSets
    (
    -- * Creating a Request
      listReusableDelegationSets
    , ListReusableDelegationSets
    -- * Request Lenses
    , lrdsMarker
    , lrdsMaxItems

    -- * Destructuring the Response
    , listReusableDelegationSetsResponse
    , ListReusableDelegationSetsResponse
    -- * Response Lenses
    , lrdsrsNextMarker
    , lrdsrsResponseStatus
    , lrdsrsDelegationSets
    , lrdsrsMarker
    , lrdsrsIsTruncated
    , lrdsrsMaxItems
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | To retrieve a list of your reusable delegation sets, send a @GET@ request to the @/2013-04-01/delegationset@ resource. The response to this request includes a @DelegationSets@ element with zero or more @DelegationSet@ child elements. By default, the list of reusable delegation sets is displayed on a single page. You can control the length of the page that is displayed by using the @MaxItems@ parameter. You can use the @Marker@ parameter to control the delegation set that the list begins with.
--
--
--
-- /See:/ 'listReusableDelegationSets' smart constructor.
data ListReusableDelegationSets = ListReusableDelegationSets'
    { _lrdsMarker   :: !(Maybe Text)
    , _lrdsMaxItems :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListReusableDelegationSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdsMarker' - If you're making the second or subsequent call to @ListReusableDelegationSets@ , the @Marker@ element matches the value that you specified in the @marker@ parameter in the previous request.
--
-- * 'lrdsMaxItems' - The value that you specified for the @maxitems@ parameter in the request that produced the current response.
listReusableDelegationSets
    :: ListReusableDelegationSets
listReusableDelegationSets =
    ListReusableDelegationSets'
    { _lrdsMarker = Nothing
    , _lrdsMaxItems = Nothing
    }

-- | If you're making the second or subsequent call to @ListReusableDelegationSets@ , the @Marker@ element matches the value that you specified in the @marker@ parameter in the previous request.
lrdsMarker :: Lens' ListReusableDelegationSets (Maybe Text)
lrdsMarker = lens _lrdsMarker (\ s a -> s{_lrdsMarker = a});

-- | The value that you specified for the @maxitems@ parameter in the request that produced the current response.
lrdsMaxItems :: Lens' ListReusableDelegationSets (Maybe Text)
lrdsMaxItems = lens _lrdsMaxItems (\ s a -> s{_lrdsMaxItems = a});

instance AWSRequest ListReusableDelegationSets where
        type Rs ListReusableDelegationSets =
             ListReusableDelegationSetsResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListReusableDelegationSetsResponse' <$>
                   (x .@? "NextMarker") <*> (pure (fromEnum s)) <*>
                     (x .@? "DelegationSets" .!@ mempty >>=
                        parseXMLList "DelegationSet")
                     <*> (x .@ "Marker")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "MaxItems"))

instance Hashable ListReusableDelegationSets

instance NFData ListReusableDelegationSets

instance ToHeaders ListReusableDelegationSets where
        toHeaders = const mempty

instance ToPath ListReusableDelegationSets where
        toPath = const "/2013-04-01/delegationset"

instance ToQuery ListReusableDelegationSets where
        toQuery ListReusableDelegationSets'{..}
          = mconcat
              ["marker" =: _lrdsMarker,
               "maxitems" =: _lrdsMaxItems]

-- | A complex type that contains information about the reusable delegation sets that are associated with the current AWS account.
--
--
--
-- /See:/ 'listReusableDelegationSetsResponse' smart constructor.
data ListReusableDelegationSetsResponse = ListReusableDelegationSetsResponse'
    { _lrdsrsNextMarker     :: !(Maybe Text)
    , _lrdsrsResponseStatus :: !Int
    , _lrdsrsDelegationSets :: ![DelegationSet]
    , _lrdsrsMarker         :: !Text
    , _lrdsrsIsTruncated    :: !Bool
    , _lrdsrsMaxItems       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListReusableDelegationSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdsrsNextMarker' - If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first reusable delegation set in the next group of @maxitems@ reusable delegation sets. Call @ListReusableDelegationSets@ again and specify the value of @NextMarker@ in the @marker@ parameter.
--
-- * 'lrdsrsResponseStatus' - -- | The response status code.
--
-- * 'lrdsrsDelegationSets' - A complex type that contains one @DelegationSet@ element for each reusable delegation set that was created by the current AWS account.
--
-- * 'lrdsrsMarker' - For the second and subsequent calls to @ListReusableDelegationSets@ , @Marker@ is the value that you specified for the marker parameter in the request that produced the current response.
--
-- * 'lrdsrsIsTruncated' - A flag that indicates whether there are more reusable delegation sets to be listed. If the response is truncated, you can get the next group of @maxitems@ reusable delegation sets by calling @ListReusableDelegationSets@ again and specifying the value of the @NextMarker@ element in the @marker@ parameter.
--
-- * 'lrdsrsMaxItems' - The value that you specified for the @maxitems@ parameter in the call to @ListReusableDelegationSets@ that produced the current response.
listReusableDelegationSetsResponse
    :: Int -- ^ 'lrdsrsResponseStatus'
    -> Text -- ^ 'lrdsrsMarker'
    -> Bool -- ^ 'lrdsrsIsTruncated'
    -> Text -- ^ 'lrdsrsMaxItems'
    -> ListReusableDelegationSetsResponse
listReusableDelegationSetsResponse pResponseStatus_ pMarker_ pIsTruncated_ pMaxItems_ =
    ListReusableDelegationSetsResponse'
    { _lrdsrsNextMarker = Nothing
    , _lrdsrsResponseStatus = pResponseStatus_
    , _lrdsrsDelegationSets = mempty
    , _lrdsrsMarker = pMarker_
    , _lrdsrsIsTruncated = pIsTruncated_
    , _lrdsrsMaxItems = pMaxItems_
    }

-- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first reusable delegation set in the next group of @maxitems@ reusable delegation sets. Call @ListReusableDelegationSets@ again and specify the value of @NextMarker@ in the @marker@ parameter.
lrdsrsNextMarker :: Lens' ListReusableDelegationSetsResponse (Maybe Text)
lrdsrsNextMarker = lens _lrdsrsNextMarker (\ s a -> s{_lrdsrsNextMarker = a});

-- | -- | The response status code.
lrdsrsResponseStatus :: Lens' ListReusableDelegationSetsResponse Int
lrdsrsResponseStatus = lens _lrdsrsResponseStatus (\ s a -> s{_lrdsrsResponseStatus = a});

-- | A complex type that contains one @DelegationSet@ element for each reusable delegation set that was created by the current AWS account.
lrdsrsDelegationSets :: Lens' ListReusableDelegationSetsResponse [DelegationSet]
lrdsrsDelegationSets = lens _lrdsrsDelegationSets (\ s a -> s{_lrdsrsDelegationSets = a}) . _Coerce;

-- | For the second and subsequent calls to @ListReusableDelegationSets@ , @Marker@ is the value that you specified for the marker parameter in the request that produced the current response.
lrdsrsMarker :: Lens' ListReusableDelegationSetsResponse Text
lrdsrsMarker = lens _lrdsrsMarker (\ s a -> s{_lrdsrsMarker = a});

-- | A flag that indicates whether there are more reusable delegation sets to be listed. If the response is truncated, you can get the next group of @maxitems@ reusable delegation sets by calling @ListReusableDelegationSets@ again and specifying the value of the @NextMarker@ element in the @marker@ parameter.
lrdsrsIsTruncated :: Lens' ListReusableDelegationSetsResponse Bool
lrdsrsIsTruncated = lens _lrdsrsIsTruncated (\ s a -> s{_lrdsrsIsTruncated = a});

-- | The value that you specified for the @maxitems@ parameter in the call to @ListReusableDelegationSets@ that produced the current response.
lrdsrsMaxItems :: Lens' ListReusableDelegationSetsResponse Text
lrdsrsMaxItems = lens _lrdsrsMaxItems (\ s a -> s{_lrdsrsMaxItems = a});

instance NFData ListReusableDelegationSetsResponse
