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
-- Module      : Network.AWS.Route53.ListHealthChecks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a list of your health checks, send a 'GET' request to the
-- '2013-04-01\/healthcheck' resource. The response to this request
-- includes a 'HealthChecks' element with zero, one, or multiple
-- 'HealthCheck' child elements. By default, the list of health checks is
-- displayed on a single page. You can control the length of the page that
-- is displayed by using the 'MaxItems' parameter. You can use the 'Marker'
-- parameter to control the health check that the list begins with.
--
-- Amazon Route 53 returns a maximum of 100 items. If you set MaxItems to a
-- value greater than 100, Amazon Route 53 returns only the first 100.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHealthChecks.html AWS API Reference> for ListHealthChecks.
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListHealthChecks
    (
    -- * Creating a Request
      listHealthChecks
    , ListHealthChecks
    -- * Request Lenses
    , lhcMarker
    , lhcMaxItems

    -- * Destructuring the Response
    , listHealthChecksResponse
    , ListHealthChecksResponse
    -- * Response Lenses
    , lhcrsNextMarker
    , lhcrsResponseStatus
    , lhcrsHealthChecks
    , lhcrsMarker
    , lhcrsIsTruncated
    , lhcrsMaxItems
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | To retrieve a list of your health checks, send a 'GET' request to the
-- '2013-04-01\/healthcheck' resource. The response to this request
-- includes a 'HealthChecks' element with zero or more 'HealthCheck' child
-- elements. By default, the list of health checks is displayed on a single
-- page. You can control the length of the page that is displayed by using
-- the 'MaxItems' parameter. You can use the 'Marker' parameter to control
-- the health check that the list begins with.
--
-- Route 53 returns a maximum of 100 items. If you set 'MaxItems' to a
-- value greater than 100, Route 53 returns only the first 100.
--
-- /See:/ 'listHealthChecks' smart constructor.
data ListHealthChecks = ListHealthChecks'
    { _lhcMarker   :: !(Maybe Text)
    , _lhcMaxItems :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListHealthChecks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhcMarker'
--
-- * 'lhcMaxItems'
listHealthChecks
    :: ListHealthChecks
listHealthChecks =
    ListHealthChecks'
    { _lhcMarker = Nothing
    , _lhcMaxItems = Nothing
    }

-- | If the request returned more than one page of results, submit another
-- request and specify the value of 'NextMarker' from the last response in
-- the 'marker' parameter to get the next page of results.
lhcMarker :: Lens' ListHealthChecks (Maybe Text)
lhcMarker = lens _lhcMarker (\ s a -> s{_lhcMarker = a});

-- | Specify the maximum number of health checks to return per page of
-- results.
lhcMaxItems :: Lens' ListHealthChecks (Maybe Text)
lhcMaxItems = lens _lhcMaxItems (\ s a -> s{_lhcMaxItems = a});

instance AWSPager ListHealthChecks where
        page rq rs
          | stop (rs ^. lhcrsNextMarker) = Nothing
          | stop (rs ^. lhcrsHealthChecks) = Nothing
          | otherwise =
            Just $ rq & lhcMarker .~ rs ^. lhcrsNextMarker

instance AWSRequest ListHealthChecks where
        type Rs ListHealthChecks = ListHealthChecksResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListHealthChecksResponse' <$>
                   (x .@? "NextMarker") <*> (pure (fromEnum s)) <*>
                     (x .@? "HealthChecks" .!@ mempty >>=
                        parseXMLList "HealthCheck")
                     <*> (x .@ "Marker")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "MaxItems"))

instance ToHeaders ListHealthChecks where
        toHeaders = const mempty

instance ToPath ListHealthChecks where
        toPath = const "/2013-04-01/healthcheck"

instance ToQuery ListHealthChecks where
        toQuery ListHealthChecks'{..}
          = mconcat
              ["marker" =: _lhcMarker, "maxitems" =: _lhcMaxItems]

-- | A complex type that contains the response for the request.
--
-- /See:/ 'listHealthChecksResponse' smart constructor.
data ListHealthChecksResponse = ListHealthChecksResponse'
    { _lhcrsNextMarker     :: !(Maybe Text)
    , _lhcrsResponseStatus :: !Int
    , _lhcrsHealthChecks   :: ![HealthCheck]
    , _lhcrsMarker         :: !Text
    , _lhcrsIsTruncated    :: !Bool
    , _lhcrsMaxItems       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListHealthChecksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhcrsNextMarker'
--
-- * 'lhcrsResponseStatus'
--
-- * 'lhcrsHealthChecks'
--
-- * 'lhcrsMarker'
--
-- * 'lhcrsIsTruncated'
--
-- * 'lhcrsMaxItems'
listHealthChecksResponse
    :: Int -- ^ 'lhcrsResponseStatus'
    -> Text -- ^ 'lhcrsMarker'
    -> Bool -- ^ 'lhcrsIsTruncated'
    -> Text -- ^ 'lhcrsMaxItems'
    -> ListHealthChecksResponse
listHealthChecksResponse pResponseStatus_ pMarker_ pIsTruncated_ pMaxItems_ =
    ListHealthChecksResponse'
    { _lhcrsNextMarker = Nothing
    , _lhcrsResponseStatus = pResponseStatus_
    , _lhcrsHealthChecks = mempty
    , _lhcrsMarker = pMarker_
    , _lhcrsIsTruncated = pIsTruncated_
    , _lhcrsMaxItems = pMaxItems_
    }

-- | Indicates where to continue listing health checks. If
-- ListHealthChecksResponse$IsTruncated is 'true', make another request to
-- 'ListHealthChecks' and include the value of the 'NextMarker' element in
-- the 'Marker' element to get the next page of results.
lhcrsNextMarker :: Lens' ListHealthChecksResponse (Maybe Text)
lhcrsNextMarker = lens _lhcrsNextMarker (\ s a -> s{_lhcrsNextMarker = a});

-- | The response status code.
lhcrsResponseStatus :: Lens' ListHealthChecksResponse Int
lhcrsResponseStatus = lens _lhcrsResponseStatus (\ s a -> s{_lhcrsResponseStatus = a});

-- | A complex type that contains information about the health checks
-- associated with the current AWS account.
lhcrsHealthChecks :: Lens' ListHealthChecksResponse [HealthCheck]
lhcrsHealthChecks = lens _lhcrsHealthChecks (\ s a -> s{_lhcrsHealthChecks = a}) . _Coerce;

-- | If the request returned more than one page of results, submit another
-- request and specify the value of 'NextMarker' from the last response in
-- the 'marker' parameter to get the next page of results.
lhcrsMarker :: Lens' ListHealthChecksResponse Text
lhcrsMarker = lens _lhcrsMarker (\ s a -> s{_lhcrsMarker = a});

-- | A flag indicating whether there are more health checks to be listed. If
-- your results were truncated, you can make a follow-up request for the
-- next page of results by using the 'Marker' element.
--
-- Valid Values: 'true' | 'false'
lhcrsIsTruncated :: Lens' ListHealthChecksResponse Bool
lhcrsIsTruncated = lens _lhcrsIsTruncated (\ s a -> s{_lhcrsIsTruncated = a});

-- | The maximum number of health checks to be included in the response body.
-- If the number of health checks associated with this AWS account exceeds
-- 'MaxItems', the value of ListHealthChecksResponse$IsTruncated in the
-- response is 'true'. Call 'ListHealthChecks' again and specify the value
-- of ListHealthChecksResponse$NextMarker in the
-- ListHostedZonesRequest$Marker element to get the next page of results.
lhcrsMaxItems :: Lens' ListHealthChecksResponse Text
lhcrsMaxItems = lens _lhcrsMaxItems (\ s a -> s{_lhcrsMaxItems = a});
