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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of your health checks. Send a @GET@ request to the @/2013-04-01/healthcheck@ resource. The response to this request includes a @HealthChecks@ element with zero or more @HealthCheck@ child elements. By default, the list of health checks is displayed on a single page. You can control the length of the page that is displayed by using the @MaxItems@ parameter. You can use the @Marker@ parameter to control the health check that the list begins with.
--
--
-- For information about listing health checks using the Amazon Route 53 console, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover.html Amazon Route 53 Health Checks and DNS Failover> .
--
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

import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | To retrieve a list of your health checks, send a @GET@ request to the @/2013-04-01/healthcheck@ resource. The response to this request includes a @HealthChecks@ element with zero or more @HealthCheck@ child elements. By default, the list of health checks is displayed on a single page. You can control the length of the page that is displayed by using the @MaxItems@ parameter. You can use the @Marker@ parameter to control the health check that the list begins with.
--
--
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
-- * 'lhcMarker' - If the response to a @ListHealthChecks@ is more than one page, marker is the health check ID for the first health check on the next page of results. For more information, see 'ListHealthChecksResponse$MaxItems' .
--
-- * 'lhcMaxItems' - The maximum number of @HealthCheck@ elements you want @ListHealthChecks@ to return on each page of the response body. If the AWS account includes more @HealthCheck@ elements than the value of @maxitems@ , the response is broken into pages. Each page contains the number of @HealthCheck@ elements specified by @maxitems@ . For example, suppose you specify @10@ for @maxitems@ and the current AWS account has @51@ health checks. In the response, @ListHealthChecks@ sets 'ListHealthChecksResponse$IsTruncated' to true and includes the 'ListHealthChecksResponse$NextMarker' element. To access the second and subsequent pages, you resend the @GET@ @ListHealthChecks@ request, add the 'ListHealthChecksResponse$Marker' parameter to the request, and specify the value of the 'ListHealthChecksResponse$NextMarker' element from the previous response. On the last (sixth) page of the response, which contains only one HealthCheck element:     * The value of 'ListHealthChecksResponse$IsTruncated' is @false@ .     * 'ListHealthChecksResponse$NextMarker' is omitted.
listHealthChecks
    :: ListHealthChecks
listHealthChecks =
    ListHealthChecks'
    { _lhcMarker = Nothing
    , _lhcMaxItems = Nothing
    }

-- | If the response to a @ListHealthChecks@ is more than one page, marker is the health check ID for the first health check on the next page of results. For more information, see 'ListHealthChecksResponse$MaxItems' .
lhcMarker :: Lens' ListHealthChecks (Maybe Text)
lhcMarker = lens _lhcMarker (\ s a -> s{_lhcMarker = a});

-- | The maximum number of @HealthCheck@ elements you want @ListHealthChecks@ to return on each page of the response body. If the AWS account includes more @HealthCheck@ elements than the value of @maxitems@ , the response is broken into pages. Each page contains the number of @HealthCheck@ elements specified by @maxitems@ . For example, suppose you specify @10@ for @maxitems@ and the current AWS account has @51@ health checks. In the response, @ListHealthChecks@ sets 'ListHealthChecksResponse$IsTruncated' to true and includes the 'ListHealthChecksResponse$NextMarker' element. To access the second and subsequent pages, you resend the @GET@ @ListHealthChecks@ request, add the 'ListHealthChecksResponse$Marker' parameter to the request, and specify the value of the 'ListHealthChecksResponse$NextMarker' element from the previous response. On the last (sixth) page of the response, which contains only one HealthCheck element:     * The value of 'ListHealthChecksResponse$IsTruncated' is @false@ .     * 'ListHealthChecksResponse$NextMarker' is omitted.
lhcMaxItems :: Lens' ListHealthChecks (Maybe Text)
lhcMaxItems = lens _lhcMaxItems (\ s a -> s{_lhcMaxItems = a});

instance AWSPager ListHealthChecks where
        page rq rs
          | stop (rs ^. lhcrsIsTruncated) = Nothing
          | isNothing (rs ^. lhcrsNextMarker) = Nothing
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

instance Hashable ListHealthChecks

instance NFData ListHealthChecks

instance ToHeaders ListHealthChecks where
        toHeaders = const mempty

instance ToPath ListHealthChecks where
        toPath = const "/2013-04-01/healthcheck"

instance ToQuery ListHealthChecks where
        toQuery ListHealthChecks'{..}
          = mconcat
              ["marker" =: _lhcMarker, "maxitems" =: _lhcMaxItems]

-- | A complex type that contains the response to a @ListHealthChecks@ request.
--
--
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
-- * 'lhcrsNextMarker' - If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first health check in the next group of @maxitems@ health checks. Call @ListHealthChecks@ again and specify the value of @NextMarker@ in the marker parameter.
--
-- * 'lhcrsResponseStatus' - -- | The response status code.
--
-- * 'lhcrsHealthChecks' - A complex type that contains one @HealthCheck@ element for each health check that is associated with the current AWS account.
--
-- * 'lhcrsMarker' - For the second and subsequent calls to @ListHealthChecks@ , @Marker@ is the value that you specified for the marker parameter in the previous request.
--
-- * 'lhcrsIsTruncated' - A flag that indicates whether there are more health checks to be listed. If the response was truncated, you can get the next group of @maxitems@ health checks by calling @ListHealthChecks@ again and specifying the value of the @NextMarker@ element in the marker parameter. Valid Values: @true@ | @false@
--
-- * 'lhcrsMaxItems' - The value that you specified for the @maxitems@ parameter in the call to @ListHealthChecks@ that produced the current response.
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

-- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first health check in the next group of @maxitems@ health checks. Call @ListHealthChecks@ again and specify the value of @NextMarker@ in the marker parameter.
lhcrsNextMarker :: Lens' ListHealthChecksResponse (Maybe Text)
lhcrsNextMarker = lens _lhcrsNextMarker (\ s a -> s{_lhcrsNextMarker = a});

-- | -- | The response status code.
lhcrsResponseStatus :: Lens' ListHealthChecksResponse Int
lhcrsResponseStatus = lens _lhcrsResponseStatus (\ s a -> s{_lhcrsResponseStatus = a});

-- | A complex type that contains one @HealthCheck@ element for each health check that is associated with the current AWS account.
lhcrsHealthChecks :: Lens' ListHealthChecksResponse [HealthCheck]
lhcrsHealthChecks = lens _lhcrsHealthChecks (\ s a -> s{_lhcrsHealthChecks = a}) . _Coerce;

-- | For the second and subsequent calls to @ListHealthChecks@ , @Marker@ is the value that you specified for the marker parameter in the previous request.
lhcrsMarker :: Lens' ListHealthChecksResponse Text
lhcrsMarker = lens _lhcrsMarker (\ s a -> s{_lhcrsMarker = a});

-- | A flag that indicates whether there are more health checks to be listed. If the response was truncated, you can get the next group of @maxitems@ health checks by calling @ListHealthChecks@ again and specifying the value of the @NextMarker@ element in the marker parameter. Valid Values: @true@ | @false@
lhcrsIsTruncated :: Lens' ListHealthChecksResponse Bool
lhcrsIsTruncated = lens _lhcrsIsTruncated (\ s a -> s{_lhcrsIsTruncated = a});

-- | The value that you specified for the @maxitems@ parameter in the call to @ListHealthChecks@ that produced the current response.
lhcrsMaxItems :: Lens' ListHealthChecksResponse Text
lhcrsMaxItems = lens _lhcrsMaxItems (\ s a -> s{_lhcrsMaxItems = a});

instance NFData ListHealthChecksResponse
