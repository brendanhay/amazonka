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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of the health checks that are associated with the current AWS account.
--
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

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request to retrieve a list of the health checks that are associated with the current AWS account.
--
--
--
-- /See:/ 'listHealthChecks' smart constructor.
data ListHealthChecks = ListHealthChecks'
  { _lhcMarker   :: !(Maybe Text)
  , _lhcMaxItems :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHealthChecks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhcMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more health checks. To get another group, submit another @ListHealthChecks@ request.  For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first health check that Amazon Route 53 will return if you submit another request. If the value of @IsTruncated@ in the previous response was @false@ , there are no more health checks to get.
--
-- * 'lhcMaxItems' - The maximum number of health checks that you want @ListHealthChecks@ to return in response to the current request. Amazon Route 53 returns a maximum of 100 items. If you set @MaxItems@ to a value greater than 100, Amazon Route 53 returns only the first 100 health checks.
listHealthChecks
    :: ListHealthChecks
listHealthChecks =
  ListHealthChecks' {_lhcMarker = Nothing, _lhcMaxItems = Nothing}


-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more health checks. To get another group, submit another @ListHealthChecks@ request.  For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first health check that Amazon Route 53 will return if you submit another request. If the value of @IsTruncated@ in the previous response was @false@ , there are no more health checks to get.
lhcMarker :: Lens' ListHealthChecks (Maybe Text)
lhcMarker = lens _lhcMarker (\ s a -> s{_lhcMarker = a})

-- | The maximum number of health checks that you want @ListHealthChecks@ to return in response to the current request. Amazon Route 53 returns a maximum of 100 items. If you set @MaxItems@ to a value greater than 100, Amazon Route 53 returns only the first 100 health checks.
lhcMaxItems :: Lens' ListHealthChecks (Maybe Text)
lhcMaxItems = lens _lhcMaxItems (\ s a -> s{_lhcMaxItems = a})

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

instance Hashable ListHealthChecks where

instance NFData ListHealthChecks where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHealthChecksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhcrsNextMarker' - If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first health check that Amazon Route 53 returns if you submit another @ListHealthChecks@ request and specify the value of @NextMarker@ in the @marker@ parameter.
--
-- * 'lhcrsResponseStatus' - -- | The response status code.
--
-- * 'lhcrsHealthChecks' - A complex type that contains one @HealthCheck@ element for each health check that is associated with the current AWS account.
--
-- * 'lhcrsMarker' - For the second and subsequent calls to @ListHealthChecks@ , @Marker@ is the value that you specified for the @marker@ parameter in the previous request.
--
-- * 'lhcrsIsTruncated' - A flag that indicates whether there are more health checks to be listed. If the response was truncated, you can get the next group of health checks by submitting another @ListHealthChecks@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
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


-- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first health check that Amazon Route 53 returns if you submit another @ListHealthChecks@ request and specify the value of @NextMarker@ in the @marker@ parameter.
lhcrsNextMarker :: Lens' ListHealthChecksResponse (Maybe Text)
lhcrsNextMarker = lens _lhcrsNextMarker (\ s a -> s{_lhcrsNextMarker = a})

-- | -- | The response status code.
lhcrsResponseStatus :: Lens' ListHealthChecksResponse Int
lhcrsResponseStatus = lens _lhcrsResponseStatus (\ s a -> s{_lhcrsResponseStatus = a})

-- | A complex type that contains one @HealthCheck@ element for each health check that is associated with the current AWS account.
lhcrsHealthChecks :: Lens' ListHealthChecksResponse [HealthCheck]
lhcrsHealthChecks = lens _lhcrsHealthChecks (\ s a -> s{_lhcrsHealthChecks = a}) . _Coerce

-- | For the second and subsequent calls to @ListHealthChecks@ , @Marker@ is the value that you specified for the @marker@ parameter in the previous request.
lhcrsMarker :: Lens' ListHealthChecksResponse Text
lhcrsMarker = lens _lhcrsMarker (\ s a -> s{_lhcrsMarker = a})

-- | A flag that indicates whether there are more health checks to be listed. If the response was truncated, you can get the next group of health checks by submitting another @ListHealthChecks@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
lhcrsIsTruncated :: Lens' ListHealthChecksResponse Bool
lhcrsIsTruncated = lens _lhcrsIsTruncated (\ s a -> s{_lhcrsIsTruncated = a})

-- | The value that you specified for the @maxitems@ parameter in the call to @ListHealthChecks@ that produced the current response.
lhcrsMaxItems :: Lens' ListHealthChecksResponse Text
lhcrsMaxItems = lens _lhcrsMaxItems (\ s a -> s{_lhcrsMaxItems = a})

instance NFData ListHealthChecksResponse where
