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
-- Module      : Network.AWS.Route53Domains.ListOperations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the operation IDs of operations that are not yet complete.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ListOperations
    (
    -- * Creating a Request
      listOperations
    , ListOperations
    -- * Request Lenses
    , loMarker
    , loMaxItems
    , loSubmittedSince

    -- * Destructuring the Response
    , listOperationsResponse
    , ListOperationsResponse
    -- * Response Lenses
    , lorsNextPageMarker
    , lorsResponseStatus
    , lorsOperations
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | The ListOperations request includes the following elements.
--
--
--
-- /See:/ 'listOperations' smart constructor.
data ListOperations = ListOperations'
  { _loMarker         :: !(Maybe Text)
  , _loMaxItems       :: !(Maybe Int)
  , _loSubmittedSince :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOperations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loMarker' - For an initial request for a list of operations, omit this element. If the number of operations that are not yet complete is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional operations. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
--
-- * 'loMaxItems' - Number of domains to be returned. Default: 20
--
-- * 'loSubmittedSince' - An optional parameter that lets you get information about all the operations that you submitted after a specified date and time. Specify the date and time in Coordinated Universal time (UTC).
listOperations
    :: ListOperations
listOperations =
  ListOperations'
    {_loMarker = Nothing, _loMaxItems = Nothing, _loSubmittedSince = Nothing}


-- | For an initial request for a list of operations, omit this element. If the number of operations that are not yet complete is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional operations. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
loMarker :: Lens' ListOperations (Maybe Text)
loMarker = lens _loMarker (\ s a -> s{_loMarker = a})

-- | Number of domains to be returned. Default: 20
loMaxItems :: Lens' ListOperations (Maybe Int)
loMaxItems = lens _loMaxItems (\ s a -> s{_loMaxItems = a})

-- | An optional parameter that lets you get information about all the operations that you submitted after a specified date and time. Specify the date and time in Coordinated Universal time (UTC).
loSubmittedSince :: Lens' ListOperations (Maybe UTCTime)
loSubmittedSince = lens _loSubmittedSince (\ s a -> s{_loSubmittedSince = a}) . mapping _Time

instance AWSPager ListOperations where
        page rq rs
          | stop (rs ^. lorsNextPageMarker) = Nothing
          | stop (rs ^. lorsOperations) = Nothing
          | otherwise =
            Just $ rq & loMarker .~ rs ^. lorsNextPageMarker

instance AWSRequest ListOperations where
        type Rs ListOperations = ListOperationsResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 ListOperationsResponse' <$>
                   (x .?> "NextPageMarker") <*> (pure (fromEnum s)) <*>
                     (x .?> "Operations" .!@ mempty))

instance Hashable ListOperations where

instance NFData ListOperations where

instance ToHeaders ListOperations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.ListOperations" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListOperations where
        toJSON ListOperations'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _loMarker,
                  ("MaxItems" .=) <$> _loMaxItems,
                  ("SubmittedSince" .=) <$> _loSubmittedSince])

instance ToPath ListOperations where
        toPath = const "/"

instance ToQuery ListOperations where
        toQuery = const mempty

-- | The ListOperations response includes the following elements.
--
--
--
-- /See:/ 'listOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { _lorsNextPageMarker :: !(Maybe Text)
  , _lorsResponseStatus :: !Int
  , _lorsOperations     :: ![OperationSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOperationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorsNextPageMarker' - If there are more operations than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
--
-- * 'lorsResponseStatus' - -- | The response status code.
--
-- * 'lorsOperations' - Lists summaries of the operations.
listOperationsResponse
    :: Int -- ^ 'lorsResponseStatus'
    -> ListOperationsResponse
listOperationsResponse pResponseStatus_ =
  ListOperationsResponse'
    { _lorsNextPageMarker = Nothing
    , _lorsResponseStatus = pResponseStatus_
    , _lorsOperations = mempty
    }


-- | If there are more operations than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
lorsNextPageMarker :: Lens' ListOperationsResponse (Maybe Text)
lorsNextPageMarker = lens _lorsNextPageMarker (\ s a -> s{_lorsNextPageMarker = a})

-- | -- | The response status code.
lorsResponseStatus :: Lens' ListOperationsResponse Int
lorsResponseStatus = lens _lorsResponseStatus (\ s a -> s{_lorsResponseStatus = a})

-- | Lists summaries of the operations.
lorsOperations :: Lens' ListOperationsResponse [OperationSummary]
lorsOperations = lens _lorsOperations (\ s a -> s{_lorsOperations = a}) . _Coerce

instance NFData ListOperationsResponse where
