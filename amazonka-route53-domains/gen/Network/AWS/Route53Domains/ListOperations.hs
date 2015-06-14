{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53Domains.ListOperations
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns the operation IDs of operations that are not yet
-- complete.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-ListOperations.html>
module Network.AWS.Route53Domains.ListOperations
    (
    -- * Request
      ListOperations
    -- ** Request constructor
    , listOperations
    -- ** Request lenses
    , loMaxItems
    , loMarker

    -- * Response
    , ListOperationsResponse
    -- ** Response constructor
    , listOperationsResponse
    -- ** Response lenses
    , lorNextPageMarker
    , lorOperations
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Route53Domains.Types

-- | /See:/ 'listOperations' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'loMaxItems'
--
-- * 'loMarker'
data ListOperations = ListOperations'{_loMaxItems :: Maybe Int, _loMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListOperations' smart constructor.
listOperations :: ListOperations
listOperations = ListOperations'{_loMaxItems = Nothing, _loMarker = Nothing};

-- | Number of domains to be returned.
--
-- Type: Integer
--
-- Default: 20
--
-- Constraints: A value between 1 and 100.
--
-- Required: No
loMaxItems :: Lens' ListOperations (Maybe Int)
loMaxItems = lens _loMaxItems (\ s a -> s{_loMaxItems = a});

-- | For an initial request for a list of operations, omit this element. If
-- the number of operations that are not yet complete is greater than the
-- value that you specified for @MaxItems@, you can use @Marker@ to return
-- additional operations. Get the value of @NextPageMarker@ from the
-- previous response, and submit another request that includes the value of
-- @NextPageMarker@ in the @Marker@ element.
--
-- Type: String
--
-- Default: None
--
-- Required: No
loMarker :: Lens' ListOperations (Maybe Text)
loMarker = lens _loMarker (\ s a -> s{_loMarker = a});

instance AWSRequest ListOperations where
        type Sv ListOperations = Route53Domains
        type Rs ListOperations = ListOperationsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListOperationsResponse' <$>
                   x .?> "NextPageMarker" <*>
                     x .?> "Operations" .!@ mempty)

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
              ["MaxItems" .= _loMaxItems, "Marker" .= _loMarker]

instance ToPath ListOperations where
        toPath = const "/"

instance ToQuery ListOperations where
        toQuery = const mempty

-- | /See:/ 'listOperationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lorNextPageMarker'
--
-- * 'lorOperations'
data ListOperationsResponse = ListOperationsResponse'{_lorNextPageMarker :: Maybe Text, _lorOperations :: [OperationSummary]} deriving (Eq, Read, Show)

-- | 'ListOperationsResponse' smart constructor.
listOperationsResponse :: [OperationSummary] -> ListOperationsResponse
listOperationsResponse pOperations = ListOperationsResponse'{_lorNextPageMarker = Nothing, _lorOperations = pOperations};

-- | If there are more operations than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- Type: String
--
-- Parent: @Operations@
lorNextPageMarker :: Lens' ListOperationsResponse (Maybe Text)
lorNextPageMarker = lens _lorNextPageMarker (\ s a -> s{_lorNextPageMarker = a});

-- | Lists summaries of the operations.
--
-- Type: Complex type containing a list of operation summaries
--
-- Children: @OperationId@, @Status@, @SubmittedDate@, @Type@
lorOperations :: Lens' ListOperationsResponse [OperationSummary]
lorOperations = lens _lorOperations (\ s a -> s{_lorOperations = a});
