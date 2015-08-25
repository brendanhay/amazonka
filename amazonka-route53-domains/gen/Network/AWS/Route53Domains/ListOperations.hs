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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the operation IDs of operations that are not yet
-- complete.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/api-ListOperations.html AWS API Reference> for ListOperations.
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ListOperations
    (
    -- * Creating a Request
      listOperations
    , ListOperations
    -- * Request Lenses
    , loMaxItems
    , loMarker

    -- * Destructuring the Response
    , listOperationsResponse
    , ListOperationsResponse
    -- * Response Lenses
    , lorsNextPageMarker
    , lorsStatus
    , lorsOperations
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | The ListOperations request includes the following elements.
--
-- /See:/ 'listOperations' smart constructor.
data ListOperations = ListOperations'
    { _loMaxItems :: !(Maybe Int)
    , _loMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListOperations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loMaxItems'
--
-- * 'loMarker'
listOperations
    :: ListOperations
listOperations =
    ListOperations'
    { _loMaxItems = Nothing
    , _loMarker = Nothing
    }

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
-- value that you specified for 'MaxItems', you can use 'Marker' to return
-- additional operations. Get the value of 'NextPageMarker' from the
-- previous response, and submit another request that includes the value of
-- 'NextPageMarker' in the 'Marker' element.
--
-- Type: String
--
-- Default: None
--
-- Required: No
loMarker :: Lens' ListOperations (Maybe Text)
loMarker = lens _loMarker (\ s a -> s{_loMarker = a});

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
                 [("MaxItems" .=) <$> _loMaxItems,
                  ("Marker" .=) <$> _loMarker])

instance ToPath ListOperations where
        toPath = const "/"

instance ToQuery ListOperations where
        toQuery = const mempty

-- | The ListOperations response includes the following elements.
--
-- /See:/ 'listOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
    { _lorsNextPageMarker :: !(Maybe Text)
    , _lorsStatus         :: !Int
    , _lorsOperations     :: ![OperationSummary]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListOperationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorsNextPageMarker'
--
-- * 'lorsStatus'
--
-- * 'lorsOperations'
listOperationsResponse
    :: Int -- ^ 'lorsStatus'
    -> ListOperationsResponse
listOperationsResponse pStatus_ =
    ListOperationsResponse'
    { _lorsNextPageMarker = Nothing
    , _lorsStatus = pStatus_
    , _lorsOperations = mempty
    }

-- | If there are more operations than you specified for 'MaxItems' in the
-- request, submit another request and include the value of
-- 'NextPageMarker' in the value of 'Marker'.
--
-- Type: String
--
-- Parent: 'Operations'
lorsNextPageMarker :: Lens' ListOperationsResponse (Maybe Text)
lorsNextPageMarker = lens _lorsNextPageMarker (\ s a -> s{_lorsNextPageMarker = a});

-- | The response status code.
lorsStatus :: Lens' ListOperationsResponse Int
lorsStatus = lens _lorsStatus (\ s a -> s{_lorsStatus = a});

-- | Lists summaries of the operations.
--
-- Type: Complex type containing a list of operation summaries
--
-- Children: 'OperationId', 'Status', 'SubmittedDate', 'Type'
lorsOperations :: Lens' ListOperationsResponse [OperationSummary]
lorsOperations = lens _lorsOperations (\ s a -> s{_lorsOperations = a}) . _Coerce;
