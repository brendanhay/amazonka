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
-- Module      : Network.AWS.Lambda.ListEventSourceMappings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists event source mappings. Specify an @EventSourceArn@ to only show event source mappings for a single event source.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListEventSourceMappings
    (
    -- * Creating a Request
      listEventSourceMappings
    , ListEventSourceMappings
    -- * Request Lenses
    , lesmEventSourceARN
    , lesmMarker
    , lesmMaxItems
    , lesmFunctionName

    -- * Destructuring the Response
    , listEventSourceMappingsResponse
    , ListEventSourceMappingsResponse
    -- * Response Lenses
    , lesmrsEventSourceMappings
    , lesmrsNextMarker
    , lesmrsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listEventSourceMappings' smart constructor.
data ListEventSourceMappings = ListEventSourceMappings'
  { _lesmEventSourceARN :: !(Maybe Text)
  , _lesmMarker         :: !(Maybe Text)
  , _lesmMaxItems       :: !(Maybe Nat)
  , _lesmFunctionName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEventSourceMappings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lesmEventSourceARN' - The Amazon Resource Name (ARN) of the event source.     * __Amazon Kinesis__ - The ARN of the data stream or a stream consumer.     * __Amazon DynamoDB Streams__ - The ARN of the stream.     * __Amazon Simple Queue Service__ - The ARN of the queue.
--
-- * 'lesmMarker' - A pagination token returned by a previous call.
--
-- * 'lesmMaxItems' - The maximum number of event source mappings to return.
--
-- * 'lesmFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
listEventSourceMappings
    :: ListEventSourceMappings
listEventSourceMappings =
  ListEventSourceMappings'
    { _lesmEventSourceARN = Nothing
    , _lesmMarker = Nothing
    , _lesmMaxItems = Nothing
    , _lesmFunctionName = Nothing
    }


-- | The Amazon Resource Name (ARN) of the event source.     * __Amazon Kinesis__ - The ARN of the data stream or a stream consumer.     * __Amazon DynamoDB Streams__ - The ARN of the stream.     * __Amazon Simple Queue Service__ - The ARN of the queue.
lesmEventSourceARN :: Lens' ListEventSourceMappings (Maybe Text)
lesmEventSourceARN = lens _lesmEventSourceARN (\ s a -> s{_lesmEventSourceARN = a})

-- | A pagination token returned by a previous call.
lesmMarker :: Lens' ListEventSourceMappings (Maybe Text)
lesmMarker = lens _lesmMarker (\ s a -> s{_lesmMarker = a})

-- | The maximum number of event source mappings to return.
lesmMaxItems :: Lens' ListEventSourceMappings (Maybe Natural)
lesmMaxItems = lens _lesmMaxItems (\ s a -> s{_lesmMaxItems = a}) . mapping _Nat

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
lesmFunctionName :: Lens' ListEventSourceMappings (Maybe Text)
lesmFunctionName = lens _lesmFunctionName (\ s a -> s{_lesmFunctionName = a})

instance AWSPager ListEventSourceMappings where
        page rq rs
          | stop (rs ^. lesmrsNextMarker) = Nothing
          | stop (rs ^. lesmrsEventSourceMappings) = Nothing
          | otherwise =
            Just $ rq & lesmMarker .~ rs ^. lesmrsNextMarker

instance AWSRequest ListEventSourceMappings where
        type Rs ListEventSourceMappings =
             ListEventSourceMappingsResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 ListEventSourceMappingsResponse' <$>
                   (x .?> "EventSourceMappings" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListEventSourceMappings where

instance NFData ListEventSourceMappings where

instance ToHeaders ListEventSourceMappings where
        toHeaders = const mempty

instance ToPath ListEventSourceMappings where
        toPath = const "/2015-03-31/event-source-mappings/"

instance ToQuery ListEventSourceMappings where
        toQuery ListEventSourceMappings'{..}
          = mconcat
              ["EventSourceArn" =: _lesmEventSourceARN,
               "Marker" =: _lesmMarker, "MaxItems" =: _lesmMaxItems,
               "FunctionName" =: _lesmFunctionName]

-- | /See:/ 'listEventSourceMappingsResponse' smart constructor.
data ListEventSourceMappingsResponse = ListEventSourceMappingsResponse'
  { _lesmrsEventSourceMappings :: !(Maybe [EventSourceMappingConfiguration])
  , _lesmrsNextMarker          :: !(Maybe Text)
  , _lesmrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEventSourceMappingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lesmrsEventSourceMappings' - A list of event source mappings.
--
-- * 'lesmrsNextMarker' - A pagination token that's returned when the response doesn't contain all event source mappings.
--
-- * 'lesmrsResponseStatus' - -- | The response status code.
listEventSourceMappingsResponse
    :: Int -- ^ 'lesmrsResponseStatus'
    -> ListEventSourceMappingsResponse
listEventSourceMappingsResponse pResponseStatus_ =
  ListEventSourceMappingsResponse'
    { _lesmrsEventSourceMappings = Nothing
    , _lesmrsNextMarker = Nothing
    , _lesmrsResponseStatus = pResponseStatus_
    }


-- | A list of event source mappings.
lesmrsEventSourceMappings :: Lens' ListEventSourceMappingsResponse [EventSourceMappingConfiguration]
lesmrsEventSourceMappings = lens _lesmrsEventSourceMappings (\ s a -> s{_lesmrsEventSourceMappings = a}) . _Default . _Coerce

-- | A pagination token that's returned when the response doesn't contain all event source mappings.
lesmrsNextMarker :: Lens' ListEventSourceMappingsResponse (Maybe Text)
lesmrsNextMarker = lens _lesmrsNextMarker (\ s a -> s{_lesmrsNextMarker = a})

-- | -- | The response status code.
lesmrsResponseStatus :: Lens' ListEventSourceMappingsResponse Int
lesmrsResponseStatus = lens _lesmrsResponseStatus (\ s a -> s{_lesmrsResponseStatus = a})

instance NFData ListEventSourceMappingsResponse where
