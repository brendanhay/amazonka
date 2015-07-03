{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Lambda.ListEventSourceMappings
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of event source mappings you created using the
-- @CreateEventSourceMapping@ (see CreateEventSourceMapping), where you
-- identify a stream as an event source. This list does not include Amazon
-- S3 event sources.
--
-- For each mapping, the API returns configuration information. You can
-- optionally specify filters to retrieve specific event source mappings.
--
-- This operation requires permission for the
-- @lambda:ListEventSourceMappings@ action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_ListEventSourceMappings.html>
module Network.AWS.Lambda.ListEventSourceMappings
    (
    -- * Request
      ListEventSourceMappings
    -- ** Request constructor
    , listEventSourceMappings
    -- ** Request lenses
    , lesmEventSourceARN
    , lesmMaxItems
    , lesmMarker
    , lesmFunctionName

    -- * Response
    , ListEventSourceMappingsResponse
    -- ** Response constructor
    , listEventSourceMappingsResponse
    -- ** Response lenses
    , lesmrEventSourceMappings
    , lesmrNextMarker
    , lesmrStatus
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listEventSourceMappings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lesmEventSourceARN'
--
-- * 'lesmMaxItems'
--
-- * 'lesmMarker'
--
-- * 'lesmFunctionName'
data ListEventSourceMappings = ListEventSourceMappings'
    { _lesmEventSourceARN :: !(Maybe Text)
    , _lesmMaxItems       :: !(Maybe Nat)
    , _lesmMarker         :: !(Maybe Text)
    , _lesmFunctionName   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ListEventSourceMappings' smart constructor.
listEventSourceMappings :: ListEventSourceMappings
listEventSourceMappings =
    ListEventSourceMappings'
    { _lesmEventSourceARN = Nothing
    , _lesmMaxItems = Nothing
    , _lesmMarker = Nothing
    , _lesmFunctionName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream.
lesmEventSourceARN :: Lens' ListEventSourceMappings (Maybe Text)
lesmEventSourceARN = lens _lesmEventSourceARN (\ s a -> s{_lesmEventSourceARN = a});

-- | Optional integer. Specifies the maximum number of event sources to
-- return in response. This value must be greater than 0.
lesmMaxItems :: Lens' ListEventSourceMappings (Maybe Natural)
lesmMaxItems = lens _lesmMaxItems (\ s a -> s{_lesmMaxItems = a}) . mapping _Nat;

-- | Optional string. An opaque pagination token returned from a previous
-- @ListEventSourceMappings@ operation. If present, specifies to continue
-- the list from where the returning call left off.
lesmMarker :: Lens' ListEventSourceMappings (Maybe Text)
lesmMarker = lens _lesmMarker (\ s a -> s{_lesmMarker = a});

-- | The name of the Lambda function.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
lesmFunctionName :: Lens' ListEventSourceMappings (Maybe Text)
lesmFunctionName = lens _lesmFunctionName (\ s a -> s{_lesmFunctionName = a});

instance AWSPager ListEventSourceMappings where
        page rq rs
          | stop (rs ^. lesmrNextMarker) = Nothing
          | stop (rs ^. lesmrEventSourceMappings) = Nothing
          | otherwise =
            Just $ rq & lesmMarker .~ rs ^. lesmrNextMarker

instance AWSRequest ListEventSourceMappings where
        type Sv ListEventSourceMappings = Lambda
        type Rs ListEventSourceMappings =
             ListEventSourceMappingsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListEventSourceMappingsResponse' <$>
                   (x .?> "EventSourceMappings" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListEventSourceMappings where
        toHeaders = const mempty

instance ToPath ListEventSourceMappings where
        toPath = const "/2015-03-31/event-source-mappings/"

instance ToQuery ListEventSourceMappings where
        toQuery ListEventSourceMappings'{..}
          = mconcat
              ["EventSourceArn" =: _lesmEventSourceARN,
               "MaxItems" =: _lesmMaxItems, "Marker" =: _lesmMarker,
               "FunctionName" =: _lesmFunctionName]

-- | Contains a list of event sources (see
-- API_EventSourceMappingConfiguration)
--
-- /See:/ 'listEventSourceMappingsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lesmrEventSourceMappings'
--
-- * 'lesmrNextMarker'
--
-- * 'lesmrStatus'
data ListEventSourceMappingsResponse = ListEventSourceMappingsResponse'
    { _lesmrEventSourceMappings :: !(Maybe [EventSourceMappingConfiguration])
    , _lesmrNextMarker          :: !(Maybe Text)
    , _lesmrStatus              :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListEventSourceMappingsResponse' smart constructor.
listEventSourceMappingsResponse :: Int -> ListEventSourceMappingsResponse
listEventSourceMappingsResponse pStatus =
    ListEventSourceMappingsResponse'
    { _lesmrEventSourceMappings = Nothing
    , _lesmrNextMarker = Nothing
    , _lesmrStatus = pStatus
    }

-- | An array of @EventSourceMappingConfiguration@ objects.
lesmrEventSourceMappings :: Lens' ListEventSourceMappingsResponse [EventSourceMappingConfiguration]
lesmrEventSourceMappings = lens _lesmrEventSourceMappings (\ s a -> s{_lesmrEventSourceMappings = a}) . _Default;

-- | A string, present if there are more event source mappings.
lesmrNextMarker :: Lens' ListEventSourceMappingsResponse (Maybe Text)
lesmrNextMarker = lens _lesmrNextMarker (\ s a -> s{_lesmrNextMarker = a});

-- | FIXME: Undocumented member.
lesmrStatus :: Lens' ListEventSourceMappingsResponse Int
lesmrStatus = lens _lesmrStatus (\ s a -> s{_lesmrStatus = a});
