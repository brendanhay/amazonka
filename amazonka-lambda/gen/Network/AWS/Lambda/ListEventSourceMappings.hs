{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListEventSourceMappings
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of event source mappings you created using the
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
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_ListEventSourceMappings.html AWS API Reference> for ListEventSourceMappings.
module Network.AWS.Lambda.ListEventSourceMappings
    (
    -- * Creating a Request
      ListEventSourceMappings
    , listEventSourceMappings
    -- * Request Lenses
    , lesmEventSourceARN
    , lesmMaxItems
    , lesmMarker
    , lesmFunctionName

    -- * Destructuring the Response
    , ListEventSourceMappingsResponse
    , listEventSourceMappingsResponse
    -- * Response Lenses
    , lesmrsEventSourceMappings
    , lesmrsNextMarker
    , lesmrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
          | stop (rs ^. lesmrsNextMarker) = Nothing
          | stop (rs ^. lesmrsEventSourceMappings) = Nothing
          | otherwise =
            Just $ rq & lesmMarker .~ rs ^. lesmrsNextMarker

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
-- * 'lesmrsEventSourceMappings'
--
-- * 'lesmrsNextMarker'
--
-- * 'lesmrsStatus'
data ListEventSourceMappingsResponse = ListEventSourceMappingsResponse'
    { _lesmrsEventSourceMappings :: !(Maybe [EventSourceMappingConfiguration])
    , _lesmrsNextMarker          :: !(Maybe Text)
    , _lesmrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListEventSourceMappingsResponse' smart constructor.
listEventSourceMappingsResponse :: Int -> ListEventSourceMappingsResponse
listEventSourceMappingsResponse pStatus_ =
    ListEventSourceMappingsResponse'
    { _lesmrsEventSourceMappings = Nothing
    , _lesmrsNextMarker = Nothing
    , _lesmrsStatus = pStatus_
    }

-- | An array of @EventSourceMappingConfiguration@ objects.
lesmrsEventSourceMappings :: Lens' ListEventSourceMappingsResponse [EventSourceMappingConfiguration]
lesmrsEventSourceMappings = lens _lesmrsEventSourceMappings (\ s a -> s{_lesmrsEventSourceMappings = a}) . _Default . _Coerce;

-- | A string, present if there are more event source mappings.
lesmrsNextMarker :: Lens' ListEventSourceMappingsResponse (Maybe Text)
lesmrsNextMarker = lens _lesmrsNextMarker (\ s a -> s{_lesmrsNextMarker = a});

-- | Undocumented member.
lesmrsStatus :: Lens' ListEventSourceMappingsResponse Int
lesmrsStatus = lens _lesmrsStatus (\ s a -> s{_lesmrsStatus = a});
