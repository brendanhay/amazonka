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
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_ListEventSourceMappings.html>
module Network.AWS.Lambda.ListEventSourceMappings
    (
    -- * Request
      ListEventSourceMappings
    -- ** Request constructor
    , listEventSourceMappings
    -- ** Request lenses
    , lesmrqEventSourceARN
    , lesmrqMaxItems
    , lesmrqMarker
    , lesmrqFunctionName

    -- * Response
    , ListEventSourceMappingsResponse
    -- ** Response constructor
    , listEventSourceMappingsResponse
    -- ** Response lenses
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
-- * 'lesmrqEventSourceARN'
--
-- * 'lesmrqMaxItems'
--
-- * 'lesmrqMarker'
--
-- * 'lesmrqFunctionName'
data ListEventSourceMappings = ListEventSourceMappings'
    { _lesmrqEventSourceARN :: !(Maybe Text)
    , _lesmrqMaxItems       :: !(Maybe Nat)
    , _lesmrqMarker         :: !(Maybe Text)
    , _lesmrqFunctionName   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListEventSourceMappings' smart constructor.
listEventSourceMappings :: ListEventSourceMappings
listEventSourceMappings =
    ListEventSourceMappings'
    { _lesmrqEventSourceARN = Nothing
    , _lesmrqMaxItems = Nothing
    , _lesmrqMarker = Nothing
    , _lesmrqFunctionName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream.
lesmrqEventSourceARN :: Lens' ListEventSourceMappings (Maybe Text)
lesmrqEventSourceARN = lens _lesmrqEventSourceARN (\ s a -> s{_lesmrqEventSourceARN = a});

-- | Optional integer. Specifies the maximum number of event sources to
-- return in response. This value must be greater than 0.
lesmrqMaxItems :: Lens' ListEventSourceMappings (Maybe Natural)
lesmrqMaxItems = lens _lesmrqMaxItems (\ s a -> s{_lesmrqMaxItems = a}) . mapping _Nat;

-- | Optional string. An opaque pagination token returned from a previous
-- @ListEventSourceMappings@ operation. If present, specifies to continue
-- the list from where the returning call left off.
lesmrqMarker :: Lens' ListEventSourceMappings (Maybe Text)
lesmrqMarker = lens _lesmrqMarker (\ s a -> s{_lesmrqMarker = a});

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
lesmrqFunctionName :: Lens' ListEventSourceMappings (Maybe Text)
lesmrqFunctionName = lens _lesmrqFunctionName (\ s a -> s{_lesmrqFunctionName = a});

instance AWSPager ListEventSourceMappings where
        page rq rs
          | stop (rs ^. lesmrsNextMarker) = Nothing
          | stop (rs ^. lesmrsEventSourceMappings) = Nothing
          | otherwise =
            Just $ rq & lesmrqMarker .~ rs ^. lesmrsNextMarker

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
              ["EventSourceArn" =: _lesmrqEventSourceARN,
               "MaxItems" =: _lesmrqMaxItems,
               "Marker" =: _lesmrqMarker,
               "FunctionName" =: _lesmrqFunctionName]

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
listEventSourceMappingsResponse pStatus =
    ListEventSourceMappingsResponse'
    { _lesmrsEventSourceMappings = Nothing
    , _lesmrsNextMarker = Nothing
    , _lesmrsStatus = pStatus
    }

-- | An array of @EventSourceMappingConfiguration@ objects.
lesmrsEventSourceMappings :: Lens' ListEventSourceMappingsResponse [EventSourceMappingConfiguration]
lesmrsEventSourceMappings = lens _lesmrsEventSourceMappings (\ s a -> s{_lesmrsEventSourceMappings = a}) . _Default;

-- | A string, present if there are more event source mappings.
lesmrsNextMarker :: Lens' ListEventSourceMappingsResponse (Maybe Text)
lesmrsNextMarker = lens _lesmrsNextMarker (\ s a -> s{_lesmrsNextMarker = a});

-- | FIXME: Undocumented member.
lesmrsStatus :: Lens' ListEventSourceMappingsResponse Int
lesmrsStatus = lens _lesmrsStatus (\ s a -> s{_lesmrsStatus = a});
