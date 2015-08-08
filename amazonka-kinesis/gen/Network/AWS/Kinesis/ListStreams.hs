{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.ListStreams
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your streams.
--
-- The number of streams may be too large to return from a single call to
-- @ListStreams@. You can limit the number of returned streams using the
-- @Limit@ parameter. If you do not specify a value for the @Limit@
-- parameter, Amazon Kinesis uses the default limit, which is currently 10.
--
-- You can detect if there are more streams available to list by using the
-- @HasMoreStreams@ flag from the returned output. If there are more
-- streams available, you can request more streams by using the name of the
-- last stream returned by the @ListStreams@ request in the
-- @ExclusiveStartStreamName@ parameter in a subsequent request to
-- @ListStreams@. The group of stream names returned by the subsequent
-- request is then added to the list. You can continue this process until
-- all the stream names have been collected in the list.
--
-- ListStreams has a limit of 5 transactions per second per account.
--
-- /See:/ <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_ListStreams.html AWS API Reference> for ListStreams.
module Network.AWS.Kinesis.ListStreams
    (
    -- * Creating a Request
      ListStreams
    , listStreams
    -- * Request Lenses
    , lsLimit
    , lsExclusiveStartStreamName

    -- * Destructuring the Response
    , ListStreamsResponse
    , listStreamsResponse
    -- * Response Lenses
    , lsrsStatus
    , lsrsStreamNames
    , lsrsHasMoreStreams
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for @ListStreams@.
--
-- /See:/ 'listStreams' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsLimit'
--
-- * 'lsExclusiveStartStreamName'
data ListStreams = ListStreams'
    { _lsLimit                    :: !(Maybe Nat)
    , _lsExclusiveStartStreamName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListStreams' smart constructor.
listStreams :: ListStreams
listStreams =
    ListStreams'
    { _lsLimit = Nothing
    , _lsExclusiveStartStreamName = Nothing
    }

-- | The maximum number of streams to list.
lsLimit :: Lens' ListStreams (Maybe Natural)
lsLimit = lens _lsLimit (\ s a -> s{_lsLimit = a}) . mapping _Nat;

-- | The name of the stream to start the list with.
lsExclusiveStartStreamName :: Lens' ListStreams (Maybe Text)
lsExclusiveStartStreamName = lens _lsExclusiveStartStreamName (\ s a -> s{_lsExclusiveStartStreamName = a});

instance AWSPager ListStreams where
        page rq rs
          | stop (rs ^. lsrsHasMoreStreams) = Nothing
          | isNothing (rs ^? lsrsStreamNames . _last) = Nothing
          | otherwise =
            Just $ rq &
              lsExclusiveStartStreamName .~
                rs ^? lsrsStreamNames . _last

instance AWSRequest ListStreams where
        type Sv ListStreams = Kinesis
        type Rs ListStreams = ListStreamsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListStreamsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "StreamNames" .!@ mempty)
                     <*> (x .:> "HasMoreStreams"))

instance ToHeaders ListStreams where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.ListStreams" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListStreams where
        toJSON ListStreams'{..}
          = object
              ["Limit" .= _lsLimit,
               "ExclusiveStartStreamName" .=
                 _lsExclusiveStartStreamName]

instance ToPath ListStreams where
        toPath = const "/"

instance ToQuery ListStreams where
        toQuery = const mempty

-- | Represents the output for @ListStreams@.
--
-- /See:/ 'listStreamsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrsStatus'
--
-- * 'lsrsStreamNames'
--
-- * 'lsrsHasMoreStreams'
data ListStreamsResponse = ListStreamsResponse'
    { _lsrsStatus         :: !Int
    , _lsrsStreamNames    :: ![Text]
    , _lsrsHasMoreStreams :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListStreamsResponse' smart constructor.
listStreamsResponse :: Int -> Bool -> ListStreamsResponse
listStreamsResponse pStatus_ pHasMoreStreams_ =
    ListStreamsResponse'
    { _lsrsStatus = pStatus_
    , _lsrsStreamNames = mempty
    , _lsrsHasMoreStreams = pHasMoreStreams_
    }

-- | Undocumented member.
lsrsStatus :: Lens' ListStreamsResponse Int
lsrsStatus = lens _lsrsStatus (\ s a -> s{_lsrsStatus = a});

-- | The names of the streams that are associated with the AWS account making
-- the @ListStreams@ request.
lsrsStreamNames :: Lens' ListStreamsResponse [Text]
lsrsStreamNames = lens _lsrsStreamNames (\ s a -> s{_lsrsStreamNames = a}) . _Coerce;

-- | If set to @true@, there are more streams available to list.
lsrsHasMoreStreams :: Lens' ListStreamsResponse Bool
lsrsHasMoreStreams = lens _lsrsHasMoreStreams (\ s a -> s{_lsrsHasMoreStreams = a});
