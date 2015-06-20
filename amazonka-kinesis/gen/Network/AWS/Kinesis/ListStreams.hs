{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Kinesis.ListStreams
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

-- | Lists your streams.
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
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_ListStreams.html>
module Network.AWS.Kinesis.ListStreams
    (
    -- * Request
      ListStreams
    -- ** Request constructor
    , listStreams
    -- ** Request lenses
    , lsLimit
    , lsExclusiveStartStreamName

    -- * Response
    , ListStreamsResponse
    -- ** Response constructor
    , listStreamsResponse
    -- ** Response lenses
    , lsrStreamNames
    , lsrHasMoreStreams
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listStreams' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsLimit'
--
-- * 'lsExclusiveStartStreamName'
data ListStreams = ListStreams'{_lsLimit :: Maybe Nat, _lsExclusiveStartStreamName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListStreams' smart constructor.
listStreams :: ListStreams
listStreams = ListStreams'{_lsLimit = Nothing, _lsExclusiveStartStreamName = Nothing};

-- | The maximum number of streams to list.
lsLimit :: Lens' ListStreams (Maybe Natural)
lsLimit = lens _lsLimit (\ s a -> s{_lsLimit = a}) . mapping _Nat;

-- | The name of the stream to start the list with.
lsExclusiveStartStreamName :: Lens' ListStreams (Maybe Text)
lsExclusiveStartStreamName = lens _lsExclusiveStartStreamName (\ s a -> s{_lsExclusiveStartStreamName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest ListStreams where
        type Sv ListStreams = Kinesis
        type Rs ListStreams = ListStreamsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListStreamsResponse' <$>
                   (x .?> "StreamNames" .!@ mempty) <*>
                     (x .:> "HasMoreStreams"))

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

-- | /See:/ 'listStreamsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrStreamNames'
--
-- * 'lsrHasMoreStreams'
data ListStreamsResponse = ListStreamsResponse'{_lsrStreamNames :: [Text], _lsrHasMoreStreams :: Bool} deriving (Eq, Read, Show)

-- | 'ListStreamsResponse' smart constructor.
listStreamsResponse :: Bool -> ListStreamsResponse
listStreamsResponse pHasMoreStreams = ListStreamsResponse'{_lsrStreamNames = mempty, _lsrHasMoreStreams = pHasMoreStreams};

-- | The names of the streams that are associated with the AWS account making
-- the @ListStreams@ request.
lsrStreamNames :: Lens' ListStreamsResponse [Text]
lsrStreamNames = lens _lsrStreamNames (\ s a -> s{_lsrStreamNames = a});

-- | If set to @true@, there are more streams available to list.
lsrHasMoreStreams :: Lens' ListStreamsResponse Bool
lsrHasMoreStreams = lens _lsrHasMoreStreams (\ s a -> s{_lsrHasMoreStreams = a});
