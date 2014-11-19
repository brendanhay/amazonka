{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.ListStreams
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists your streams. The number of streams may be too large to return from a
-- single call to ListStreams. You can limit the number of returned streams
-- using the Limit parameter. If you do not specify a value for the Limit
-- parameter, Amazon Kinesis uses the default limit, which is currently 10.
-- You can detect if there are more streams available to list by using the
-- HasMoreStreams flag from the returned output. If there are more streams
-- available, you can request more streams by using the name of the last
-- stream returned by the ListStreams request in the ExclusiveStartStreamName
-- parameter in a subsequent request to ListStreams. The group of stream names
-- returned by the subsequent request is then added to the list. You can
-- continue this process until all the stream names have been collected in the
-- list. ListStreams has a limit of 5 transactions per second per account.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_ListStreams.html>
module Network.AWS.Kinesis.ListStreams
    (
    -- * Request
      ListStreams
    -- ** Request constructor
    , listStreams
    -- ** Request lenses
    , lsExclusiveStartStreamName
    , lsLimit

    -- * Response
    , ListStreamsResponse
    -- ** Response constructor
    , listStreamsResponse
    -- ** Response lenses
    , lsrHasMoreStreams
    , lsrStreamNames
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Kinesis.Types
import qualified GHC.Exts

data ListStreams = ListStreams
    { _lsExclusiveStartStreamName :: Maybe Text
    , _lsLimit                    :: Maybe Nat
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListStreams' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsExclusiveStartStreamName' @::@ 'Maybe' 'Text'
--
-- * 'lsLimit' @::@ 'Maybe' 'Natural'
--
listStreams :: ListStreams
listStreams = ListStreams
    { _lsLimit                    = Nothing
    , _lsExclusiveStartStreamName = Nothing
    }

-- | The name of the stream to start the list with.
lsExclusiveStartStreamName :: Lens' ListStreams (Maybe Text)
lsExclusiveStartStreamName =
    lens _lsExclusiveStartStreamName
        (\s a -> s { _lsExclusiveStartStreamName = a })

-- | The maximum number of streams to list.
lsLimit :: Lens' ListStreams (Maybe Natural)
lsLimit = lens _lsLimit (\s a -> s { _lsLimit = a })
    . mapping _Nat

data ListStreamsResponse = ListStreamsResponse
    { _lsrHasMoreStreams :: Bool
    , _lsrStreamNames    :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListStreamsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrHasMoreStreams' @::@ 'Bool'
--
-- * 'lsrStreamNames' @::@ ['Text']
--
listStreamsResponse :: Bool -- ^ 'lsrHasMoreStreams'
                    -> ListStreamsResponse
listStreamsResponse p1 = ListStreamsResponse
    { _lsrHasMoreStreams = p1
    , _lsrStreamNames    = mempty
    }

-- | If set to true, there are more streams available to list.
lsrHasMoreStreams :: Lens' ListStreamsResponse Bool
lsrHasMoreStreams =
    lens _lsrHasMoreStreams (\s a -> s { _lsrHasMoreStreams = a })

-- | The names of the streams that are associated with the AWS account making
-- the ListStreams request.
lsrStreamNames :: Lens' ListStreamsResponse [Text]
lsrStreamNames = lens _lsrStreamNames (\s a -> s { _lsrStreamNames = a })

instance ToPath ListStreams where
    toPath = const "/"

instance ToQuery ListStreams where
    toQuery = const mempty

instance ToHeaders ListStreams

instance ToJSON ListStreams where
    toJSON ListStreams{..} = object
        [ "Limit"                    .= _lsLimit
        , "ExclusiveStartStreamName" .= _lsExclusiveStartStreamName
        ]

instance AWSRequest ListStreams where
    type Sv ListStreams = Kinesis
    type Rs ListStreams = ListStreamsResponse

    request  = post "ListStreams"
    response = jsonResponse

instance FromJSON ListStreamsResponse where
    parseJSON = withObject "ListStreamsResponse" $ \o -> ListStreamsResponse
        <$> o .: "HasMoreStreams"
        <*> o .: "StreamNames"

instance AWSPager ListStreams where
    next rq rs
        | not (more (rs ^. lsrHasMoreStreams)) = Nothing
        | otherwise = Just $ rq
            & lsExclusiveStartStreamName .~ rs ^. index lsrStreamNames (to id)
