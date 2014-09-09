{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.V2013_12_02.ListStreams
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns an array of the names of all the streams that are
-- associated with the AWS account making the ListStreams request. A given AWS
-- account can have many streams active at one time. The number of streams may
-- be too large to return from a single call to ListStreams. You can limit the
-- number of returned streams using the Limit parameter. If you do not specify
-- a value for the Limit parameter, Amazon Kinesis uses the default limit,
-- which is currently 10. You can detect if there are more streams available
-- to list by using the HasMoreStreams flag from the returned output. If there
-- are more streams available, you can request more streams by using the name
-- of the last stream returned by the ListStreams request in the
-- ExclusiveStartStreamName parameter in a subsequent request to ListStreams.
-- The group of stream names returned by the subsequent request is then added
-- to the list. You can continue this process until all the stream names have
-- been collected in the list. ListStreams has a limit of 5 transactions per
-- second per account. List the Streams for an AWS Account The following is an
-- example of an Amazon Kinesis ListStreams request and response. POST /
-- HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.ListStreams HTTP/1.1 200 OK x-amzn-RequestId:
-- Content-Type: application/x-amz-json-1.1 Content-Length: Date: ]]> {
-- "HasMoreStreams": false, "StreamNames": [ "exampleStreamName" ] }.
module Network.AWS.Kinesis.V2013_12_02.ListStreams
    (
    -- * Request
      ListStreams
    -- ** Request constructor
    , mkListStreams
    -- ** Request lenses
    , lsLimit
    , lsExclusiveStartStreamName

    -- * Response
    , ListStreamsResponse
    -- ** Response constructor
    , mkListStreamsResponse
    -- ** Response lenses
    , lsrStreamNames
    , lsrHasMoreStreams
    ) where

import Network.AWS.Kinesis.V2013_12_02.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of a ListStreams operation.
data ListStreams = ListStreams
    { _lsLimit :: Maybe Integer
    , _lsExclusiveStartStreamName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListStreams' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Limit ::@ @Maybe Integer@
--
-- * @ExclusiveStartStreamName ::@ @Maybe Text@
--
mkListStreams :: ListStreams
mkListStreams = ListStreams
    { _lsLimit = Nothing
    , _lsExclusiveStartStreamName = Nothing
    }

-- | The maximum number of streams to list.
lsLimit :: Lens' ListStreams (Maybe Integer)
lsLimit = lens _lsLimit (\s a -> s { _lsLimit = a })

-- | The name of the stream to start the list with.
lsExclusiveStartStreamName :: Lens' ListStreams (Maybe Text)
lsExclusiveStartStreamName =
    lens _lsExclusiveStartStreamName
         (\s a -> s { _lsExclusiveStartStreamName = a })

instance ToPath ListStreams

instance ToQuery ListStreams

instance ToHeaders ListStreams

instance ToJSON ListStreams

-- | Represents the output of a ListStreams operation.
data ListStreamsResponse = ListStreamsResponse
    { _lsrStreamNames :: [Text]
    , _lsrHasMoreStreams :: Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListStreamsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamNames ::@ @[Text]@
--
-- * @HasMoreStreams ::@ @Bool@
--
mkListStreamsResponse :: [Text] -- ^ 'lsrStreamNames'
                      -> Bool -- ^ 'lsrHasMoreStreams'
                      -> ListStreamsResponse
mkListStreamsResponse p1 p2 = ListStreamsResponse
    { _lsrStreamNames = p1
    , _lsrHasMoreStreams = p2
    }

-- | The names of the streams that are associated with the AWS account making
-- the ListStreams request.
lsrStreamNames :: Lens' ListStreamsResponse [Text]
lsrStreamNames = lens _lsrStreamNames (\s a -> s { _lsrStreamNames = a })

-- | If set to true, there are more streams available to list.
lsrHasMoreStreams :: Lens' ListStreamsResponse Bool
lsrHasMoreStreams =
    lens _lsrHasMoreStreams (\s a -> s { _lsrHasMoreStreams = a })

instance FromJSON ListStreamsResponse

instance AWSRequest ListStreams where
    type Sv ListStreams = Kinesis
    type Rs ListStreams = ListStreamsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListStreams where
    next rq rs
        | not (rs ^. lsrHasMoreStreams) = Nothing
        | otherwise = Just $
            rq & lsExclusiveStartStreamName .~ index lsrStreamNames (to id) rs
