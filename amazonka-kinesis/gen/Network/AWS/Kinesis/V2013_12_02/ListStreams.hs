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
    , listStreams
    -- ** Request lenses
    , lsiLimit
    , lsiExclusiveStartStreamName

    -- * Response
    , ListStreamsResponse
    -- ** Response lenses
    , lsoHasMoreStreams
    , lsoStreamNames
    ) where

import           Network.AWS.Kinesis.V2013_12_02.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'ListStreams' request.
listStreams :: ListStreams
listStreams = ListStreams
    { _lsiLimit = Nothing
    , _lsiExclusiveStartStreamName = Nothing
    }
{-# INLINE listStreams #-}

data ListStreams = ListStreams
    { _lsiLimit :: Maybe Integer
      -- ^ The maximum number of streams to list.
    , _lsiExclusiveStartStreamName :: Maybe Text
      -- ^ The name of the stream to start the list with.
    } deriving (Show, Generic)

-- | The maximum number of streams to list.
lsiLimit :: Lens' ListStreams (Maybe Integer)
lsiLimit f x =
    f (_lsiLimit x)
        <&> \y -> x { _lsiLimit = y }
{-# INLINE lsiLimit #-}

-- | The name of the stream to start the list with.
lsiExclusiveStartStreamName :: Lens' ListStreams (Maybe Text)
lsiExclusiveStartStreamName f x =
    f (_lsiExclusiveStartStreamName x)
        <&> \y -> x { _lsiExclusiveStartStreamName = y }
{-# INLINE lsiExclusiveStartStreamName #-}

instance ToPath ListStreams

instance ToQuery ListStreams

instance ToHeaders ListStreams

instance ToJSON ListStreams

data ListStreamsResponse = ListStreamsResponse
    { _lsoHasMoreStreams :: Bool
      -- ^ If set to true, there are more streams available to list.
    , _lsoStreamNames :: [Text]
      -- ^ The names of the streams that are associated with the AWS account
      -- making the ListStreams request.
    } deriving (Show, Generic)

-- | If set to true, there are more streams available to list.
lsoHasMoreStreams :: Lens' ListStreamsResponse (Bool)
lsoHasMoreStreams f x =
    f (_lsoHasMoreStreams x)
        <&> \y -> x { _lsoHasMoreStreams = y }
{-# INLINE lsoHasMoreStreams #-}

-- | The names of the streams that are associated with the AWS account making
-- the ListStreams request.
lsoStreamNames :: Lens' ListStreamsResponse ([Text])
lsoStreamNames f x =
    f (_lsoStreamNames x)
        <&> \y -> x { _lsoStreamNames = y }
{-# INLINE lsoStreamNames #-}

instance FromJSON ListStreamsResponse

instance AWSRequest ListStreams where
    type Sv ListStreams = Kinesis
    type Rs ListStreams = ListStreamsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListStreams where
    next rq rs
        | not (_lsoHasMoreStreams rs) = Nothing
        | otherwise = Just $ rq
            { _lsiExclusiveStartStreamName = keyed id _lsoStreamNames rs
            }
