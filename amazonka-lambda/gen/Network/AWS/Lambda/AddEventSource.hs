{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.AddEventSource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Identifies an Amazon Kinesis stream as the event source for an AWS Lambda
-- function. AWS Lambda invokes the specified function when records are posted
-- to the stream. This is the pull model, where AWS Lambda invokes the
-- function. For more information, go to AWS LambdaL How it Works in the AWS
-- Lambda Developer Guide. This association between an Amazon Kinesis stream
-- and an AWS Lambda function is called the event source mapping. You provide
-- the configuration information (for example, which stream to read from and
-- which AWS Lambda function to invoke) for the event source mapping in the
-- request body. This operation requires permission for the iam:PassRole
-- action for the IAM role. It also requires permission for the
-- lambda:AddEventSource action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_AddEventSource.html>
module Network.AWS.Lambda.AddEventSource
    (
    -- * Request
      AddEventSource
    -- ** Request constructor
    , addEventSource
    -- ** Request lenses
    , aesBatchSize
    , aesEventSource
    , aesFunctionName
    , aesParameters
    , aesRole

    -- * Response
    , AddEventSourceResponse
    -- ** Response constructor
    , addEventSourceResponse
    -- ** Response lenses
    , aesrBatchSize
    , aesrEventSource
    , aesrFunctionName
    , aesrIsActive
    , aesrLastModified
    , aesrParameters
    , aesrRole
    , aesrStatus
    , aesrUUID
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data AddEventSource = AddEventSource
    { _aesBatchSize    :: Maybe Int
    , _aesEventSource  :: Text
    , _aesFunctionName :: Text
    , _aesParameters   :: Map Text Text
    , _aesRole         :: Text
    } deriving (Eq, Show, Generic)

-- | 'AddEventSource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aesBatchSize' @::@ 'Maybe' 'Int'
--
-- * 'aesEventSource' @::@ 'Text'
--
-- * 'aesFunctionName' @::@ 'Text'
--
-- * 'aesParameters' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'aesRole' @::@ 'Text'
--
addEventSource :: Text -- ^ 'aesEventSource'
               -> Text -- ^ 'aesFunctionName'
               -> Text -- ^ 'aesRole'
               -> AddEventSource
addEventSource p1 p2 p3 = AddEventSource
    { _aesEventSource  = p1
    , _aesFunctionName = p2
    , _aesRole         = p3
    , _aesBatchSize    = Nothing
    , _aesParameters   = mempty
    }

-- | The largest number of records that AWS Lambda will give to your function
-- in a single event. The default is 100 records.
aesBatchSize :: Lens' AddEventSource (Maybe Int)
aesBatchSize = lens _aesBatchSize (\s a -> s { _aesBatchSize = a })

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
-- event source. Any record added to this stream causes AWS Lambda to invoke
-- your Lambda function. AWS Lambda POSTs the Amazon Kinesis event,
-- containing records, to your Lambda function as JSON.
aesEventSource :: Lens' AddEventSource Text
aesEventSource = lens _aesEventSource (\s a -> s { _aesEventSource = a })

-- | The Lambda function to invoke when AWS Lambda detects an event on the
-- stream.
aesFunctionName :: Lens' AddEventSource Text
aesFunctionName = lens _aesFunctionName (\s a -> s { _aesFunctionName = a })

-- | A map (key-value pairs) defining the configuration for AWS Lambda to use
-- when reading the event source. Currently, AWS Lambda supports only the
-- InitialPositionInStream key. The valid values are: "TRIM_HORIZON" and
-- "LATEST". The default value is "TRIM_HORIZON". For more information, go
-- to ShardIteratorType in the Amazon Kinesis Service API Reference.
aesParameters :: Lens' AddEventSource (HashMap Text Text)
aesParameters = lens _aesParameters (\s a -> s { _aesParameters = a })
    . _Map

-- | The ARN of the IAM role (invocation role) that AWS Lambda can assume to
-- read from the stream and invoke the function.
aesRole :: Lens' AddEventSource Text
aesRole = lens _aesRole (\s a -> s { _aesRole = a })

data AddEventSourceResponse = AddEventSourceResponse
    { _aesrBatchSize    :: Maybe Int
    , _aesrEventSource  :: Maybe Text
    , _aesrFunctionName :: Maybe Text
    , _aesrIsActive     :: Maybe Bool
    , _aesrLastModified :: Maybe RFC822
    , _aesrParameters   :: Map Text Text
    , _aesrRole         :: Maybe Text
    , _aesrStatus       :: Maybe Text
    , _aesrUUID         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'AddEventSourceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aesrBatchSize' @::@ 'Maybe' 'Int'
--
-- * 'aesrEventSource' @::@ 'Maybe' 'Text'
--
-- * 'aesrFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'aesrIsActive' @::@ 'Maybe' 'Bool'
--
-- * 'aesrLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'aesrParameters' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'aesrRole' @::@ 'Maybe' 'Text'
--
-- * 'aesrStatus' @::@ 'Maybe' 'Text'
--
-- * 'aesrUUID' @::@ 'Maybe' 'Text'
--
addEventSourceResponse :: AddEventSourceResponse
addEventSourceResponse = AddEventSourceResponse
    { _aesrUUID         = Nothing
    , _aesrBatchSize    = Nothing
    , _aesrEventSource  = Nothing
    , _aesrFunctionName = Nothing
    , _aesrParameters   = mempty
    , _aesrRole         = Nothing
    , _aesrLastModified = Nothing
    , _aesrIsActive     = Nothing
    , _aesrStatus       = Nothing
    }

-- | The largest number of records that AWS Lambda will POST in the invocation
-- request to your function.
aesrBatchSize :: Lens' AddEventSourceResponse (Maybe Int)
aesrBatchSize = lens _aesrBatchSize (\s a -> s { _aesrBatchSize = a })

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
-- source of events.
aesrEventSource :: Lens' AddEventSourceResponse (Maybe Text)
aesrEventSource = lens _aesrEventSource (\s a -> s { _aesrEventSource = a })

-- | The Lambda function to invoke when AWS Lambda detects an event on the
-- stream.
aesrFunctionName :: Lens' AddEventSourceResponse (Maybe Text)
aesrFunctionName = lens _aesrFunctionName (\s a -> s { _aesrFunctionName = a })

-- | Indicates whether the event source mapping is currently honored. Events
-- are only processes if IsActive is true.
aesrIsActive :: Lens' AddEventSourceResponse (Maybe Bool)
aesrIsActive = lens _aesrIsActive (\s a -> s { _aesrIsActive = a })

-- | The UTC time string indicating the last time the event mapping was
-- updated.
aesrLastModified :: Lens' AddEventSourceResponse (Maybe UTCTime)
aesrLastModified = lens _aesrLastModified (\s a -> s { _aesrLastModified = a })
    . mapping _Time

-- | The map (key-value pairs) defining the configuration for AWS Lambda to
-- use when reading the event source.
aesrParameters :: Lens' AddEventSourceResponse (HashMap Text Text)
aesrParameters = lens _aesrParameters (\s a -> s { _aesrParameters = a })
    . _Map

-- | The ARN of the IAM role (invocation role) that AWS Lambda can assume to
-- read from the stream and invoke the function.
aesrRole :: Lens' AddEventSourceResponse (Maybe Text)
aesrRole = lens _aesrRole (\s a -> s { _aesrRole = a })

-- | The description of the health of the event source mapping. Valid values
-- are: "PENDING", "OK", and "PROBLEM:message". Initially this staus is
-- "PENDING". When AWS Lambda begins processing events, it changes the
-- status to "OK".
aesrStatus :: Lens' AddEventSourceResponse (Maybe Text)
aesrStatus = lens _aesrStatus (\s a -> s { _aesrStatus = a })

-- | The AWS Lambda assigned opaque identifier for the mapping.
aesrUUID :: Lens' AddEventSourceResponse (Maybe Text)
aesrUUID = lens _aesrUUID (\s a -> s { _aesrUUID = a })

instance ToPath AddEventSource where
    toPath = const "/2014-11-13/event-source-mappings/"

instance ToQuery AddEventSource where
    toQuery = const mempty

instance ToHeaders AddEventSource
instance ToJSON AddEventSource where
    toJSON = genericToJSON jsonOptions

instance AWSRequest AddEventSource where
    type Sv AddEventSource = Lambda
    type Rs AddEventSource = AddEventSourceResponse

    request  = post
    response = jsonResponse

instance FromJSON AddEventSourceResponse where
    parseJSON = genericParseJSON jsonOptions
