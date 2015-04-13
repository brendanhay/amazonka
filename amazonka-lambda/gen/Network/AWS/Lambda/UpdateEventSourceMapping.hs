{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.UpdateEventSourceMapping
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | You can update an event source mapping. This is useful if you want to change
-- the parameters of the existing mapping without losing your position in the
-- stream. You can change which function will receive the stream records, but to
-- change the stream itself, you must create a new mapping.
--
-- This operation requires permission for the 'lambda:UpdateEventSourceMapping'
-- action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateEventSourceMapping.html>
module Network.AWS.Lambda.UpdateEventSourceMapping
    (
    -- * Request
      UpdateEventSourceMapping
    -- ** Request constructor
    , updateEventSourceMapping
    -- ** Request lenses
    , uesmBatchSize
    , uesmEnabled
    , uesmFunctionName
    , uesmUUID

    -- * Response
    , UpdateEventSourceMappingResponse
    -- ** Response constructor
    , updateEventSourceMappingResponse
    -- ** Response lenses
    , uesmrBatchSize
    , uesmrEventSourceArn
    , uesmrFunctionArn
    , uesmrLastModified
    , uesmrLastProcessingResult
    , uesmrState
    , uesmrStateTransitionReason
    , uesmrUUID
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data UpdateEventSourceMapping = UpdateEventSourceMapping
    { _uesmBatchSize    :: Maybe Nat
    , _uesmEnabled      :: Maybe Bool
    , _uesmFunctionName :: Maybe Text
    , _uesmUUID         :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UpdateEventSourceMapping' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uesmBatchSize' @::@ 'Maybe' 'Natural'
--
-- * 'uesmEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'uesmFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'uesmUUID' @::@ 'Text'
--
updateEventSourceMapping :: Text -- ^ 'uesmUUID'
                         -> UpdateEventSourceMapping
updateEventSourceMapping p1 = UpdateEventSourceMapping
    { _uesmUUID         = p1
    , _uesmFunctionName = Nothing
    , _uesmEnabled      = Nothing
    , _uesmBatchSize    = Nothing
    }

-- | The maximum number of stream records that can be sent to your Lambda function
-- for a single invocation.
uesmBatchSize :: Lens' UpdateEventSourceMapping (Maybe Natural)
uesmBatchSize = lens _uesmBatchSize (\s a -> s { _uesmBatchSize = a }) . mapping _Nat

-- | Specifies whether AWS Lambda should actively poll the stream or not. If
-- disabled, AWS Lambda will not poll the stream.
uesmEnabled :: Lens' UpdateEventSourceMapping (Maybe Bool)
uesmEnabled = lens _uesmEnabled (\s a -> s { _uesmEnabled = a })

-- | The Lambda function to which you want the stream records sent.
--
-- You can specify an unqualified function name (for example, "Thumbnail") or
-- you can specify Amazon Resource Name (ARN) of the function (for example,
-- "arn:aws:lambda:us-west-2:account-id:function:ThumbNail"). AWS Lambda also
-- allows you to specify only the account ID qualifier (for example,
-- "account-id:Thumbnail"). Note that the length constraint applies only to the
-- ARN. If you specify only the function name, it is limited to 64 character in
-- length.
uesmFunctionName :: Lens' UpdateEventSourceMapping (Maybe Text)
uesmFunctionName = lens _uesmFunctionName (\s a -> s { _uesmFunctionName = a })

-- | The event source mapping identifier.
uesmUUID :: Lens' UpdateEventSourceMapping Text
uesmUUID = lens _uesmUUID (\s a -> s { _uesmUUID = a })

data UpdateEventSourceMappingResponse = UpdateEventSourceMappingResponse
    { _uesmrBatchSize             :: Maybe Nat
    , _uesmrEventSourceArn        :: Maybe Text
    , _uesmrFunctionArn           :: Maybe Text
    , _uesmrLastModified          :: Maybe POSIX
    , _uesmrLastProcessingResult  :: Maybe Text
    , _uesmrState                 :: Maybe Text
    , _uesmrStateTransitionReason :: Maybe Text
    , _uesmrUUID                  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UpdateEventSourceMappingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uesmrBatchSize' @::@ 'Maybe' 'Natural'
--
-- * 'uesmrEventSourceArn' @::@ 'Maybe' 'Text'
--
-- * 'uesmrFunctionArn' @::@ 'Maybe' 'Text'
--
-- * 'uesmrLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'uesmrLastProcessingResult' @::@ 'Maybe' 'Text'
--
-- * 'uesmrState' @::@ 'Maybe' 'Text'
--
-- * 'uesmrStateTransitionReason' @::@ 'Maybe' 'Text'
--
-- * 'uesmrUUID' @::@ 'Maybe' 'Text'
--
updateEventSourceMappingResponse :: UpdateEventSourceMappingResponse
updateEventSourceMappingResponse = UpdateEventSourceMappingResponse
    { _uesmrUUID                  = Nothing
    , _uesmrBatchSize             = Nothing
    , _uesmrEventSourceArn        = Nothing
    , _uesmrFunctionArn           = Nothing
    , _uesmrLastModified          = Nothing
    , _uesmrLastProcessingResult  = Nothing
    , _uesmrState                 = Nothing
    , _uesmrStateTransitionReason = Nothing
    }

-- | The largest number of records that AWS Lambda will retrieve from your event
-- source at the time of invoking your function. Your function receives an event
-- with all the retrieved records.
uesmrBatchSize :: Lens' UpdateEventSourceMappingResponse (Maybe Natural)
uesmrBatchSize = lens _uesmrBatchSize (\s a -> s { _uesmrBatchSize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
-- source of events.
uesmrEventSourceArn :: Lens' UpdateEventSourceMappingResponse (Maybe Text)
uesmrEventSourceArn =
    lens _uesmrEventSourceArn (\s a -> s { _uesmrEventSourceArn = a })

-- | The Lambda function to invoke when AWS Lambda detects an event on the stream.
uesmrFunctionArn :: Lens' UpdateEventSourceMappingResponse (Maybe Text)
uesmrFunctionArn = lens _uesmrFunctionArn (\s a -> s { _uesmrFunctionArn = a })

-- | The UTC time string indicating the last time the event mapping was updated.
uesmrLastModified :: Lens' UpdateEventSourceMappingResponse (Maybe UTCTime)
uesmrLastModified =
    lens _uesmrLastModified (\s a -> s { _uesmrLastModified = a })
        . mapping _Time

-- | The result of the last AWS Lambda invocation of your Lambda function.
uesmrLastProcessingResult :: Lens' UpdateEventSourceMappingResponse (Maybe Text)
uesmrLastProcessingResult =
    lens _uesmrLastProcessingResult
        (\s a -> s { _uesmrLastProcessingResult = a })

-- | The state of the event source mapping. It can be "Creating", "Enabled",
-- "Disabled", "Enabling", "Disabling", "Updating", or "Deleting".
uesmrState :: Lens' UpdateEventSourceMappingResponse (Maybe Text)
uesmrState = lens _uesmrState (\s a -> s { _uesmrState = a })

-- | The reason the event source mapping is in its current state. It is either
-- user-requested or an AWS Lambda-initiated state transition.
uesmrStateTransitionReason :: Lens' UpdateEventSourceMappingResponse (Maybe Text)
uesmrStateTransitionReason =
    lens _uesmrStateTransitionReason
        (\s a -> s { _uesmrStateTransitionReason = a })

-- | The AWS Lambda assigned opaque identifier for the mapping.
uesmrUUID :: Lens' UpdateEventSourceMappingResponse (Maybe Text)
uesmrUUID = lens _uesmrUUID (\s a -> s { _uesmrUUID = a })

instance ToPath UpdateEventSourceMapping where
    toPath UpdateEventSourceMapping{..} = mconcat
        [ "/2015-03-31/event-source-mappings/"
        , toText _uesmUUID
        ]

instance ToQuery UpdateEventSourceMapping where
    toQuery = const mempty

instance ToHeaders UpdateEventSourceMapping

instance ToJSON UpdateEventSourceMapping where
    toJSON UpdateEventSourceMapping{..} = object
        [ "FunctionName" .= _uesmFunctionName
        , "Enabled"      .= _uesmEnabled
        , "BatchSize"    .= _uesmBatchSize
        ]

instance AWSRequest UpdateEventSourceMapping where
    type Sv UpdateEventSourceMapping = Lambda
    type Rs UpdateEventSourceMapping = UpdateEventSourceMappingResponse

    request  = put
    response = jsonResponse

instance FromJSON UpdateEventSourceMappingResponse where
    parseJSON = withObject "UpdateEventSourceMappingResponse" $ \o -> UpdateEventSourceMappingResponse
        <$> o .:? "BatchSize"
        <*> o .:? "EventSourceArn"
        <*> o .:? "FunctionArn"
        <*> o .:? "LastModified"
        <*> o .:? "LastProcessingResult"
        <*> o .:? "State"
        <*> o .:? "StateTransitionReason"
        <*> o .:? "UUID"
