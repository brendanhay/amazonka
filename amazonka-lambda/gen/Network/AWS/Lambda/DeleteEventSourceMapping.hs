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

-- Module      : Network.AWS.Lambda.DeleteEventSourceMapping
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

-- | Removes an event source mapping. This means AWS Lambda will no longer invoke
-- the function for events in the associated source.
--
-- This operation requires permission for the 'lambda:DeleteEventSourceMapping'
-- action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_DeleteEventSourceMapping.html>
module Network.AWS.Lambda.DeleteEventSourceMapping
    (
    -- * Request
      DeleteEventSourceMapping
    -- ** Request constructor
    , deleteEventSourceMapping
    -- ** Request lenses
    , desmUUID

    -- * Response
    , DeleteEventSourceMappingResponse
    -- ** Response constructor
    , deleteEventSourceMappingResponse
    -- ** Response lenses
    , desmrBatchSize
    , desmrEventSourceArn
    , desmrFunctionArn
    , desmrLastModified
    , desmrLastProcessingResult
    , desmrState
    , desmrStateTransitionReason
    , desmrUUID
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

newtype DeleteEventSourceMapping = DeleteEventSourceMapping
    { _desmUUID :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteEventSourceMapping' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desmUUID' @::@ 'Text'
--
deleteEventSourceMapping :: Text -- ^ 'desmUUID'
                         -> DeleteEventSourceMapping
deleteEventSourceMapping p1 = DeleteEventSourceMapping
    { _desmUUID = p1
    }

-- | The event source mapping ID.
desmUUID :: Lens' DeleteEventSourceMapping Text
desmUUID = lens _desmUUID (\s a -> s { _desmUUID = a })

data DeleteEventSourceMappingResponse = DeleteEventSourceMappingResponse
    { _desmrBatchSize             :: Maybe Nat
    , _desmrEventSourceArn        :: Maybe Text
    , _desmrFunctionArn           :: Maybe Text
    , _desmrLastModified          :: Maybe POSIX
    , _desmrLastProcessingResult  :: Maybe Text
    , _desmrState                 :: Maybe Text
    , _desmrStateTransitionReason :: Maybe Text
    , _desmrUUID                  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteEventSourceMappingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desmrBatchSize' @::@ 'Maybe' 'Natural'
--
-- * 'desmrEventSourceArn' @::@ 'Maybe' 'Text'
--
-- * 'desmrFunctionArn' @::@ 'Maybe' 'Text'
--
-- * 'desmrLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'desmrLastProcessingResult' @::@ 'Maybe' 'Text'
--
-- * 'desmrState' @::@ 'Maybe' 'Text'
--
-- * 'desmrStateTransitionReason' @::@ 'Maybe' 'Text'
--
-- * 'desmrUUID' @::@ 'Maybe' 'Text'
--
deleteEventSourceMappingResponse :: DeleteEventSourceMappingResponse
deleteEventSourceMappingResponse = DeleteEventSourceMappingResponse
    { _desmrUUID                  = Nothing
    , _desmrBatchSize             = Nothing
    , _desmrEventSourceArn        = Nothing
    , _desmrFunctionArn           = Nothing
    , _desmrLastModified          = Nothing
    , _desmrLastProcessingResult  = Nothing
    , _desmrState                 = Nothing
    , _desmrStateTransitionReason = Nothing
    }

-- | The largest number of records that AWS Lambda will retrieve from your event
-- source at the time of invoking your function. Your function receives an event
-- with all the retrieved records.
desmrBatchSize :: Lens' DeleteEventSourceMappingResponse (Maybe Natural)
desmrBatchSize = lens _desmrBatchSize (\s a -> s { _desmrBatchSize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
-- source of events.
desmrEventSourceArn :: Lens' DeleteEventSourceMappingResponse (Maybe Text)
desmrEventSourceArn =
    lens _desmrEventSourceArn (\s a -> s { _desmrEventSourceArn = a })

-- | The Lambda function to invoke when AWS Lambda detects an event on the stream.
desmrFunctionArn :: Lens' DeleteEventSourceMappingResponse (Maybe Text)
desmrFunctionArn = lens _desmrFunctionArn (\s a -> s { _desmrFunctionArn = a })

-- | The UTC time string indicating the last time the event mapping was updated.
desmrLastModified :: Lens' DeleteEventSourceMappingResponse (Maybe UTCTime)
desmrLastModified =
    lens _desmrLastModified (\s a -> s { _desmrLastModified = a })
        . mapping _Time

-- | The result of the last AWS Lambda invocation of your Lambda function.
desmrLastProcessingResult :: Lens' DeleteEventSourceMappingResponse (Maybe Text)
desmrLastProcessingResult =
    lens _desmrLastProcessingResult
        (\s a -> s { _desmrLastProcessingResult = a })

-- | The state of the event source mapping. It can be "Creating", "Enabled",
-- "Disabled", "Enabling", "Disabling", "Updating", or "Deleting".
desmrState :: Lens' DeleteEventSourceMappingResponse (Maybe Text)
desmrState = lens _desmrState (\s a -> s { _desmrState = a })

-- | The reason the event source mapping is in its current state. It is either
-- user-requested or an AWS Lambda-initiated state transition.
desmrStateTransitionReason :: Lens' DeleteEventSourceMappingResponse (Maybe Text)
desmrStateTransitionReason =
    lens _desmrStateTransitionReason
        (\s a -> s { _desmrStateTransitionReason = a })

-- | The AWS Lambda assigned opaque identifier for the mapping.
desmrUUID :: Lens' DeleteEventSourceMappingResponse (Maybe Text)
desmrUUID = lens _desmrUUID (\s a -> s { _desmrUUID = a })

instance ToPath DeleteEventSourceMapping where
    toPath DeleteEventSourceMapping{..} = mconcat
        [ "/2015-03-31/event-source-mappings/"
        , toText _desmUUID
        ]

instance ToQuery DeleteEventSourceMapping where
    toQuery = const mempty

instance ToHeaders DeleteEventSourceMapping

instance ToJSON DeleteEventSourceMapping where
    toJSON = const (toJSON Empty)

instance AWSRequest DeleteEventSourceMapping where
    type Sv DeleteEventSourceMapping = Lambda
    type Rs DeleteEventSourceMapping = DeleteEventSourceMappingResponse

    request  = delete
    response = jsonResponse

instance FromJSON DeleteEventSourceMappingResponse where
    parseJSON = withObject "DeleteEventSourceMappingResponse" $ \o -> DeleteEventSourceMappingResponse
        <$> o .:? "BatchSize"
        <*> o .:? "EventSourceArn"
        <*> o .:? "FunctionArn"
        <*> o .:? "LastModified"
        <*> o .:? "LastProcessingResult"
        <*> o .:? "State"
        <*> o .:? "StateTransitionReason"
        <*> o .:? "UUID"
