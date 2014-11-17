{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.GetEventSource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns configuration information for the specified event source mapping
-- (see AddEventSource). This operation requires permission for the
-- lambda:GetEventSource action.
--
-- <GetEventSource.html>
module Network.AWS.Lambda.GetEventSource
    (
    -- * Request
      GetEventSource
    -- ** Request constructor
    , getEventSource
    -- ** Request lenses
    , gesUUID

    -- * Response
    , GetEventSourceResponse
    -- ** Response constructor
    , getEventSourceResponse
    -- ** Response lenses
    , gesrBatchSize
    , gesrEventSource
    , gesrFunctionName
    , gesrIsActive
    , gesrLastModified
    , gesrParameters
    , gesrRole
    , gesrStatus
    , gesrUUID
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

newtype GetEventSource = GetEventSource
    { _gesUUID :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetEventSource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gesUUID' @::@ 'Text'
--
getEventSource :: Text -- ^ 'gesUUID'
               -> GetEventSource
getEventSource p1 = GetEventSource
    { _gesUUID = p1
    }

-- | The AWS Lambda assigned ID of the event source mapping.
gesUUID :: Lens' GetEventSource Text
gesUUID = lens _gesUUID (\s a -> s { _gesUUID = a })

data GetEventSourceResponse = GetEventSourceResponse
    { _gesrBatchSize    :: Maybe Int
    , _gesrEventSource  :: Maybe Text
    , _gesrFunctionName :: Maybe Text
    , _gesrIsActive     :: Maybe Bool
    , _gesrLastModified :: Maybe RFC822
    , _gesrParameters   :: Map Text Text
    , _gesrRole         :: Maybe Text
    , _gesrStatus       :: Maybe Text
    , _gesrUUID         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetEventSourceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gesrBatchSize' @::@ 'Maybe' 'Int'
--
-- * 'gesrEventSource' @::@ 'Maybe' 'Text'
--
-- * 'gesrFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'gesrIsActive' @::@ 'Maybe' 'Bool'
--
-- * 'gesrLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'gesrParameters' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'gesrRole' @::@ 'Maybe' 'Text'
--
-- * 'gesrStatus' @::@ 'Maybe' 'Text'
--
-- * 'gesrUUID' @::@ 'Maybe' 'Text'
--
getEventSourceResponse :: GetEventSourceResponse
getEventSourceResponse = GetEventSourceResponse
    { _gesrUUID         = Nothing
    , _gesrBatchSize    = Nothing
    , _gesrEventSource  = Nothing
    , _gesrFunctionName = Nothing
    , _gesrParameters   = mempty
    , _gesrRole         = Nothing
    , _gesrLastModified = Nothing
    , _gesrIsActive     = Nothing
    , _gesrStatus       = Nothing
    }

-- | The largest number of records that AWS Lambda will POST in the invocation
-- request to your function.
gesrBatchSize :: Lens' GetEventSourceResponse (Maybe Int)
gesrBatchSize = lens _gesrBatchSize (\s a -> s { _gesrBatchSize = a })

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
-- source of events.
gesrEventSource :: Lens' GetEventSourceResponse (Maybe Text)
gesrEventSource = lens _gesrEventSource (\s a -> s { _gesrEventSource = a })

-- | The Lambda function to invoke when AWS Lambda detects an event on the
-- stream.
gesrFunctionName :: Lens' GetEventSourceResponse (Maybe Text)
gesrFunctionName = lens _gesrFunctionName (\s a -> s { _gesrFunctionName = a })

-- | Indicates whether the event source mapping is currently honored. Events
-- are only processes if IsActive is true.
gesrIsActive :: Lens' GetEventSourceResponse (Maybe Bool)
gesrIsActive = lens _gesrIsActive (\s a -> s { _gesrIsActive = a })

-- | The UTC time string indicating the last time the event mapping was
-- updated.
gesrLastModified :: Lens' GetEventSourceResponse (Maybe UTCTime)
gesrLastModified = lens _gesrLastModified (\s a -> s { _gesrLastModified = a })
    . mapping _Time

-- | The map (key-value pairs) defining the configuration for AWS Lambda to
-- use when reading the event source.
gesrParameters :: Lens' GetEventSourceResponse (HashMap Text Text)
gesrParameters = lens _gesrParameters (\s a -> s { _gesrParameters = a })
    . _Map

-- | The ARN of the IAM role (invocation role) that AWS Lambda can assume to
-- read from the stream and invoke the function.
gesrRole :: Lens' GetEventSourceResponse (Maybe Text)
gesrRole = lens _gesrRole (\s a -> s { _gesrRole = a })

-- | The description of the health of the event source mapping. Valid values
-- are: "PENDING", "OK", and "PROBLEM:message". Initially this staus is
-- "PENDING". When AWS Lambda begins processing events, it changes the
-- status to "OK".
gesrStatus :: Lens' GetEventSourceResponse (Maybe Text)
gesrStatus = lens _gesrStatus (\s a -> s { _gesrStatus = a })

-- | The AWS Lambda assigned opaque identifier for the mapping.
gesrUUID :: Lens' GetEventSourceResponse (Maybe Text)
gesrUUID = lens _gesrUUID (\s a -> s { _gesrUUID = a })

instance AWSRequest GetEventSource where
    type Sv GetEventSource = Lambda
    type Rs GetEventSource = GetEventSourceResponse

    request  = get
    response = jsonResponse

instance FromJSON GetEventSourceResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath GetEventSource where
    toPath GetEventSource{..} = mconcat
        [ "/2014-11-13/event-source-mappings/"
        , toText _gesUUID
        ]

instance ToHeaders GetEventSource

instance ToQuery GetEventSource where
    toQuery = const mempty

instance ToJSON GetEventSource where
    toJSON = genericToJSON jsonOptions
