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

-- Module      : Network.AWS.CloudWatchLogs.CreateLogStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new log stream in the specified log group. The name of the log
-- stream must be unique within the log group. There is no limit on the number
-- of log streams that can exist in a log group. You must use the following
-- guidelines when naming a log stream: Log stream names can be between 1 and
-- 512 characters long. The ':' colon character is not allowed.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_CreateLogStream.html>
module Network.AWS.CloudWatchLogs.CreateLogStream
    (
    -- * Request
      CreateLogStream
    -- ** Request constructor
    , createLogStream
    -- ** Request lenses
    , clsLogGroupName
    , clsLogStreamName

    -- * Response
    , CreateLogStreamResponse
    -- ** Response constructor
    , createLogStreamResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudWatchLogs.Types
import qualified GHC.Exts

data CreateLogStream = CreateLogStream
    { _clsLogGroupName  :: Text
    , _clsLogStreamName :: Text
    } deriving (Eq, Ord, Show)

-- | 'CreateLogStream' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clsLogGroupName' @::@ 'Text'
--
-- * 'clsLogStreamName' @::@ 'Text'
--
createLogStream :: Text -- ^ 'clsLogGroupName'
                -> Text -- ^ 'clsLogStreamName'
                -> CreateLogStream
createLogStream p1 p2 = CreateLogStream
    { _clsLogGroupName  = p1
    , _clsLogStreamName = p2
    }

clsLogGroupName :: Lens' CreateLogStream Text
clsLogGroupName = lens _clsLogGroupName (\s a -> s { _clsLogGroupName = a })

clsLogStreamName :: Lens' CreateLogStream Text
clsLogStreamName = lens _clsLogStreamName (\s a -> s { _clsLogStreamName = a })

data CreateLogStreamResponse = CreateLogStreamResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateLogStreamResponse' constructor.
createLogStreamResponse :: CreateLogStreamResponse
createLogStreamResponse = CreateLogStreamResponse

instance ToPath CreateLogStream where
    toPath = const "/"

instance ToQuery CreateLogStream where
    toQuery = const mempty

instance ToHeaders CreateLogStream

instance ToJSON CreateLogStream where
    toJSON CreateLogStream{..} = object
        [ "logGroupName"  .= _clsLogGroupName
        , "logStreamName" .= _clsLogStreamName
        ]

instance AWSRequest CreateLogStream where
    type Sv CreateLogStream = CloudWatchLogs
    type Rs CreateLogStream = CreateLogStreamResponse

    request  = post "CreateLogStream"
    response = nullResponse CreateLogStreamResponse
