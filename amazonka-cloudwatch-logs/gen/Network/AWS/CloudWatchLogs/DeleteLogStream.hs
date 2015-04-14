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

-- Module      : Network.AWS.CloudWatchLogs.DeleteLogStream
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

-- | Deletes a log stream and permanently deletes all the archived log events
-- associated with it.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteLogStream.html>
module Network.AWS.CloudWatchLogs.DeleteLogStream
    (
    -- * Request
      DeleteLogStream
    -- ** Request constructor
    , deleteLogStream
    -- ** Request lenses
    , dlsLogGroupName
    , dlsLogStreamName

    -- * Response
    , DeleteLogStreamResponse
    -- ** Response constructor
    , deleteLogStreamResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudWatchLogs.Types
import qualified GHC.Exts

data DeleteLogStream = DeleteLogStream
    { _dlsLogGroupName  :: Text
    , _dlsLogStreamName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteLogStream' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlsLogGroupName' @::@ 'Text'
--
-- * 'dlsLogStreamName' @::@ 'Text'
--
deleteLogStream :: Text -- ^ 'dlsLogGroupName'
                -> Text -- ^ 'dlsLogStreamName'
                -> DeleteLogStream
deleteLogStream p1 p2 = DeleteLogStream
    { _dlsLogGroupName  = p1
    , _dlsLogStreamName = p2
    }

dlsLogGroupName :: Lens' DeleteLogStream Text
dlsLogGroupName = lens _dlsLogGroupName (\s a -> s { _dlsLogGroupName = a })

dlsLogStreamName :: Lens' DeleteLogStream Text
dlsLogStreamName = lens _dlsLogStreamName (\s a -> s { _dlsLogStreamName = a })

data DeleteLogStreamResponse = DeleteLogStreamResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteLogStreamResponse' constructor.
deleteLogStreamResponse :: DeleteLogStreamResponse
deleteLogStreamResponse = DeleteLogStreamResponse

instance ToPath DeleteLogStream where
    toPath = const "/"

instance ToQuery DeleteLogStream where
    toQuery = const mempty

instance ToHeaders DeleteLogStream

instance ToJSON DeleteLogStream where
    toJSON DeleteLogStream{..} = object
        [ "logGroupName"  .= _dlsLogGroupName
        , "logStreamName" .= _dlsLogStreamName
        ]

instance AWSRequest DeleteLogStream where
    type Sv DeleteLogStream = CloudWatchLogs
    type Rs DeleteLogStream = DeleteLogStreamResponse

    request  = post "DeleteLogStream"
    response = nullResponse DeleteLogStreamResponse
