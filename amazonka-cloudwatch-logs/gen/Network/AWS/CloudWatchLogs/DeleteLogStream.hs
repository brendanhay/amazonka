{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.CloudWatchLogs.DeleteLogStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a log stream and permanently deletes all the archived log events
-- associated with it.
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

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudWatchLogs.Types

data DeleteLogStream = DeleteLogStream
    { _dlsLogGroupName  :: Text
    , _dlsLogStreamName :: Text
    } deriving (Eq, Ord, Show, Generic)

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

instance ToPath DeleteLogStream where
    toPath = const "/"

instance ToQuery DeleteLogStream where
    toQuery = const mempty

instance ToHeaders DeleteLogStream

instance ToBody DeleteLogStream where
    toBody = toBody . encode . _dlsLogGroupName

data DeleteLogStreamResponse = DeleteLogStreamResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLogStreamResponse' constructor.
deleteLogStreamResponse :: DeleteLogStreamResponse
deleteLogStreamResponse = DeleteLogStreamResponse

-- FromJSON

instance AWSRequest DeleteLogStream where
    type Sv DeleteLogStream = CloudWatchLogs
    type Rs DeleteLogStream = DeleteLogStreamResponse

    request  = post'
    response = nullaryResponse DeleteLogStreamResponse
