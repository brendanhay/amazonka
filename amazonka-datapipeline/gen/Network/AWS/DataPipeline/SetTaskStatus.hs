{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.SetTaskStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Notifies AWS Data Pipeline that a task is completed and provides
-- information about the final status. The task runner calls this action
-- regardless of whether the task was sucessful. The task runner does not need
-- to call SetTaskStatus for tasks that are canceled by the web service during
-- a call to ReportTaskProgress.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_SetTaskStatus.html>
module Network.AWS.DataPipeline.SetTaskStatus
    (
    -- * Request
      SetTaskStatus
    -- ** Request constructor
    , setTaskStatus
    -- ** Request lenses
    , stsErrorId
    , stsErrorMessage
    , stsErrorStackTrace
    , stsTaskId
    , stsTaskStatus

    -- * Response
    , SetTaskStatusResponse
    -- ** Response constructor
    , setTaskStatusResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data SetTaskStatus = SetTaskStatus
    { _stsErrorId         :: Maybe Text
    , _stsErrorMessage    :: Maybe Text
    , _stsErrorStackTrace :: Maybe Text
    , _stsTaskId          :: Text
    , _stsTaskStatus      :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetTaskStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stsErrorId' @::@ 'Maybe' 'Text'
--
-- * 'stsErrorMessage' @::@ 'Maybe' 'Text'
--
-- * 'stsErrorStackTrace' @::@ 'Maybe' 'Text'
--
-- * 'stsTaskId' @::@ 'Text'
--
-- * 'stsTaskStatus' @::@ 'Text'
--
setTaskStatus :: Text -- ^ 'stsTaskId'
              -> Text -- ^ 'stsTaskStatus'
              -> SetTaskStatus
setTaskStatus p1 p2 = SetTaskStatus
    { _stsTaskId          = p1
    , _stsTaskStatus      = p2
    , _stsErrorId         = Nothing
    , _stsErrorMessage    = Nothing
    , _stsErrorStackTrace = Nothing
    }

-- | If an error occurred during the task, this value specifies an id value
-- that represents the error. This value is set on the physical attempt
-- object. It is used to display error information to the user. It should
-- not start with string "Service_" which is reserved by the system.
stsErrorId :: Lens' SetTaskStatus (Maybe Text)
stsErrorId = lens _stsErrorId (\s a -> s { _stsErrorId = a })

-- | If an error occurred during the task, this value specifies a text
-- description of the error. This value is set on the physical attempt
-- object. It is used to display error information to the user. The web
-- service does not parse this value.
stsErrorMessage :: Lens' SetTaskStatus (Maybe Text)
stsErrorMessage = lens _stsErrorMessage (\s a -> s { _stsErrorMessage = a })

-- | If an error occurred during the task, this value specifies the stack
-- trace associated with the error. This value is set on the physical
-- attempt object. It is used to display error information to the user. The
-- web service does not parse this value.
stsErrorStackTrace :: Lens' SetTaskStatus (Maybe Text)
stsErrorStackTrace =
    lens _stsErrorStackTrace (\s a -> s { _stsErrorStackTrace = a })

-- | Identifies the task assigned to the task runner. This value is set in the
-- TaskObject that is returned by the PollForTask action.
stsTaskId :: Lens' SetTaskStatus Text
stsTaskId = lens _stsTaskId (\s a -> s { _stsTaskId = a })

-- | If FINISHED, the task successfully completed. If FAILED the task ended
-- unsuccessfully. The FALSE value is used by preconditions.
stsTaskStatus :: Lens' SetTaskStatus Text
stsTaskStatus = lens _stsTaskStatus (\s a -> s { _stsTaskStatus = a })

data SetTaskStatusResponse = SetTaskStatusResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetTaskStatusResponse' constructor.
setTaskStatusResponse :: SetTaskStatusResponse
setTaskStatusResponse = SetTaskStatusResponse

instance AWSRequest SetTaskStatus where
    type Sv SetTaskStatus = DataPipeline
    type Rs SetTaskStatus = SetTaskStatusResponse

    request  = post
    response = nullResponse SetTaskStatusResponse

instance ToPath SetTaskStatus where
    toPath = const "/"

instance ToHeaders SetTaskStatus

instance ToQuery SetTaskStatus where
    toQuery = const mempty

instance ToJSON SetTaskStatus where
    toJSON = genericToJSON jsonOptions
