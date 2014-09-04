{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.SetTaskStatus
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
-- a call to ReportTaskProgress. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.SetTaskStatus
-- Content-Length: 847 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"taskId":
-- "aaGgHT4LuH0T0Y0oLrJRjas5qH0d8cDPADxqq3tn+zCWGELkCdV2JprLreXm1oxeP5EFZHFLJ69kjSsLYE0iYHYBYVGBrB+E/pYq7ANEEeGJFnSBMRiXZVA+8UJ3OzcInvXeinqBmBaKwii7hnnKb/AXjXiNTXyxgydX1KAyg1AxkwBYG4cfPYMZbuEbQJFJvv5C/2+GVXz1w94nKYTeUeepwUOFOuRLS6JVtZoYwpF56E+Yfk1IcGpFOvCZ01B4Bkuu7x3J+MD/j6kJgZLAgbCJQtI3eiW3kdGmX0p0I2BdY1ZsX6b4UiSvM3OMj6NEHJCJL4E0ZfitnhCoe24Kvjo6C2hFbZq+ei/HPgSXBQMSagkr4vS9c0ChzxH2+LNYvec6bY4kymkaZI1dvOzmpa0FcnGf5AjSK4GpsViZ/ujz6zxFv81qBXzjF0/4M1775rjV1VUdyKaixiA/sJiACNezqZqETidp8d24BDPRhGsj6pBCrnelqGFrk/gXEXUsJ+xwMifRC8UVwiKekpAvHUywVk7Ku4jH/n3i2VoLRP6FXwpUbelu34iiZ9czpXyLtyPKwxa87dlrnRVURwkcVjOt2Mcrcaqe+cbWHvNRhyrPkkdfSF3ac8/wfgVbXvLEB2k9mKc67aD9rvdc1PKX09Tk8BKklsMTpZ3TRCd4NzQlJKigMe8Jat9+1tKj4Ole5ZzW6uyTu2s2iFjEV8KXu4MaiRJyNKCdKeGhhZWY37Qk4NBK4Ppgu+C6Y41dpfOh288SLDEVx0/UySlqOEdhba7c6BiPp5r3hKj3mk9lFy5OYp1aoGLeeFmjXveTnPdf2gkWqXXg7AUbJ7jEs1F0lKZQg4szep2gcKyAJXgvXLfJJHcha8Lfb/Ee7wYmyOcAaRpDBoFNSbtoVXar46teIrpho+ZDvynUXvU0grHWGOk=:wn3SgymHZM99bEXAMPLE",
-- "taskStatus": "FINISHED"} x-amzn-RequestId:
-- 8c8deb53-0788-11e2-af9c-6bc7a6be6qr8 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {}.
module Network.AWS.DataPipeline.V2012_10_29.SetTaskStatus
    (
    -- * Request
      SetTaskStatus
    -- ** Request constructor
    , setTaskStatus
    -- ** Request lenses
    , stsiTaskId
    , stsiTaskStatus
    , stsiErrorMessage
    , stsiErrorId
    , stsiErrorStackTrace

    -- * Response
    , SetTaskStatusResponse
    ) where

import           Network.AWS.DataPipeline.V2012_10_29.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'SetTaskStatus' request.
setTaskStatus :: Text -- ^ 'stsiTaskId'
              -> TaskStatus -- ^ 'stsiTaskStatus'
              -> SetTaskStatus
setTaskStatus p1 p2 = SetTaskStatus
    { _stsiTaskId = p1
    , _stsiTaskStatus = p2
    , _stsiErrorMessage = Nothing
    , _stsiErrorId = Nothing
    , _stsiErrorStackTrace = Nothing
    }
{-# INLINE setTaskStatus #-}

data SetTaskStatus = SetTaskStatus
    { _stsiTaskId :: Text
      -- ^ Identifies the task assigned to the task runner. This value is
      -- set in the TaskObject that is returned by the PollForTask action.
    , _stsiTaskStatus :: TaskStatus
      -- ^ If FINISHED, the task successfully completed. If FAILED the task
      -- ended unsuccessfully. The FALSE value is used by preconditions.
    , _stsiErrorMessage :: Maybe Text
      -- ^ If an error occurred during the task, this value specifies a text
      -- description of the error. This value is set on the physical
      -- attempt object. It is used to display error information to the
      -- user. The web service does not parse this value.
    , _stsiErrorId :: Maybe Text
      -- ^ If an error occurred during the task, this value specifies an id
      -- value that represents the error. This value is set on the
      -- physical attempt object. It is used to display error information
      -- to the user. It should not start with string "Service_" which is
      -- reserved by the system.
    , _stsiErrorStackTrace :: Maybe Text
      -- ^ If an error occurred during the task, this value specifies the
      -- stack trace associated with the error. This value is set on the
      -- physical attempt object. It is used to display error information
      -- to the user. The web service does not parse this value.
    } deriving (Show, Generic)

-- | Identifies the task assigned to the task runner. This value is set in the
-- TaskObject that is returned by the PollForTask action.
stsiTaskId :: Lens' SetTaskStatus (Text)
stsiTaskId f x =
    f (_stsiTaskId x)
        <&> \y -> x { _stsiTaskId = y }
{-# INLINE stsiTaskId #-}

-- | If FINISHED, the task successfully completed. If FAILED the task ended
-- unsuccessfully. The FALSE value is used by preconditions.
stsiTaskStatus :: Lens' SetTaskStatus (TaskStatus)
stsiTaskStatus f x =
    f (_stsiTaskStatus x)
        <&> \y -> x { _stsiTaskStatus = y }
{-# INLINE stsiTaskStatus #-}

-- | If an error occurred during the task, this value specifies a text
-- description of the error. This value is set on the physical attempt object.
-- It is used to display error information to the user. The web service does
-- not parse this value.
stsiErrorMessage :: Lens' SetTaskStatus (Maybe Text)
stsiErrorMessage f x =
    f (_stsiErrorMessage x)
        <&> \y -> x { _stsiErrorMessage = y }
{-# INLINE stsiErrorMessage #-}

-- | If an error occurred during the task, this value specifies an id value that
-- represents the error. This value is set on the physical attempt object. It
-- is used to display error information to the user. It should not start with
-- string "Service_" which is reserved by the system.
stsiErrorId :: Lens' SetTaskStatus (Maybe Text)
stsiErrorId f x =
    f (_stsiErrorId x)
        <&> \y -> x { _stsiErrorId = y }
{-# INLINE stsiErrorId #-}

-- | If an error occurred during the task, this value specifies the stack trace
-- associated with the error. This value is set on the physical attempt
-- object. It is used to display error information to the user. The web
-- service does not parse this value.
stsiErrorStackTrace :: Lens' SetTaskStatus (Maybe Text)
stsiErrorStackTrace f x =
    f (_stsiErrorStackTrace x)
        <&> \y -> x { _stsiErrorStackTrace = y }
{-# INLINE stsiErrorStackTrace #-}

instance ToPath SetTaskStatus

instance ToQuery SetTaskStatus

instance ToHeaders SetTaskStatus

instance ToJSON SetTaskStatus

data SetTaskStatusResponse = SetTaskStatusResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetTaskStatus where
    type Sv SetTaskStatus = DataPipeline
    type Rs SetTaskStatus = SetTaskStatusResponse

    request = get
    response _ = nullaryResponse SetTaskStatusResponse
