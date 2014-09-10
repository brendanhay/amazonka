{-# LANGUAGE DeriveGeneric               #-}
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
-- a call to ReportTaskProgress. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.SetTaskStatus
-- Content-Length: 847 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"taskId":
-- "aaGgHT4LuH0T0Y0oLrJRjas5qH0d8cDPADxqq3tn+zCWGELkCdV2JprLreXm1oxeP5EFZHFLJ69kjSsLYE0iYHYBYVGBrB+E/pYq7ANEEeGJFnSBMRiXZVA+8UJ3OzcInvXeinqBmBaKwii7hnnKb/AXjXiNTXyxgydX1KAyg1AxkwBYG4cfPYMZbuEbQJFJvv5C/2+GVXz1w94nKYTeUeepwUOFOuRLS6JVtZoYwpF56E+Yfk1IcGpFOvCZ01B4Bkuu7x3J+MD/j6kJgZLAgbCJQtI3eiW3kdGmX0p0I2BdY1ZsX6b4UiSvM3OMj6NEHJCJL4E0ZfitnhCoe24Kvjo6C2hFbZq+ei/HPgSXBQMSagkr4vS9c0ChzxH2+LNYvec6bY4kymkaZI1dvOzmpa0FcnGf5AjSK4GpsViZ/ujz6zxFv81qBXzjF0/4M1775rjV1VUdyKaixiA/sJiACNezqZqETidp8d24BDPRhGsj6pBCrnelqGFrk/gXEXUsJ+xwMifRC8UVwiKekpAvHUywVk7Ku4jH/n3i2VoLRP6FXwpUbelu34iiZ9czpXyLtyPKwxa87dlrnRVURwkcVjOt2Mcrcaqe+cbWHvNRhyrPkkdfSF3ac8/wfgVbXvLEB2k9mKc67aD9rvdc1PKX09Tk8BKklsMTpZ3TRCd4NzQlJKigMe8Jat9+1tKj4Ole5ZzW6uyTu2s2iFjEV8KXu4MaiRJyNKCdKeGhhZWY37Qk4NBK4Ppgu+C6Y41dpfOh288SLDEVx0/UySlqOEdhba7c6BiPp5r3hKj3mk9lFy5OYp1aoGLeeFmjXveTnPdf2gkWqXXg7AUbJ7jEs1F0lKZQg4szep2gcKyAJXgvXLfJJHcha8Lfb/Ee7wYmyOcAaRpDBoFNSbtoVXar46teIrpho+ZDvynUXvU0grHWGOk=:wn3SgymHZM99bEXAMPLE",
-- "taskStatus": "FINISHED"} x-amzn-RequestId:
-- 8c8deb53-0788-11e2-af9c-6bc7a6be6qr8 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {}.
module Network.AWS.DataPipeline
    (
    -- * Request
      SetTaskStatus
    -- ** Request constructor
    , mkSetTaskStatus
    -- ** Request lenses
    , stsTaskId
    , stsTaskStatus
    , stsErrorId
    , stsErrorMessage
    , stsErrorStackTrace

    -- * Response
    , SetTaskStatusResponse
    -- ** Response constructor
    , mkSetTaskStatusResponse
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The input of the SetTaskStatus action.
data SetTaskStatus = SetTaskStatus
    { _stsTaskId :: !Text
    , _stsTaskStatus :: TaskStatus
    , _stsErrorId :: !(Maybe Text)
    , _stsErrorMessage :: !(Maybe Text)
    , _stsErrorStackTrace :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetTaskStatus' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TaskId ::@ @Text@
--
-- * @TaskStatus ::@ @TaskStatus@
--
-- * @ErrorId ::@ @Maybe Text@
--
-- * @ErrorMessage ::@ @Maybe Text@
--
-- * @ErrorStackTrace ::@ @Maybe Text@
--
mkSetTaskStatus :: Text -- ^ 'stsTaskId'
                -> TaskStatus -- ^ 'stsTaskStatus'
                -> SetTaskStatus
mkSetTaskStatus p1 p2 = SetTaskStatus
    { _stsTaskId = p1
    , _stsTaskStatus = p2
    , _stsErrorId = Nothing
    , _stsErrorMessage = Nothing
    , _stsErrorStackTrace = Nothing
    }

-- | Identifies the task assigned to the task runner. This value is set in the
-- TaskObject that is returned by the PollForTask action.
stsTaskId :: Lens' SetTaskStatus Text
stsTaskId = lens _stsTaskId (\s a -> s { _stsTaskId = a })

-- | If FINISHED, the task successfully completed. If FAILED the task ended
-- unsuccessfully. The FALSE value is used by preconditions.
stsTaskStatus :: Lens' SetTaskStatus TaskStatus
stsTaskStatus = lens _stsTaskStatus (\s a -> s { _stsTaskStatus = a })

-- | If an error occurred during the task, this value specifies an id value that
-- represents the error. This value is set on the physical attempt object. It
-- is used to display error information to the user. It should not start with
-- string "Service_" which is reserved by the system.
stsErrorId :: Lens' SetTaskStatus (Maybe Text)
stsErrorId = lens _stsErrorId (\s a -> s { _stsErrorId = a })

-- | If an error occurred during the task, this value specifies a text
-- description of the error. This value is set on the physical attempt object.
-- It is used to display error information to the user. The web service does
-- not parse this value.
stsErrorMessage :: Lens' SetTaskStatus (Maybe Text)
stsErrorMessage = lens _stsErrorMessage (\s a -> s { _stsErrorMessage = a })

-- | If an error occurred during the task, this value specifies the stack trace
-- associated with the error. This value is set on the physical attempt
-- object. It is used to display error information to the user. The web
-- service does not parse this value.
stsErrorStackTrace :: Lens' SetTaskStatus (Maybe Text)
stsErrorStackTrace =
    lens _stsErrorStackTrace (\s a -> s { _stsErrorStackTrace = a })

instance ToPath SetTaskStatus

instance ToQuery SetTaskStatus

instance ToHeaders SetTaskStatus

instance ToJSON SetTaskStatus

-- | The output from the SetTaskStatus action.
data SetTaskStatusResponse = SetTaskStatusResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetTaskStatusResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkSetTaskStatusResponse :: SetTaskStatusResponse
mkSetTaskStatusResponse = SetTaskStatusResponse

instance AWSRequest SetTaskStatus where
    type Sv SetTaskStatus = DataPipeline
    type Rs SetTaskStatus = SetTaskStatusResponse

    request = get
    response _ = nullaryResponse SetTaskStatusResponse
