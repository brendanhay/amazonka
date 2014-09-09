{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.ReportTaskProgress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the AWS Data Pipeline service on the progress of the calling task
-- runner. When the task runner is assigned a task, it should call
-- ReportTaskProgress to acknowledge that it has the task within 2 minutes. If
-- the web service does not recieve this acknowledgement within the 2 minute
-- window, it will assign the task in a subsequent PollForTask call. After
-- this initial acknowledgement, the task runner only needs to report progress
-- every 15 minutes to maintain its ownership of the task. You can change this
-- reporting time from 15 minutes by specifying a reportProgressTimeout field
-- in your pipeline. If a task runner does not report its status after 5
-- minutes, AWS Data Pipeline will assume that the task runner is unable to
-- process the task and will reassign the task in a subsequent response to
-- PollForTask. task runners should call ReportTaskProgress every 60 seconds.
-- POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.ReportTaskProgress Content-Length: 832 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"taskId":
-- "aaGgHT4LuH0T0Y0oLrJRjas5qH0d8cDPADxqq3tn+zCWGELkCdV2JprLreXm1oxeP5EFZHFLJ69kjSsLYE0iYHYBYVGBrB+E/pYq7ANEEeGJFnSBMRiXZVA+8UJ3OzcInvXeinqBmBaKwii7hnnKb/AXjXiNTXyxgydX1KAyg1AxkwBYG4cfPYMZbuEbQJFJvv5C/2+GVXz1w94nKYTeUeepwUOFOuRLS6JVtZoYwpF56E+Yfk1IcGpFOvCZ01B4Bkuu7x3J+MD/j6kJgZLAgbCJQtI3eiW3kdGmX0p0I2BdY1ZsX6b4UiSvM3OMj6NEHJCJL4E0ZfitnhCoe24Kvjo6C2hFbZq+ei/HPgSXBQMSagkr4vS9c0ChzxH2+LNYvec6bY4kymkaZI1dvOzmpa0FcnGf5AjSK4GpsViZ/ujz6zxFv81qBXzjF0/4M1775rjV1VUdyKaixiA/sJiACNezqZqETidp8d24BDPRhGsj6pBCrnelqGFrk/gXEXUsJ+xwMifRC8UVwiKekpAvHUywVk7Ku4jH/n3i2VoLRP6FXwpUbelu34iiZ9czpXyLtyPKwxa87dlrnRVURwkcVjOt2Mcrcaqe+cbWHvNRhyrPkkdfSF3ac8/wfgVbXvLEB2k9mKc67aD9rvdc1PKX09Tk8BKklsMTpZ3TRCd4NzQlJKigMe8Jat9+1tKj4Ole5ZzW6uyTu2s2iFjEV8KXu4MaiRJyNKCdKeGhhZWY37Qk4NBK4Ppgu+C6Y41dpfOh288SLDEVx0/UySlqOEdhba7c6BiPp5r3hKj3mk9lFy5OYp1aoGLeeFmjXveTnPdf2gkWqXXg7AUbJ7jEs1F0lKZQg4szep2gcKyAJXgvXLfJJHcha8Lfb/Ee7wYmyOcAaRpDBoFNSbtoVXar46teIrpho+ZDvynUXvU0grHWGOk=:wn3SgymHZM99bEXAMPLE",
-- "fields": [ {"key": "percentComplete", "stringValue": "50"} ] }
-- x-amzn-RequestId: 640bd023-0775-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 18 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"canceled": false}.
module Network.AWS.DataPipeline.V2012_10_29.ReportTaskProgress
    (
    -- * Request
      ReportTaskProgress
    -- ** Request constructor
    , mkReportTaskProgress
    -- ** Request lenses
    , rtpTaskId

    -- * Response
    , ReportTaskProgressResponse
    -- ** Response constructor
    , mkReportTaskProgressResponse
    -- ** Response lenses
    , rtprCanceled
    ) where

import Network.AWS.DataPipeline.V2012_10_29.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The input for the ReportTaskProgress action.
newtype ReportTaskProgress = ReportTaskProgress
    { _rtpTaskId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReportTaskProgress' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TaskId ::@ @Text@
--
mkReportTaskProgress :: Text -- ^ 'rtpTaskId'
                     -> ReportTaskProgress
mkReportTaskProgress p1 = ReportTaskProgress
    { _rtpTaskId = p1
    }

-- | Identifier of the task assigned to the task runner. This value is provided
-- in the TaskObject that the service returns with the response for the
-- PollForTask action.
rtpTaskId :: Lens' ReportTaskProgress Text
rtpTaskId = lens _rtpTaskId (\s a -> s { _rtpTaskId = a })

instance ToPath ReportTaskProgress

instance ToQuery ReportTaskProgress

instance ToHeaders ReportTaskProgress

instance ToJSON ReportTaskProgress

-- | Contains the output from the ReportTaskProgress action.
newtype ReportTaskProgressResponse = ReportTaskProgressResponse
    { _rtprCanceled :: Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReportTaskProgressResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Canceled ::@ @Bool@
--
mkReportTaskProgressResponse :: Bool -- ^ 'rtprCanceled'
                             -> ReportTaskProgressResponse
mkReportTaskProgressResponse p1 = ReportTaskProgressResponse
    { _rtprCanceled = p1
    }

-- | If True, the calling task runner should cancel processing of the task. The
-- task runner does not need to call SetTaskStatus for canceled tasks.
rtprCanceled :: Lens' ReportTaskProgressResponse Bool
rtprCanceled = lens _rtprCanceled (\s a -> s { _rtprCanceled = a })

instance FromJSON ReportTaskProgressResponse

instance AWSRequest ReportTaskProgress where
    type Sv ReportTaskProgress = DataPipeline
    type Rs ReportTaskProgress = ReportTaskProgressResponse

    request = get
    response _ = jsonResponse
