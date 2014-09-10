{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.CountPendingActivityTasks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the estimated number of activity tasks in the specified task list.
-- The count returned is an approximation and is not guaranteed to be exact.
-- If you specify a task list that no activity task was ever scheduled in then
-- 0 will be returned. Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. Constrain
-- the taskList.name parameter by using a Condition element with the
-- swf:taskList.name key to allow the action to access only certain task
-- lists. If the caller does not have sufficient permissions to invoke the
-- action, or the parameter values fall outside the specified constraints, the
-- action fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
-- CountPendingActivityTasks Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 03:29:28 GMT X-Amz-Target:
-- SimpleWorkflowService.CountPendingActivityTasks Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=eCNiyyl5qmP0gGQ0hM8LqeRzxEvVZ0LAjE4oxVzzk9w=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 70 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "taskList": {"name": "specialTaskList"} } HTTP/1.1 200 OK
-- Content-Length: 29 Content-Type: application/json x-amzn-RequestId:
-- 4b977c76-3ff2-11e1-a23a-99d60383ae71 {"count":1,"truncated":false}.
module Network.AWS.SWF.CountPendingActivityTasks
    (
    -- * Request
      CountPendingActivityTasks
    -- ** Request constructor
    , mkCountPendingActivityTasks
    -- ** Request lenses
    , cpatDomain
    , cpatTaskList

    -- * Response
    , CountPendingActivityTasksResponse
    -- ** Response constructor
    , mkCountPendingActivityTasksResponse
    -- ** Response lenses
    , cpatrCount
    , cpatrTruncated
    ) where

import Network.AWS.SWF.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data CountPendingActivityTasks = CountPendingActivityTasks
    { _cpatDomain :: !Text
    , _cpatTaskList :: TaskList
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CountPendingActivityTasks' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Domain ::@ @Text@
--
-- * @TaskList ::@ @TaskList@
--
mkCountPendingActivityTasks :: Text -- ^ 'cpatDomain'
                            -> TaskList -- ^ 'cpatTaskList'
                            -> CountPendingActivityTasks
mkCountPendingActivityTasks p1 p2 = CountPendingActivityTasks
    { _cpatDomain = p1
    , _cpatTaskList = p2
    }

-- | The name of the domain that contains the task list.
cpatDomain :: Lens' CountPendingActivityTasks Text
cpatDomain = lens _cpatDomain (\s a -> s { _cpatDomain = a })

-- | The name of the task list.
cpatTaskList :: Lens' CountPendingActivityTasks TaskList
cpatTaskList = lens _cpatTaskList (\s a -> s { _cpatTaskList = a })

instance ToPath CountPendingActivityTasks

instance ToQuery CountPendingActivityTasks

instance ToHeaders CountPendingActivityTasks

instance ToJSON CountPendingActivityTasks

-- | Contains the count of tasks in a task list.
data CountPendingActivityTasksResponse = CountPendingActivityTasksResponse
    { _cpatrCount :: !Integer
    , _cpatrTruncated :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CountPendingActivityTasksResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Count ::@ @Integer@
--
-- * @Truncated ::@ @Maybe Bool@
--
mkCountPendingActivityTasksResponse :: Integer -- ^ 'cpatrCount'
                                    -> CountPendingActivityTasksResponse
mkCountPendingActivityTasksResponse p1 = CountPendingActivityTasksResponse
    { _cpatrCount = p1
    , _cpatrTruncated = Nothing
    }

-- | The number of tasks in the task list.
cpatrCount :: Lens' CountPendingActivityTasksResponse Integer
cpatrCount = lens _cpatrCount (\s a -> s { _cpatrCount = a })

-- | If set to true, indicates that the actual count was more than the maximum
-- supported by this API and the count returned is the truncated value.
cpatrTruncated :: Lens' CountPendingActivityTasksResponse (Maybe Bool)
cpatrTruncated = lens _cpatrTruncated (\s a -> s { _cpatrTruncated = a })

instance FromJSON CountPendingActivityTasksResponse

instance AWSRequest CountPendingActivityTasks where
    type Sv CountPendingActivityTasks = SWF
    type Rs CountPendingActivityTasks = CountPendingActivityTasksResponse

    request = get
    response _ = jsonResponse
