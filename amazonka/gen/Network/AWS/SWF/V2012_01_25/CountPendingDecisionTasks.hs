{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SWF.V2012_01_25.CountPendingDecisionTasks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the estimated number of decision tasks in the specified task list.
-- The count returned is an approximation and is not guaranteed to be exact.
-- If you specify a task list that no decision task was ever scheduled in then
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
-- CountPendingDecisionTasks Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 23:25:57 GMT X-Amz-Target:
-- SimpleWorkflowService.CountPendingDecisionTasks Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=i9tUkWnZBLfn/T6BOymajCtwArAll6Stuh1x2C4dbsE=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 70 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "taskList": {"name": "specialTaskList"} } HTTP/1.1 200 OK
-- Content-Length: 29 Content-Type: application/json x-amzn-RequestId:
-- 4718a364-3fd0-11e1-9914-a356b6ea8bdf {"count": 2, "truncated": false}.
module Network.AWS.SWF.V2012_01_25.CountPendingDecisionTasks where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude

data CountPendingDecisionTasks = CountPendingDecisionTasks
    { _cpdtiDomain :: Text
      -- ^ The name of the domain that contains the task list.
    , _cpdtiTaskList :: TaskList
      -- ^ The name of the task list.
    } deriving (Show, Generic)

makeLenses ''CountPendingDecisionTasks

instance ToPath CountPendingDecisionTasks

instance ToQuery CountPendingDecisionTasks

instance ToHeaders CountPendingDecisionTasks

instance ToJSON CountPendingDecisionTasks

data CountPendingDecisionTasksResponse = CountPendingDecisionTasksResponse
    { _ptdCount :: Integer
      -- ^ The number of tasks in the task list.
    , _ptdTruncated :: Maybe Bool
      -- ^ If set to true, indicates that the actual count was more than the
      -- maximum supported by this API and the count returned is the
      -- truncated value.
    } deriving (Show, Generic)

makeLenses ''CountPendingDecisionTasksResponse

instance FromJSON CountPendingDecisionTasksResponse

instance AWSRequest CountPendingDecisionTasks where
    type Sv CountPendingDecisionTasks = SWF
    type Rs CountPendingDecisionTasks = CountPendingDecisionTasksResponse

    request = get
    response _ = jsonResponse
