{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.ListClosedWorkflowExecutions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of closed workflow executions in the specified domain that
-- meet the filtering criteria. The results may be split into multiple pages.
-- To retrieve subsequent pages, make the call again using the nextPageToken
-- returned by the initial call. This operation is eventually consistent. The
-- results are best effort and may not exactly reflect recent updates and
-- changes. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. Constrain the
-- following parameters by using a Condition element with the appropriate
-- keys. tagFilter.tag: String constraint. The key is swf:tagFilter.tag.
-- typeFilter.name: String constraint. The key is swf:typeFilter.name.
-- typeFilter.version: String constraint. The key is swf:typeFilter.version.
-- If the caller does not have sufficient permissions to invoke the action, or
-- the parameter values fall outside the specified constraints, the action
-- fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
-- ListClosedWorkflowExecutions Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 02:51:01 GMT X-Amz-Target:
-- SimpleWorkflowService.ListClosedWorkflowExecutions Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=WY9jGbf5E3F9smGJHANhEXz9VL+1oGVgNL0/o7cBxQw=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 150 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "closeTimeFilter": {"oldestDate": 1325376070, "latestDate":
-- 1356998399}, "tagFilter": {"tag": "ricoh-the-dog"} } HTTP/1.1 200 OK
-- Content-Length: 1084 Content-Type: application/json x-amzn-RequestId:
-- c28b4df4-3f23-11e1-9e8f-57bb03e21482 {"executionInfos": [
-- {"cancelRequested": false, "closeStatus": "TIMED_OUT", "closeTimestamp":
-- 1326590754.654, "execution": {"runId":
-- "c724e07a-b966-441f-a1c0-4831acbda1cd", "workflowId": "20110927-T-1"},
-- "executionStatus": "CLOSED", "startTimestamp": 1326587154.626, "tagList":
-- ["music purchase", "digital", "ricoh-the-dog"], "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} }, {"cancelRequested": false,
-- "closeStatus": "TIMED_OUT", "closeTimestamp": 1326586831.628, "execution":
-- {"runId": "f5ebbac6-941c-4342-ad69-dfd2f8be6689", "workflowId":
-- "20110927-T-1"}, "executionStatus": "CLOSED", "startTimestamp":
-- 1326585031.619, "tagList": ["music purchase", "digital", "ricoh-the-dog"],
-- "workflowType": {"name": "customerOrderWorkflow", "version": "1.0"} },
-- {"cancelRequested": false, "closeStatus": "TIMED_OUT", "closeTimestamp":
-- 1326582914.031, "execution": {"runId":
-- "1e536162-f1ea-48b0-85f3-aade88eef2f7", "workflowId": "20110927-T-1"},
-- "executionStatus": "CLOSED", "startTimestamp": 1326581114.02, "tagList":
-- ["music purchase", "digital", "ricoh-the-dog"], "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} } ] }.
module Network.AWS.SWF.V2012_01_25.ListClosedWorkflowExecutions where

import           Network.AWS.SWF.V2012_01_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'ListClosedWorkflowExecutions' request.
listClosedWorkflowExecutions :: Text -- ^ '_lcweiDomain'
                             -> ListClosedWorkflowExecutions
listClosedWorkflowExecutions p1 = ListClosedWorkflowExecutions
    { _lcweiDomain = p1
    , _lcweiCloseStatusFilter = Nothing
    , _lcweiStartTimeFilter = Nothing
    , _lcweiCloseTimeFilter = Nothing
    , _lcweiMaximumPageSize = Nothing
    , _lcweiNextPageToken = Nothing
    , _lcweiReverseOrder = Nothing
    , _lcweiTagFilter = Nothing
    , _lcweiExecutionFilter = Nothing
    , _lcweiTypeFilter = Nothing
    }

data ListClosedWorkflowExecutions = ListClosedWorkflowExecutions
    { _lcweiDomain :: Text
      -- ^ The name of the domain that contains the workflow executions to
      -- list.
    , _lcweiCloseStatusFilter :: Maybe CloseStatusFilter
      -- ^ If specified, only workflow executions that match this close
      -- status are listed. For example, if TERMINATED is specified, then
      -- only TERMINATED workflow executions are listed.
      -- closeStatusFilter, executionFilter, typeFilter and tagFilter are
      -- mutually exclusive. You can specify at most one of these in a
      -- request.
    , _lcweiStartTimeFilter :: Maybe ExecutionTimeFilter
      -- ^ If specified, the workflow executions are included in the
      -- returned results based on whether their start times are within
      -- the range specified by this filter. Also, if this parameter is
      -- specified, the returned results are ordered by their start times.
      -- startTimeFilter and closeTimeFilter are mutually exclusive. You
      -- must specify one of these in a request but not both.
    , _lcweiCloseTimeFilter :: Maybe ExecutionTimeFilter
      -- ^ If specified, the workflow executions are included in the
      -- returned results based on whether their close times are within
      -- the range specified by this filter. Also, if this parameter is
      -- specified, the returned results are ordered by their close times.
      -- startTimeFilter and closeTimeFilter are mutually exclusive. You
      -- must specify one of these in a request but not both.
    , _lcweiMaximumPageSize :: Maybe Integer
      -- ^ The maximum number of results returned in each page. The default
      -- is 100, but the caller can override this value to a page size
      -- smaller than the default. You cannot specify a page size greater
      -- than 100. Note that the number of executions may be less than the
      -- maxiumum page size, in which case, the returned page will have
      -- fewer results than the maximumPageSize specified.
    , _lcweiNextPageToken :: Maybe Text
      -- ^ If on a previous call to this method a NextPageToken was
      -- returned, the results are being paginated. To get the next page
      -- of results, repeat the call with the returned token and all other
      -- arguments unchanged.
    , _lcweiReverseOrder :: Maybe Bool
      -- ^ When set to true, returns the results in reverse order. By
      -- default the results are returned in descending order of the start
      -- or the close time of the executions.
    , _lcweiTagFilter :: Maybe TagFilter
      -- ^ If specified, only executions that have the matching tag are
      -- listed. closeStatusFilter, executionFilter, typeFilter and
      -- tagFilter are mutually exclusive. You can specify at most one of
      -- these in a request.
    , _lcweiExecutionFilter :: Maybe WorkflowExecutionFilter
      -- ^ If specified, only workflow executions matching the workflow id
      -- specified in the filter are returned. closeStatusFilter,
      -- executionFilter, typeFilter and tagFilter are mutually exclusive.
      -- You can specify at most one of these in a request.
    , _lcweiTypeFilter :: Maybe WorkflowTypeFilter
      -- ^ If specified, only executions of the type specified in the filter
      -- are returned. closeStatusFilter, executionFilter, typeFilter and
      -- tagFilter are mutually exclusive. You can specify at most one of
      -- these in a request.
    } deriving (Show, Generic)

makeLenses ''ListClosedWorkflowExecutions

instance ToPath ListClosedWorkflowExecutions

instance ToQuery ListClosedWorkflowExecutions

instance ToHeaders ListClosedWorkflowExecutions

instance ToJSON ListClosedWorkflowExecutions

data ListClosedWorkflowExecutionsResponse = ListClosedWorkflowExecutionsResponse
    { _welExecutionInfos :: [WorkflowExecutionInfo]
      -- ^ The list of workflow information structures.
    , _welNextPageToken :: Maybe Text
      -- ^ The token of the next page in the result. If set, the results
      -- have more than one page. The next page can be retrieved by
      -- repeating the request with this token and all other arguments
      -- unchanged.
    } deriving (Show, Generic)

makeLenses ''ListClosedWorkflowExecutionsResponse

instance FromJSON ListClosedWorkflowExecutionsResponse

instance AWSRequest ListClosedWorkflowExecutions where
    type Sv ListClosedWorkflowExecutions = SWF
    type Rs ListClosedWorkflowExecutions = ListClosedWorkflowExecutionsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListClosedWorkflowExecutions where
    next rq rs = (\x -> rq { _lcweiNextPageToken = Just x })
        <$> (_welNextPageToken rs)
