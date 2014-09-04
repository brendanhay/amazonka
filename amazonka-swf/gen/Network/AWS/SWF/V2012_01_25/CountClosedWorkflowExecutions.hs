{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.CountClosedWorkflowExecutions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the number of closed workflow executions within the given domain
-- that meet the specified filtering criteria. This operation is eventually
-- consistent. The results are best effort and may not exactly reflect recent
-- updates and changes. Access Control You can use IAM policies to control
-- this action's access to Amazon SWF resources as follows: Use a Resource
-- element with the domain name to limit the action to only specified domains.
-- Use an Action element to allow or deny permission to call this action.
-- Constrain the following parameters by using a Condition element with the
-- appropriate keys. tagFilter.tag: String constraint. The key is
-- swf:tagFilter.tag. typeFilter.name: String constraint. The key is
-- swf:typeFilter.name. typeFilter.version: String constraint. The key is
-- swf:typeFilter.version. If the caller does not have sufficient permissions
-- to invoke the action, or the parameter values fall outside the specified
-- constraints, the action fails by throwing OperationNotPermitted. For
-- details and example IAM policies, see Using IAM to Manage Access to Amazon
-- SWF Workflows. CountClosedWorkflowExecutions Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 02:42:47 GMT X-Amz-Target:
-- SimpleWorkflowService.CountClosedWorkflowExecutions Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=jFS74utjeATV7vj72CWdLToPCKW0RQse6OEDkafB+SA=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 157 Pragma: no-cache Cache-Control: no-cache { "domain":
-- "867530901", "closeTimeFilter": {"oldestDate": 1325376070, "latestDate":
-- 1356998399}, "closeStatusFilter": {"status": "TIMED_OUT"} } HTTP/1.1 200 OK
-- Content-Length: 29 Content-Type: application/json x-amzn-RequestId:
-- 9bfad387-3f22-11e1-9914-a356b6ea8bdf { "count":3, "truncated":false }.
module Network.AWS.SWF.V2012_01_25.CountClosedWorkflowExecutions
    (
    -- * Request
      CountClosedWorkflowExecutions
    -- ** Request constructor
    , mkCountClosedWorkflowExecutionsInput
    -- ** Request lenses
    , ccweiDomain
    , ccweiStartTimeFilter
    , ccweiCloseTimeFilter
    , ccweiExecutionFilter
    , ccweiTypeFilter
    , ccweiTagFilter
    , ccweiCloseStatusFilter

    -- * Response
    , CountClosedWorkflowExecutionsResponse
    -- ** Response lenses
    , wecCount
    , wecTruncated
    ) where

import           Network.AWS.SWF.V2012_01_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CountClosedWorkflowExecutions' request.
mkCountClosedWorkflowExecutionsInput :: Text -- ^ 'ccweiDomain'
                                     -> CountClosedWorkflowExecutions
mkCountClosedWorkflowExecutionsInput p1 = CountClosedWorkflowExecutions
    { _ccweiDomain = p1
    , _ccweiStartTimeFilter = Nothing
    , _ccweiCloseTimeFilter = Nothing
    , _ccweiExecutionFilter = Nothing
    , _ccweiTypeFilter = Nothing
    , _ccweiTagFilter = Nothing
    , _ccweiCloseStatusFilter = Nothing
    }
{-# INLINE mkCountClosedWorkflowExecutionsInput #-}

data CountClosedWorkflowExecutions = CountClosedWorkflowExecutions
    { _ccweiDomain :: Text
      -- ^ The name of the domain containing the workflow executions to
      -- count.
    , _ccweiStartTimeFilter :: Maybe ExecutionTimeFilter
      -- ^ If specified, only workflow executions that meet the start time
      -- criteria of the filter are counted. startTimeFilter and
      -- closeTimeFilter are mutually exclusive. You must specify one of
      -- these in a request but not both.
    , _ccweiCloseTimeFilter :: Maybe ExecutionTimeFilter
      -- ^ If specified, only workflow executions that meet the close time
      -- criteria of the filter are counted. startTimeFilter and
      -- closeTimeFilter are mutually exclusive. You must specify one of
      -- these in a request but not both.
    , _ccweiExecutionFilter :: Maybe WorkflowExecutionFilter
      -- ^ If specified, only workflow executions matching the WorkflowId in
      -- the filter are counted. closeStatusFilter, executionFilter,
      -- typeFilter and tagFilter are mutually exclusive. You can specify
      -- at most one of these in a request.
    , _ccweiTypeFilter :: Maybe WorkflowTypeFilter
      -- ^ If specified, indicates the type of the workflow executions to be
      -- counted. closeStatusFilter, executionFilter, typeFilter and
      -- tagFilter are mutually exclusive. You can specify at most one of
      -- these in a request.
    , _ccweiTagFilter :: Maybe TagFilter
      -- ^ If specified, only executions that have a tag that matches the
      -- filter are counted. closeStatusFilter, executionFilter,
      -- typeFilter and tagFilter are mutually exclusive. You can specify
      -- at most one of these in a request.
    , _ccweiCloseStatusFilter :: Maybe CloseStatusFilter
      -- ^ If specified, only workflow executions that match this close
      -- status are counted. This filter has an affect only if
      -- executionStatus is specified as CLOSED. closeStatusFilter,
      -- executionFilter, typeFilter and tagFilter are mutually exclusive.
      -- You can specify at most one of these in a request.
    } deriving (Show, Generic)

-- | The name of the domain containing the workflow executions to count.
ccweiDomain :: Lens' CountClosedWorkflowExecutions (Text)
ccweiDomain = lens _ccweiDomain (\s a -> s { _ccweiDomain = a })
{-# INLINE ccweiDomain #-}

-- | If specified, only workflow executions that meet the start time criteria of
-- the filter are counted. startTimeFilter and closeTimeFilter are mutually
-- exclusive. You must specify one of these in a request but not both.
ccweiStartTimeFilter :: Lens' CountClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
ccweiStartTimeFilter = lens _ccweiStartTimeFilter (\s a -> s { _ccweiStartTimeFilter = a })
{-# INLINE ccweiStartTimeFilter #-}

-- | If specified, only workflow executions that meet the close time criteria of
-- the filter are counted. startTimeFilter and closeTimeFilter are mutually
-- exclusive. You must specify one of these in a request but not both.
ccweiCloseTimeFilter :: Lens' CountClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
ccweiCloseTimeFilter = lens _ccweiCloseTimeFilter (\s a -> s { _ccweiCloseTimeFilter = a })
{-# INLINE ccweiCloseTimeFilter #-}

-- | If specified, only workflow executions matching the WorkflowId in the
-- filter are counted. closeStatusFilter, executionFilter, typeFilter and
-- tagFilter are mutually exclusive. You can specify at most one of these in a
-- request.
ccweiExecutionFilter :: Lens' CountClosedWorkflowExecutions (Maybe WorkflowExecutionFilter)
ccweiExecutionFilter = lens _ccweiExecutionFilter (\s a -> s { _ccweiExecutionFilter = a })
{-# INLINE ccweiExecutionFilter #-}

-- | If specified, indicates the type of the workflow executions to be counted.
-- closeStatusFilter, executionFilter, typeFilter and tagFilter are mutually
-- exclusive. You can specify at most one of these in a request.
ccweiTypeFilter :: Lens' CountClosedWorkflowExecutions (Maybe WorkflowTypeFilter)
ccweiTypeFilter = lens _ccweiTypeFilter (\s a -> s { _ccweiTypeFilter = a })
{-# INLINE ccweiTypeFilter #-}

-- | If specified, only executions that have a tag that matches the filter are
-- counted. closeStatusFilter, executionFilter, typeFilter and tagFilter are
-- mutually exclusive. You can specify at most one of these in a request.
ccweiTagFilter :: Lens' CountClosedWorkflowExecutions (Maybe TagFilter)
ccweiTagFilter = lens _ccweiTagFilter (\s a -> s { _ccweiTagFilter = a })
{-# INLINE ccweiTagFilter #-}

-- | If specified, only workflow executions that match this close status are
-- counted. This filter has an affect only if executionStatus is specified as
-- CLOSED. closeStatusFilter, executionFilter, typeFilter and tagFilter are
-- mutually exclusive. You can specify at most one of these in a request.
ccweiCloseStatusFilter :: Lens' CountClosedWorkflowExecutions (Maybe CloseStatusFilter)
ccweiCloseStatusFilter = lens _ccweiCloseStatusFilter (\s a -> s { _ccweiCloseStatusFilter = a })
{-# INLINE ccweiCloseStatusFilter #-}

instance ToPath CountClosedWorkflowExecutions

instance ToQuery CountClosedWorkflowExecutions

instance ToHeaders CountClosedWorkflowExecutions

instance ToJSON CountClosedWorkflowExecutions

data CountClosedWorkflowExecutionsResponse = CountClosedWorkflowExecutionsResponse
    { _wecCount :: Integer
      -- ^ The number of workflow executions.
    , _wecTruncated :: Maybe Bool
      -- ^ If set to true, indicates that the actual count was more than the
      -- maximum supported by this API and the count returned is the
      -- truncated value.
    } deriving (Show, Generic)

-- | The number of workflow executions.
wecCount :: Lens' CountClosedWorkflowExecutionsResponse (Integer)
wecCount = lens _wecCount (\s a -> s { _wecCount = a })
{-# INLINE wecCount #-}

-- | If set to true, indicates that the actual count was more than the maximum
-- supported by this API and the count returned is the truncated value.
wecTruncated :: Lens' CountClosedWorkflowExecutionsResponse (Maybe Bool)
wecTruncated = lens _wecTruncated (\s a -> s { _wecTruncated = a })
{-# INLINE wecTruncated #-}

instance FromJSON CountClosedWorkflowExecutionsResponse

instance AWSRequest CountClosedWorkflowExecutions where
    type Sv CountClosedWorkflowExecutions = SWF
    type Rs CountClosedWorkflowExecutions = CountClosedWorkflowExecutionsResponse

    request = get
    response _ = jsonResponse
