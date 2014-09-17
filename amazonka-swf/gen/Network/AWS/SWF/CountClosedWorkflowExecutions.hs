{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.CountClosedWorkflowExecutions
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
module Network.AWS.SWF.CountClosedWorkflowExecutions
    (
    -- * Request
      CountClosedWorkflowExecutions
    -- ** Request constructor
    , mkCountClosedWorkflowExecutions
    -- ** Request lenses
    , ccweDomain
    , ccweStartTimeFilter
    , ccweCloseTimeFilter
    , ccweExecutionFilter
    , ccweTypeFilter
    , ccweTagFilter
    , ccweCloseStatusFilter

    -- * Response
    , CountClosedWorkflowExecutionsResponse
    -- ** Response constructor
    , mkCountClosedWorkflowExecutionsResponse
    -- ** Response lenses
    , ccwerCount
    , ccwerTruncated
    ) where

import Network.AWS.SWF.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data CountClosedWorkflowExecutions = CountClosedWorkflowExecutions
    { _ccweDomain :: Text
    , _ccweStartTimeFilter :: Maybe ExecutionTimeFilter
    , _ccweCloseTimeFilter :: Maybe ExecutionTimeFilter
    , _ccweExecutionFilter :: Maybe WorkflowExecutionFilter
    , _ccweTypeFilter :: Maybe WorkflowTypeFilter
    , _ccweTagFilter :: Maybe TagFilter
    , _ccweCloseStatusFilter :: Maybe CloseStatusFilter
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CountClosedWorkflowExecutions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Domain ::@ @Text@
--
-- * @StartTimeFilter ::@ @Maybe ExecutionTimeFilter@
--
-- * @CloseTimeFilter ::@ @Maybe ExecutionTimeFilter@
--
-- * @ExecutionFilter ::@ @Maybe WorkflowExecutionFilter@
--
-- * @TypeFilter ::@ @Maybe WorkflowTypeFilter@
--
-- * @TagFilter ::@ @Maybe TagFilter@
--
-- * @CloseStatusFilter ::@ @Maybe CloseStatusFilter@
--
mkCountClosedWorkflowExecutions :: Text -- ^ 'ccweDomain'
                                -> CountClosedWorkflowExecutions
mkCountClosedWorkflowExecutions p1 = CountClosedWorkflowExecutions
    { _ccweDomain = p1
    , _ccweStartTimeFilter = Nothing
    , _ccweCloseTimeFilter = Nothing
    , _ccweExecutionFilter = Nothing
    , _ccweTypeFilter = Nothing
    , _ccweTagFilter = Nothing
    , _ccweCloseStatusFilter = Nothing
    }

-- | The name of the domain containing the workflow executions to count.
ccweDomain :: Lens' CountClosedWorkflowExecutions Text
ccweDomain = lens _ccweDomain (\s a -> s { _ccweDomain = a })

-- | If specified, only workflow executions that meet the start time criteria of
-- the filter are counted. startTimeFilter and closeTimeFilter are mutually
-- exclusive. You must specify one of these in a request but not both.
ccweStartTimeFilter :: Lens' CountClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
ccweStartTimeFilter =
    lens _ccweStartTimeFilter (\s a -> s { _ccweStartTimeFilter = a })

-- | If specified, only workflow executions that meet the close time criteria of
-- the filter are counted. startTimeFilter and closeTimeFilter are mutually
-- exclusive. You must specify one of these in a request but not both.
ccweCloseTimeFilter :: Lens' CountClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
ccweCloseTimeFilter =
    lens _ccweCloseTimeFilter (\s a -> s { _ccweCloseTimeFilter = a })

-- | If specified, only workflow executions matching the WorkflowId in the
-- filter are counted. closeStatusFilter, executionFilter, typeFilter and
-- tagFilter are mutually exclusive. You can specify at most one of these in a
-- request.
ccweExecutionFilter :: Lens' CountClosedWorkflowExecutions (Maybe WorkflowExecutionFilter)
ccweExecutionFilter =
    lens _ccweExecutionFilter (\s a -> s { _ccweExecutionFilter = a })

-- | If specified, indicates the type of the workflow executions to be counted.
-- closeStatusFilter, executionFilter, typeFilter and tagFilter are mutually
-- exclusive. You can specify at most one of these in a request.
ccweTypeFilter :: Lens' CountClosedWorkflowExecutions (Maybe WorkflowTypeFilter)
ccweTypeFilter = lens _ccweTypeFilter (\s a -> s { _ccweTypeFilter = a })

-- | If specified, only executions that have a tag that matches the filter are
-- counted. closeStatusFilter, executionFilter, typeFilter and tagFilter are
-- mutually exclusive. You can specify at most one of these in a request.
ccweTagFilter :: Lens' CountClosedWorkflowExecutions (Maybe TagFilter)
ccweTagFilter = lens _ccweTagFilter (\s a -> s { _ccweTagFilter = a })

-- | If specified, only workflow executions that match this close status are
-- counted. This filter has an affect only if executionStatus is specified as
-- CLOSED. closeStatusFilter, executionFilter, typeFilter and tagFilter are
-- mutually exclusive. You can specify at most one of these in a request.
ccweCloseStatusFilter :: Lens' CountClosedWorkflowExecutions (Maybe CloseStatusFilter)
ccweCloseStatusFilter =
    lens _ccweCloseStatusFilter (\s a -> s { _ccweCloseStatusFilter = a })

instance ToPath CountClosedWorkflowExecutions

instance ToQuery CountClosedWorkflowExecutions

instance ToHeaders CountClosedWorkflowExecutions

instance ToJSON CountClosedWorkflowExecutions

-- | Contains the count of workflow executions returned from
-- CountOpenWorkflowExecutions or CountClosedWorkflowExecutions.
data CountClosedWorkflowExecutionsResponse = CountClosedWorkflowExecutionsResponse
    { _ccwerCount :: !Integer
    , _ccwerTruncated :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CountClosedWorkflowExecutionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Count ::@ @Integer@
--
-- * @Truncated ::@ @Maybe Bool@
--
mkCountClosedWorkflowExecutionsResponse :: Integer -- ^ 'ccwerCount'
                                        -> CountClosedWorkflowExecutionsResponse
mkCountClosedWorkflowExecutionsResponse p1 = CountClosedWorkflowExecutionsResponse
    { _ccwerCount = p1
    , _ccwerTruncated = Nothing
    }

-- | The number of workflow executions.
ccwerCount :: Lens' CountClosedWorkflowExecutionsResponse Integer
ccwerCount = lens _ccwerCount (\s a -> s { _ccwerCount = a })

-- | If set to true, indicates that the actual count was more than the maximum
-- supported by this API and the count returned is the truncated value.
ccwerTruncated :: Lens' CountClosedWorkflowExecutionsResponse (Maybe Bool)
ccwerTruncated = lens _ccwerTruncated (\s a -> s { _ccwerTruncated = a })

instance FromJSON CountClosedWorkflowExecutionsResponse

instance AWSRequest CountClosedWorkflowExecutions where
    type Sv CountClosedWorkflowExecutions = SWF
    type Rs CountClosedWorkflowExecutions = CountClosedWorkflowExecutionsResponse

    request = get
    response _ = jsonResponse
