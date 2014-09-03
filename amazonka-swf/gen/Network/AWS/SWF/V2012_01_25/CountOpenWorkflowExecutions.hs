{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.CountOpenWorkflowExecutions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the number of open workflow executions within the given domain that
-- meet the specified filtering criteria. This operation is eventually
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
-- SWF Workflows. CountOpenWorkflowExecutions Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sat, 14 Jan 2012 23:13:29 GMT X-Amz-Target:
-- SimpleWorkflowService.CountOpenWorkflowExecutions Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=3v6shiGzWukq4KiX/5HFMIUF/w5qajhW4dp+6AKyOtY=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 150 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "startTimeFilter": {"oldestDate": 1325376070, "latestDate":
-- 1356998399}, "tagFilter": {"tag": "ricoh-the-dog"} } HTTP/1.1 200 OK
-- Content-Length: 29 Content-Type: application/json x-amzn-RequestId:
-- 5ea6789e-3f05-11e1-9e8f-57bb03e21482 {"count":1,"truncated":false}.
module Network.AWS.SWF.V2012_01_25.CountOpenWorkflowExecutions
    (
    -- * Request
      CountOpenWorkflowExecutions
    -- ** Request constructor
    , countOpenWorkflowExecutions
    -- ** Request lenses
    , coweiDomain
    , coweiStartTimeFilter
    , coweiTagFilter
    , coweiExecutionFilter
    , coweiTypeFilter

    -- * Response
    , CountOpenWorkflowExecutionsResponse
    -- ** Response lenses
    , wedCount
    , wedTruncated
    ) where

import           Network.AWS.SWF.V2012_01_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CountOpenWorkflowExecutions' request.
countOpenWorkflowExecutions :: Text -- ^ 'coweiDomain'
                            -> ExecutionTimeFilter -- ^ 'coweiStartTimeFilter'
                            -> CountOpenWorkflowExecutions
countOpenWorkflowExecutions p1 p2 = CountOpenWorkflowExecutions
    { _coweiDomain = p1
    , _coweiStartTimeFilter = p2
    , _coweiTagFilter = Nothing
    , _coweiExecutionFilter = Nothing
    , _coweiTypeFilter = Nothing
    }

data CountOpenWorkflowExecutions = CountOpenWorkflowExecutions
    { _coweiDomain :: Text
      -- ^ The name of the domain containing the workflow executions to
      -- count.
    , _coweiStartTimeFilter :: ExecutionTimeFilter
      -- ^ Specifies the start time criteria that workflow executions must
      -- meet in order to be counted.
    , _coweiTagFilter :: Maybe TagFilter
      -- ^ If specified, only executions that have a tag that matches the
      -- filter are counted. executionFilter, typeFilter and tagFilter are
      -- mutually exclusive. You can specify at most one of these in a
      -- request.
    , _coweiExecutionFilter :: Maybe WorkflowExecutionFilter
      -- ^ If specified, only workflow executions matching the WorkflowId in
      -- the filter are counted. executionFilter, typeFilter and tagFilter
      -- are mutually exclusive. You can specify at most one of these in a
      -- request.
    , _coweiTypeFilter :: Maybe WorkflowTypeFilter
      -- ^ Specifies the type of the workflow executions to be counted.
      -- executionFilter, typeFilter and tagFilter are mutually exclusive.
      -- You can specify at most one of these in a request.
    } deriving (Show, Generic)

-- | The name of the domain containing the workflow executions to count.
coweiDomain
    :: Functor f
    => (Text
    -> f (Text))
    -> CountOpenWorkflowExecutions
    -> f CountOpenWorkflowExecutions
coweiDomain f x =
    (\y -> x { _coweiDomain = y })
       <$> f (_coweiDomain x)
{-# INLINE coweiDomain #-}

-- | Specifies the start time criteria that workflow executions must meet in
-- order to be counted.
coweiStartTimeFilter
    :: Functor f
    => (ExecutionTimeFilter
    -> f (ExecutionTimeFilter))
    -> CountOpenWorkflowExecutions
    -> f CountOpenWorkflowExecutions
coweiStartTimeFilter f x =
    (\y -> x { _coweiStartTimeFilter = y })
       <$> f (_coweiStartTimeFilter x)
{-# INLINE coweiStartTimeFilter #-}

-- | If specified, only executions that have a tag that matches the filter are
-- counted. executionFilter, typeFilter and tagFilter are mutually exclusive.
-- You can specify at most one of these in a request.
coweiTagFilter
    :: Functor f
    => (Maybe TagFilter
    -> f (Maybe TagFilter))
    -> CountOpenWorkflowExecutions
    -> f CountOpenWorkflowExecutions
coweiTagFilter f x =
    (\y -> x { _coweiTagFilter = y })
       <$> f (_coweiTagFilter x)
{-# INLINE coweiTagFilter #-}

-- | If specified, only workflow executions matching the WorkflowId in the
-- filter are counted. executionFilter, typeFilter and tagFilter are mutually
-- exclusive. You can specify at most one of these in a request.
coweiExecutionFilter
    :: Functor f
    => (Maybe WorkflowExecutionFilter
    -> f (Maybe WorkflowExecutionFilter))
    -> CountOpenWorkflowExecutions
    -> f CountOpenWorkflowExecutions
coweiExecutionFilter f x =
    (\y -> x { _coweiExecutionFilter = y })
       <$> f (_coweiExecutionFilter x)
{-# INLINE coweiExecutionFilter #-}

-- | Specifies the type of the workflow executions to be counted.
-- executionFilter, typeFilter and tagFilter are mutually exclusive. You can
-- specify at most one of these in a request.
coweiTypeFilter
    :: Functor f
    => (Maybe WorkflowTypeFilter
    -> f (Maybe WorkflowTypeFilter))
    -> CountOpenWorkflowExecutions
    -> f CountOpenWorkflowExecutions
coweiTypeFilter f x =
    (\y -> x { _coweiTypeFilter = y })
       <$> f (_coweiTypeFilter x)
{-# INLINE coweiTypeFilter #-}

instance ToPath CountOpenWorkflowExecutions

instance ToQuery CountOpenWorkflowExecutions

instance ToHeaders CountOpenWorkflowExecutions

instance ToJSON CountOpenWorkflowExecutions

data CountOpenWorkflowExecutionsResponse = CountOpenWorkflowExecutionsResponse
    { _wedCount :: Integer
      -- ^ The number of workflow executions.
    , _wedTruncated :: Maybe Bool
      -- ^ If set to true, indicates that the actual count was more than the
      -- maximum supported by this API and the count returned is the
      -- truncated value.
    } deriving (Show, Generic)

-- | The number of workflow executions.
wedCount
    :: Functor f
    => (Integer
    -> f (Integer))
    -> CountOpenWorkflowExecutionsResponse
    -> f CountOpenWorkflowExecutionsResponse
wedCount f x =
    (\y -> x { _wedCount = y })
       <$> f (_wedCount x)
{-# INLINE wedCount #-}

-- | If set to true, indicates that the actual count was more than the maximum
-- supported by this API and the count returned is the truncated value.
wedTruncated
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CountOpenWorkflowExecutionsResponse
    -> f CountOpenWorkflowExecutionsResponse
wedTruncated f x =
    (\y -> x { _wedTruncated = y })
       <$> f (_wedTruncated x)
{-# INLINE wedTruncated #-}

instance FromJSON CountOpenWorkflowExecutionsResponse

instance AWSRequest CountOpenWorkflowExecutions where
    type Sv CountOpenWorkflowExecutions = SWF
    type Rs CountOpenWorkflowExecutions = CountOpenWorkflowExecutionsResponse

    request = get
    response _ = jsonResponse
