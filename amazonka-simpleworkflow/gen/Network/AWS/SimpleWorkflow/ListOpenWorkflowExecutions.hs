{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SimpleWorkflow.ListOpenWorkflowExecutions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of open workflow executions in the specified domain that
-- meet the filtering criteria. The results may be split into multiple pages.
-- To retrieve subsequent pages, make the call again using the nextPageToken
-- returned by the initial call. Access Control You can use IAM policies to
-- control this action's access to Amazon SWF resources as follows: Use a
-- Resource element with the domain name to limit the action to only specified
-- domains. Use an Action element to allow or deny permission to call this
-- action. Constrain the following parameters by using a Condition element
-- with the appropriate keys. tagFilter.tag: String constraint. The key is
-- swf:tagFilter.tag. typeFilter.name: String constraint. The key is
-- swf:typeFilter.name. typeFilter.version: String constraint. The key is
-- swf:typeFilter.version. If the caller does not have sufficient permissions
-- to invoke the action, or the parameter values fall outside the specified
-- constraints, the action fails by throwing OperationNotPermitted. For
-- details and example IAM policies, see Using IAM to Manage Access to Amazon
-- SWF Workflows.
module Network.AWS.SimpleWorkflow.ListOpenWorkflowExecutions
    (
    -- * Request
      ListOpenWorkflowExecutions
    -- ** Request constructor
    , listOpenWorkflowExecutions
    -- ** Request lenses
    , loweDomain
    , loweExecutionFilter
    , loweMaximumPageSize
    , loweNextPageToken
    , loweReverseOrder
    , loweStartTimeFilter
    , loweTagFilter
    , loweTypeFilter

    -- * Response
    , ListOpenWorkflowExecutionsResponse
    -- ** Response constructor
    , listOpenWorkflowExecutionsResponse
    -- ** Response lenses
    , lowerExecutionInfos
    , lowerNextPageToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.SimpleWorkflow.Types

data ListOpenWorkflowExecutions = ListOpenWorkflowExecutions
    { _loweDomain          :: Text
    , _loweExecutionFilter :: Maybe WorkflowExecutionFilter
    , _loweMaximumPageSize :: Maybe Natural
    , _loweNextPageToken   :: Maybe Text
    , _loweReverseOrder    :: Maybe Bool
    , _loweStartTimeFilter :: ExecutionTimeFilter
    , _loweTagFilter       :: Maybe TagFilter
    , _loweTypeFilter      :: Maybe WorkflowTypeFilter
    } deriving (Eq, Show, Generic)

-- | 'ListOpenWorkflowExecutions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'loweDomain' @::@ 'Text'
--
-- * 'loweExecutionFilter' @::@ 'Maybe' 'WorkflowExecutionFilter'
--
-- * 'loweMaximumPageSize' @::@ 'Maybe' 'Natural'
--
-- * 'loweNextPageToken' @::@ 'Maybe' 'Text'
--
-- * 'loweReverseOrder' @::@ 'Maybe' 'Bool'
--
-- * 'loweStartTimeFilter' @::@ 'ExecutionTimeFilter'
--
-- * 'loweTagFilter' @::@ 'Maybe' 'TagFilter'
--
-- * 'loweTypeFilter' @::@ 'Maybe' 'WorkflowTypeFilter'
--
listOpenWorkflowExecutions :: Text -- ^ 'loweDomain'
                           -> ExecutionTimeFilter -- ^ 'loweStartTimeFilter'
                           -> ListOpenWorkflowExecutions
listOpenWorkflowExecutions p1 p2 = ListOpenWorkflowExecutions
    { _loweDomain          = p1
    , _loweStartTimeFilter = p2
    , _loweTypeFilter      = Nothing
    , _loweTagFilter       = Nothing
    , _loweNextPageToken   = Nothing
    , _loweMaximumPageSize = Nothing
    , _loweReverseOrder    = Nothing
    , _loweExecutionFilter = Nothing
    }

-- | The name of the domain that contains the workflow executions to list.
loweDomain :: Lens' ListOpenWorkflowExecutions Text
loweDomain = lens _loweDomain (\s a -> s { _loweDomain = a })

-- | If specified, only workflow executions matching the workflow id specified
-- in the filter are returned.
loweExecutionFilter :: Lens' ListOpenWorkflowExecutions (Maybe WorkflowExecutionFilter)
loweExecutionFilter =
    lens _loweExecutionFilter (\s a -> s { _loweExecutionFilter = a })

-- | The maximum number of results returned in each page. The default is 100,
-- but the caller can override this value to a page size smaller than the
-- default. You cannot specify a page size greater than 100. Note that the
-- number of executions may be less than the maxiumum page size, in which
-- case, the returned page will have fewer results than the maximumPageSize
-- specified.
loweMaximumPageSize :: Lens' ListOpenWorkflowExecutions (Maybe Natural)
loweMaximumPageSize =
    lens _loweMaximumPageSize (\s a -> s { _loweMaximumPageSize = a })

-- | If on a previous call to this method a NextPageToken was returned, the
-- results are being paginated. To get the next page of results, repeat the
-- call with the returned token and all other arguments unchanged.
loweNextPageToken :: Lens' ListOpenWorkflowExecutions (Maybe Text)
loweNextPageToken =
    lens _loweNextPageToken (\s a -> s { _loweNextPageToken = a })

-- | When set to true, returns the results in reverse order. By default the
-- results are returned in descending order of the start time of the
-- executions.
loweReverseOrder :: Lens' ListOpenWorkflowExecutions (Maybe Bool)
loweReverseOrder = lens _loweReverseOrder (\s a -> s { _loweReverseOrder = a })

-- | Workflow executions are included in the returned results based on whether
-- their start times are within the range specified by this filter.
loweStartTimeFilter :: Lens' ListOpenWorkflowExecutions ExecutionTimeFilter
loweStartTimeFilter =
    lens _loweStartTimeFilter (\s a -> s { _loweStartTimeFilter = a })

-- | If specified, only executions that have the matching tag are listed.
loweTagFilter :: Lens' ListOpenWorkflowExecutions (Maybe TagFilter)
loweTagFilter = lens _loweTagFilter (\s a -> s { _loweTagFilter = a })

-- | If specified, only executions of the type specified in the filter are
-- returned.
loweTypeFilter :: Lens' ListOpenWorkflowExecutions (Maybe WorkflowTypeFilter)
loweTypeFilter = lens _loweTypeFilter (\s a -> s { _loweTypeFilter = a })

instance ToPath ListOpenWorkflowExecutions where
    toPath = const "/"

instance ToQuery ListOpenWorkflowExecutions where
    toQuery = const mempty

instance ToHeaders ListOpenWorkflowExecutions

instance ToBody ListOpenWorkflowExecutions where
    toBody = toBody . encode . _loweDomain

data ListOpenWorkflowExecutionsResponse = ListOpenWorkflowExecutionsResponse
    { _lowerExecutionInfos :: [WorkflowExecutionInfo]
    , _lowerNextPageToken  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListOpenWorkflowExecutionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lowerExecutionInfos' @::@ ['WorkflowExecutionInfo']
--
-- * 'lowerNextPageToken' @::@ 'Maybe' 'Text'
--
listOpenWorkflowExecutionsResponse :: ListOpenWorkflowExecutionsResponse
listOpenWorkflowExecutionsResponse = ListOpenWorkflowExecutionsResponse
    { _lowerExecutionInfos = mempty
    , _lowerNextPageToken  = Nothing
    }

-- | The list of workflow information structures.
lowerExecutionInfos :: Lens' ListOpenWorkflowExecutionsResponse [WorkflowExecutionInfo]
lowerExecutionInfos =
    lens _lowerExecutionInfos (\s a -> s { _lowerExecutionInfos = a })

-- | The token of the next page in the result. If set, the results have more
-- than one page. The next page can be retrieved by repeating the request
-- with this token and all other arguments unchanged.
lowerNextPageToken :: Lens' ListOpenWorkflowExecutionsResponse (Maybe Text)
lowerNextPageToken =
    lens _lowerNextPageToken (\s a -> s { _lowerNextPageToken = a })

-- FromJSON

instance AWSRequest ListOpenWorkflowExecutions where
    type Sv ListOpenWorkflowExecutions = SimpleWorkflow
    type Rs ListOpenWorkflowExecutions = ListOpenWorkflowExecutionsResponse

    request  = post'
    response = jsonResponse $ \h o -> ListOpenWorkflowExecutionsResponse
        <$> o .: "executionInfos"
        <*> o .: "nextPageToken"
