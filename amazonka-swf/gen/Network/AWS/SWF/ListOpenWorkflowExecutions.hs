{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.ListOpenWorkflowExecutions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of open workflow executions in the specified domain that meet
-- the filtering criteria. The results may be split into multiple pages. To
-- retrieve subsequent pages, make the call again using the nextPageToken
-- returned by the initial call.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes. Access Control
--
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows:
--
-- Use a 'Resource' element with the domain name to limit the action to only
-- specified domains. Use an 'Action' element to allow or deny permission to call
-- this action. Constrain the following parameters by using a 'Condition' element
-- with the appropriate keys.  'tagFilter.tag': String constraint. The key is 'swf:tagFilter.tag'. 'typeFilter.name': String constraint. The key is 'swf:typeFilter.name'. 'typeFilter.version': String constraint. The key is 'swf:typeFilter.version'.    If the caller does
-- not have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails. The associated
-- event attribute's cause parameter will be set to OPERATION_NOT_PERMITTED. For
-- details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to AmazonSWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_ListOpenWorkflowExecutions.html>
module Network.AWS.SWF.ListOpenWorkflowExecutions
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
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data ListOpenWorkflowExecutions = ListOpenWorkflowExecutions
    { _loweDomain          :: Text
    , _loweExecutionFilter :: Maybe WorkflowExecutionFilter
    , _loweMaximumPageSize :: Maybe Nat
    , _loweNextPageToken   :: Maybe Text
    , _loweReverseOrder    :: Maybe Bool
    , _loweStartTimeFilter :: ExecutionTimeFilter
    , _loweTagFilter       :: Maybe TagFilter
    , _loweTypeFilter      :: Maybe WorkflowTypeFilter
    } deriving (Eq, Read, Show)

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

-- | If specified, only workflow executions matching the workflow id specified in
-- the filter are returned.
--
-- 'executionFilter', 'typeFilter' and 'tagFilter' are mutually exclusive. You can
-- specify at most one of these in a request.
loweExecutionFilter :: Lens' ListOpenWorkflowExecutions (Maybe WorkflowExecutionFilter)
loweExecutionFilter =
    lens _loweExecutionFilter (\s a -> s { _loweExecutionFilter = a })

-- | The maximum number of results that will be returned per call. 'nextPageToken'
-- can be used to obtain futher pages of results. The default is 100, which is
-- the maximum allowed page size. You can, however, specify a page size /smaller/
-- than 100.
--
-- This is an upper limit only; the actual number of results returned per call
-- may be fewer than the specified maximum.
loweMaximumPageSize :: Lens' ListOpenWorkflowExecutions (Maybe Natural)
loweMaximumPageSize =
    lens _loweMaximumPageSize (\s a -> s { _loweMaximumPageSize = a })
        . mapping _Nat

-- | If a 'NextPageToken' was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again using
-- the returned token in 'nextPageToken'. Keep all other arguments unchanged.
--
-- The configured 'maximumPageSize' determines how many results can be returned
-- in a single call.
loweNextPageToken :: Lens' ListOpenWorkflowExecutions (Maybe Text)
loweNextPageToken =
    lens _loweNextPageToken (\s a -> s { _loweNextPageToken = a })

-- | When set to 'true', returns the results in reverse order. By default the
-- results are returned in descending order of the start time of the executions.
loweReverseOrder :: Lens' ListOpenWorkflowExecutions (Maybe Bool)
loweReverseOrder = lens _loweReverseOrder (\s a -> s { _loweReverseOrder = a })

-- | Workflow executions are included in the returned results based on whether
-- their start times are within the range specified by this filter.
loweStartTimeFilter :: Lens' ListOpenWorkflowExecutions ExecutionTimeFilter
loweStartTimeFilter =
    lens _loweStartTimeFilter (\s a -> s { _loweStartTimeFilter = a })

-- | If specified, only executions that have the matching tag are listed.
--
-- 'executionFilter', 'typeFilter' and 'tagFilter' are mutually exclusive. You can
-- specify at most one of these in a request.
loweTagFilter :: Lens' ListOpenWorkflowExecutions (Maybe TagFilter)
loweTagFilter = lens _loweTagFilter (\s a -> s { _loweTagFilter = a })

-- | If specified, only executions of the type specified in the filter are
-- returned.
--
-- 'executionFilter', 'typeFilter' and 'tagFilter' are mutually exclusive. You can
-- specify at most one of these in a request.
loweTypeFilter :: Lens' ListOpenWorkflowExecutions (Maybe WorkflowTypeFilter)
loweTypeFilter = lens _loweTypeFilter (\s a -> s { _loweTypeFilter = a })

data ListOpenWorkflowExecutionsResponse = ListOpenWorkflowExecutionsResponse
    { _lowerExecutionInfos :: List "executionInfos" WorkflowExecutionInfo
    , _lowerNextPageToken  :: Maybe Text
    } deriving (Eq, Read, Show)

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
        . _List

-- | If a 'NextPageToken' was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again using
-- the returned token in 'nextPageToken'. Keep all other arguments unchanged.
--
-- The configured 'maximumPageSize' determines how many results can be returned
-- in a single call.
lowerNextPageToken :: Lens' ListOpenWorkflowExecutionsResponse (Maybe Text)
lowerNextPageToken =
    lens _lowerNextPageToken (\s a -> s { _lowerNextPageToken = a })

instance ToPath ListOpenWorkflowExecutions where
    toPath = const "/"

instance ToQuery ListOpenWorkflowExecutions where
    toQuery = const mempty

instance ToHeaders ListOpenWorkflowExecutions

instance ToJSON ListOpenWorkflowExecutions where
    toJSON ListOpenWorkflowExecutions{..} = object
        [ "domain"          .= _loweDomain
        , "startTimeFilter" .= _loweStartTimeFilter
        , "typeFilter"      .= _loweTypeFilter
        , "tagFilter"       .= _loweTagFilter
        , "nextPageToken"   .= _loweNextPageToken
        , "maximumPageSize" .= _loweMaximumPageSize
        , "reverseOrder"    .= _loweReverseOrder
        , "executionFilter" .= _loweExecutionFilter
        ]

instance AWSRequest ListOpenWorkflowExecutions where
    type Sv ListOpenWorkflowExecutions = SWF
    type Rs ListOpenWorkflowExecutions = ListOpenWorkflowExecutionsResponse

    request  = post "ListOpenWorkflowExecutions"
    response = jsonResponse

instance FromJSON ListOpenWorkflowExecutionsResponse where
    parseJSON = withObject "ListOpenWorkflowExecutionsResponse" $ \o -> ListOpenWorkflowExecutionsResponse
        <$> o .:? "executionInfos" .!= mempty
        <*> o .:? "nextPageToken"

instance AWSPager ListOpenWorkflowExecutions where
    page rq rs
        | stop (rq ^. loweNextPageToken) = Nothing
        | otherwise = (\x -> rq & loweNextPageToken ?~ x)
            <$> (rs ^. lowerNextPageToken)
