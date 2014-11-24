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

-- Module      : Network.AWS.SWF.CountOpenWorkflowExecutions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the number of open workflow executions within the given domain that
-- meet the specified filtering criteria. Access Control You can use IAM
-- policies to control this action's access to Amazon SWF resources as
-- follows: Use a 'Resource' element with the domain name to limit the action
-- to only specified domains. Use an 'Action' element to allow or deny
-- permission to call this action. Constrain the following parameters by using
-- a 'Condition' element with the appropriate keys. 'tagFilter.tag': String
-- constraint. The key is 'swf:tagFilter.tag'. 'typeFilter.name': String
-- constraint. The key is 'swf:typeFilter.name'. 'typeFilter.version': String
-- constraint. The key is 'swf:typeFilter.version'. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- 'OperationNotPermitted'. For details and example IAM policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html
-- Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_CountOpenWorkflowExecutions.html>
module Network.AWS.SWF.CountOpenWorkflowExecutions
    (
    -- * Request
      CountOpenWorkflowExecutions
    -- ** Request constructor
    , countOpenWorkflowExecutions
    -- ** Request lenses
    , coweDomain
    , coweExecutionFilter
    , coweStartTimeFilter
    , coweTagFilter
    , coweTypeFilter

    -- * Response
    , CountOpenWorkflowExecutionsResponse
    -- ** Response constructor
    , countOpenWorkflowExecutionsResponse
    -- ** Response lenses
    , cowerCount
    , cowerTruncated
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data CountOpenWorkflowExecutions = CountOpenWorkflowExecutions
    { _coweDomain          :: Text
    , _coweExecutionFilter :: Maybe WorkflowExecutionFilter
    , _coweStartTimeFilter :: ExecutionTimeFilter
    , _coweTagFilter       :: Maybe TagFilter
    , _coweTypeFilter      :: Maybe WorkflowTypeFilter
    } deriving (Eq, Show)

-- | 'CountOpenWorkflowExecutions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'coweDomain' @::@ 'Text'
--
-- * 'coweExecutionFilter' @::@ 'Maybe' 'WorkflowExecutionFilter'
--
-- * 'coweStartTimeFilter' @::@ 'ExecutionTimeFilter'
--
-- * 'coweTagFilter' @::@ 'Maybe' 'TagFilter'
--
-- * 'coweTypeFilter' @::@ 'Maybe' 'WorkflowTypeFilter'
--
countOpenWorkflowExecutions :: Text -- ^ 'coweDomain'
                            -> ExecutionTimeFilter -- ^ 'coweStartTimeFilter'
                            -> CountOpenWorkflowExecutions
countOpenWorkflowExecutions p1 p2 = CountOpenWorkflowExecutions
    { _coweDomain          = p1
    , _coweStartTimeFilter = p2
    , _coweTypeFilter      = Nothing
    , _coweTagFilter       = Nothing
    , _coweExecutionFilter = Nothing
    }

-- | The name of the domain containing the workflow executions to count.
coweDomain :: Lens' CountOpenWorkflowExecutions Text
coweDomain = lens _coweDomain (\s a -> s { _coweDomain = a })

-- | If specified, only workflow executions matching the 'WorkflowId' in the
-- filter are counted.
coweExecutionFilter :: Lens' CountOpenWorkflowExecutions (Maybe WorkflowExecutionFilter)
coweExecutionFilter =
    lens _coweExecutionFilter (\s a -> s { _coweExecutionFilter = a })

-- | Specifies the start time criteria that workflow executions must meet in
-- order to be counted.
coweStartTimeFilter :: Lens' CountOpenWorkflowExecutions ExecutionTimeFilter
coweStartTimeFilter =
    lens _coweStartTimeFilter (\s a -> s { _coweStartTimeFilter = a })

-- | If specified, only executions that have a tag that matches the filter are
-- counted.
coweTagFilter :: Lens' CountOpenWorkflowExecutions (Maybe TagFilter)
coweTagFilter = lens _coweTagFilter (\s a -> s { _coweTagFilter = a })

-- | Specifies the type of the workflow executions to be counted.
coweTypeFilter :: Lens' CountOpenWorkflowExecutions (Maybe WorkflowTypeFilter)
coweTypeFilter = lens _coweTypeFilter (\s a -> s { _coweTypeFilter = a })

data CountOpenWorkflowExecutionsResponse = CountOpenWorkflowExecutionsResponse
    { _cowerCount     :: Nat
    , _cowerTruncated :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'CountOpenWorkflowExecutionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cowerCount' @::@ 'Natural'
--
-- * 'cowerTruncated' @::@ 'Maybe' 'Bool'
--
countOpenWorkflowExecutionsResponse :: Natural -- ^ 'cowerCount'
                                    -> CountOpenWorkflowExecutionsResponse
countOpenWorkflowExecutionsResponse p1 = CountOpenWorkflowExecutionsResponse
    { _cowerCount     = withIso _Nat (const id) p1
    , _cowerTruncated = Nothing
    }

-- | The number of workflow executions.
cowerCount :: Lens' CountOpenWorkflowExecutionsResponse Natural
cowerCount = lens _cowerCount (\s a -> s { _cowerCount = a }) . _Nat

-- | If set to true, indicates that the actual count was more than the maximum
-- supported by this API and the count returned is the truncated value.
cowerTruncated :: Lens' CountOpenWorkflowExecutionsResponse (Maybe Bool)
cowerTruncated = lens _cowerTruncated (\s a -> s { _cowerTruncated = a })

instance ToPath CountOpenWorkflowExecutions where
    toPath = const "/"

instance ToQuery CountOpenWorkflowExecutions where
    toQuery = const mempty

instance ToHeaders CountOpenWorkflowExecutions

instance ToJSON CountOpenWorkflowExecutions where
    toJSON CountOpenWorkflowExecutions{..} = object
        [ "domain"          .= _coweDomain
        , "startTimeFilter" .= _coweStartTimeFilter
        , "typeFilter"      .= _coweTypeFilter
        , "tagFilter"       .= _coweTagFilter
        , "executionFilter" .= _coweExecutionFilter
        ]

instance AWSRequest CountOpenWorkflowExecutions where
    type Sv CountOpenWorkflowExecutions = SWF
    type Rs CountOpenWorkflowExecutions = CountOpenWorkflowExecutionsResponse

    request  = post "CountOpenWorkflowExecutions"
    response = jsonResponse

instance FromJSON CountOpenWorkflowExecutionsResponse where
    parseJSON = withObject "CountOpenWorkflowExecutionsResponse" $ \o -> CountOpenWorkflowExecutionsResponse
        <$> o .:  "count"
        <*> o .:? "truncated"
