{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.SWF.GetWorkflowExecutionHistory
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the history of the specified workflow execution. The results may be
-- split into multiple pages. To retrieve subsequent pages, make the call
-- again using the nextPageToken returned by the initial call. Access Control
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows: Use a Resource element with the domain name to limit
-- the action to only specified domains. Use an Action element to allow or
-- deny permission to call this action. You cannot use an IAM policy to
-- constrain this action's parameters. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows.
module Network.AWS.SWF.GetWorkflowExecutionHistory
    (
    -- * Request
      GetWorkflowExecutionHistory
    -- ** Request constructor
    , getWorkflowExecutionHistory
    -- ** Request lenses
    , gwehDomain
    , gwehExecution
    , gwehMaximumPageSize
    , gwehNextPageToken
    , gwehReverseOrder

    -- * Response
    , GetWorkflowExecutionHistoryResponse
    -- ** Response constructor
    , getWorkflowExecutionHistoryResponse
    -- ** Response lenses
    , gwehrEvents
    , gwehrNextPageToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.SWF.Types
import qualified GHC.Exts

data GetWorkflowExecutionHistory = GetWorkflowExecutionHistory
    { _gwehDomain          :: Text
    , _gwehExecution       :: WorkflowExecution
    , _gwehMaximumPageSize :: Maybe Natural
    , _gwehNextPageToken   :: Maybe Text
    , _gwehReverseOrder    :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'GetWorkflowExecutionHistory' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gwehDomain' @::@ 'Text'
--
-- * 'gwehExecution' @::@ 'WorkflowExecution'
--
-- * 'gwehMaximumPageSize' @::@ 'Maybe' 'Natural'
--
-- * 'gwehNextPageToken' @::@ 'Maybe' 'Text'
--
-- * 'gwehReverseOrder' @::@ 'Maybe' 'Bool'
--
getWorkflowExecutionHistory :: Text -- ^ 'gwehDomain'
                            -> WorkflowExecution -- ^ 'gwehExecution'
                            -> GetWorkflowExecutionHistory
getWorkflowExecutionHistory p1 p2 = GetWorkflowExecutionHistory
    { _gwehDomain          = p1
    , _gwehExecution       = p2
    , _gwehNextPageToken   = Nothing
    , _gwehMaximumPageSize = Nothing
    , _gwehReverseOrder    = Nothing
    }

-- | The name of the domain containing the workflow execution.
gwehDomain :: Lens' GetWorkflowExecutionHistory Text
gwehDomain = lens _gwehDomain (\s a -> s { _gwehDomain = a })

-- | Specifies the workflow execution for which to return the history.
gwehExecution :: Lens' GetWorkflowExecutionHistory WorkflowExecution
gwehExecution = lens _gwehExecution (\s a -> s { _gwehExecution = a })

-- | Specifies the maximum number of history events returned in one page. The
-- next page in the result is identified by the NextPageToken returned. By
-- default 100 history events are returned in a page but the caller can
-- override this value to a page size smaller than the default. You cannot
-- specify a page size larger than 100. Note that the number of events may
-- be less than the maxiumum page size, in which case, the returned page
-- will have fewer results than the maximumPageSize specified.
gwehMaximumPageSize :: Lens' GetWorkflowExecutionHistory (Maybe Natural)
gwehMaximumPageSize =
    lens _gwehMaximumPageSize (\s a -> s { _gwehMaximumPageSize = a })

-- | If a NextPageToken is returned, the result has more than one pages. To
-- get the next page, repeat the call and specify the nextPageToken with all
-- other arguments unchanged.
gwehNextPageToken :: Lens' GetWorkflowExecutionHistory (Maybe Text)
gwehNextPageToken =
    lens _gwehNextPageToken (\s a -> s { _gwehNextPageToken = a })

-- | When set to true, returns the events in reverse order. By default the
-- results are returned in ascending order of the eventTimeStamp of the
-- events.
gwehReverseOrder :: Lens' GetWorkflowExecutionHistory (Maybe Bool)
gwehReverseOrder = lens _gwehReverseOrder (\s a -> s { _gwehReverseOrder = a })

instance ToPath GetWorkflowExecutionHistory where
    toPath = const "/"

instance ToQuery GetWorkflowExecutionHistory where
    toQuery = const mempty

instance ToHeaders GetWorkflowExecutionHistory

instance ToBody GetWorkflowExecutionHistory where
    toBody = toBody . encode . _gwehDomain

data GetWorkflowExecutionHistoryResponse = GetWorkflowExecutionHistoryResponse
    { _gwehrEvents        :: [HistoryEvent]
    , _gwehrNextPageToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetWorkflowExecutionHistoryResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gwehrEvents' @::@ ['HistoryEvent']
--
-- * 'gwehrNextPageToken' @::@ 'Maybe' 'Text'
--
getWorkflowExecutionHistoryResponse :: GetWorkflowExecutionHistoryResponse
getWorkflowExecutionHistoryResponse = GetWorkflowExecutionHistoryResponse
    { _gwehrEvents        = mempty
    , _gwehrNextPageToken = Nothing
    }

-- | The list of history events.
gwehrEvents :: Lens' GetWorkflowExecutionHistoryResponse [HistoryEvent]
gwehrEvents = lens _gwehrEvents (\s a -> s { _gwehrEvents = a })

-- | The token for the next page. If set, the history consists of more than
-- one page and the next page can be retrieved by repeating the request with
-- this token and all other arguments unchanged.
gwehrNextPageToken :: Lens' GetWorkflowExecutionHistoryResponse (Maybe Text)
gwehrNextPageToken =
    lens _gwehrNextPageToken (\s a -> s { _gwehrNextPageToken = a })

instance AWSRequest GetWorkflowExecutionHistory where
    type Sv GetWorkflowExecutionHistory = SWF
    type Rs GetWorkflowExecutionHistory = GetWorkflowExecutionHistoryResponse

    request  = post
    response = jsonResponse $ \h o -> GetWorkflowExecutionHistoryResponse
        <$> o .: "events"
        <*> o .: "nextPageToken"
