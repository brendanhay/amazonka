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

-- Module      : Network.AWS.SWF.GetWorkflowExecutionHistory
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

-- | Returns the history of the specified workflow execution. The results may be
-- split into multiple pages. To retrieve subsequent pages, make the call again
-- using the 'nextPageToken' returned by the initial call.
--
-- This operation is eventually consistent. The results are best effort and may
-- not exactly reflect recent updates and changes. Access Control
--
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows:
--
-- Use a 'Resource' element with the domain name to limit the action to only
-- specified domains. Use an 'Action' element to allow or deny permission to call
-- this action. You cannot use an IAM policy to constrain this action's
-- parameters.  If the caller does not have sufficient permissions to invoke the
-- action, or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute's cause parameter will be set to
-- OPERATION_NOT_PERMITTED. For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAMto Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_GetWorkflowExecutionHistory.html>
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
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data GetWorkflowExecutionHistory = GetWorkflowExecutionHistory
    { _gwehDomain          :: Text
    , _gwehExecution       :: WorkflowExecution
    , _gwehMaximumPageSize :: Maybe Nat
    , _gwehNextPageToken   :: Maybe Text
    , _gwehReverseOrder    :: Maybe Bool
    } deriving (Eq, Read, Show)

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

-- | The maximum number of results that will be returned per call. 'nextPageToken'
-- can be used to obtain futher pages of results. The default is 100, which is
-- the maximum allowed page size. You can, however, specify a page size /smaller/
-- than 100.
--
-- This is an upper limit only; the actual number of results returned per call
-- may be fewer than the specified maximum.
gwehMaximumPageSize :: Lens' GetWorkflowExecutionHistory (Maybe Natural)
gwehMaximumPageSize =
    lens _gwehMaximumPageSize (\s a -> s { _gwehMaximumPageSize = a })
        . mapping _Nat

-- | If a 'NextPageToken' was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again using
-- the returned token in 'nextPageToken'. Keep all other arguments unchanged.
--
-- The configured 'maximumPageSize' determines how many results can be returned
-- in a single call.
gwehNextPageToken :: Lens' GetWorkflowExecutionHistory (Maybe Text)
gwehNextPageToken =
    lens _gwehNextPageToken (\s a -> s { _gwehNextPageToken = a })

-- | When set to 'true', returns the events in reverse order. By default the results
-- are returned in ascending order of the 'eventTimeStamp' of the events.
gwehReverseOrder :: Lens' GetWorkflowExecutionHistory (Maybe Bool)
gwehReverseOrder = lens _gwehReverseOrder (\s a -> s { _gwehReverseOrder = a })

data GetWorkflowExecutionHistoryResponse = GetWorkflowExecutionHistoryResponse
    { _gwehrEvents        :: List "events" HistoryEvent
    , _gwehrNextPageToken :: Maybe Text
    } deriving (Eq, Read, Show)

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
gwehrEvents = lens _gwehrEvents (\s a -> s { _gwehrEvents = a }) . _List

-- | If a 'NextPageToken' was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again using
-- the returned token in 'nextPageToken'. Keep all other arguments unchanged.
--
-- The configured 'maximumPageSize' determines how many results can be returned
-- in a single call.
gwehrNextPageToken :: Lens' GetWorkflowExecutionHistoryResponse (Maybe Text)
gwehrNextPageToken =
    lens _gwehrNextPageToken (\s a -> s { _gwehrNextPageToken = a })

instance ToPath GetWorkflowExecutionHistory where
    toPath = const "/"

instance ToQuery GetWorkflowExecutionHistory where
    toQuery = const mempty

instance ToHeaders GetWorkflowExecutionHistory

instance ToJSON GetWorkflowExecutionHistory where
    toJSON GetWorkflowExecutionHistory{..} = object
        [ "domain"          .= _gwehDomain
        , "execution"       .= _gwehExecution
        , "nextPageToken"   .= _gwehNextPageToken
        , "maximumPageSize" .= _gwehMaximumPageSize
        , "reverseOrder"    .= _gwehReverseOrder
        ]

instance AWSRequest GetWorkflowExecutionHistory where
    type Sv GetWorkflowExecutionHistory = SWF
    type Rs GetWorkflowExecutionHistory = GetWorkflowExecutionHistoryResponse

    request  = post "GetWorkflowExecutionHistory"
    response = jsonResponse

instance FromJSON GetWorkflowExecutionHistoryResponse where
    parseJSON = withObject "GetWorkflowExecutionHistoryResponse" $ \o -> GetWorkflowExecutionHistoryResponse
        <$> o .:? "events" .!= mempty
        <*> o .:? "nextPageToken"

instance AWSPager GetWorkflowExecutionHistory where
    page rq rs
        | stop (rs ^. gwehrNextPageToken) = Nothing
        | otherwise = (\x -> rq & gwehNextPageToken ?~ x)
            <$> (rs ^. gwehrNextPageToken)
