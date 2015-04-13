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

-- Module      : Network.AWS.ECS.SubmitTaskStateChange
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

-- | This action is only used by the Amazon EC2 Container Service agent, and it is
-- not intended for use outside of the agent.
--
-- Sent to acknowledge that a task changed states.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_SubmitTaskStateChange.html>
module Network.AWS.ECS.SubmitTaskStateChange
    (
    -- * Request
      SubmitTaskStateChange
    -- ** Request constructor
    , submitTaskStateChange
    -- ** Request lenses
    , stscCluster
    , stscReason
    , stscStatus
    , stscTask

    -- * Response
    , SubmitTaskStateChangeResponse
    -- ** Response constructor
    , submitTaskStateChangeResponse
    -- ** Response lenses
    , stscrAcknowledgment
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data SubmitTaskStateChange = SubmitTaskStateChange
    { _stscCluster :: Maybe Text
    , _stscReason  :: Maybe Text
    , _stscStatus  :: Maybe Text
    , _stscTask    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'SubmitTaskStateChange' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stscCluster' @::@ 'Maybe' 'Text'
--
-- * 'stscReason' @::@ 'Maybe' 'Text'
--
-- * 'stscStatus' @::@ 'Maybe' 'Text'
--
-- * 'stscTask' @::@ 'Maybe' 'Text'
--
submitTaskStateChange :: SubmitTaskStateChange
submitTaskStateChange = SubmitTaskStateChange
    { _stscCluster = Nothing
    , _stscTask    = Nothing
    , _stscStatus  = Nothing
    , _stscReason  = Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts
-- the task.
stscCluster :: Lens' SubmitTaskStateChange (Maybe Text)
stscCluster = lens _stscCluster (\s a -> s { _stscCluster = a })

-- | The reason for the state change request.
stscReason :: Lens' SubmitTaskStateChange (Maybe Text)
stscReason = lens _stscReason (\s a -> s { _stscReason = a })

-- | The status of the state change request.
stscStatus :: Lens' SubmitTaskStateChange (Maybe Text)
stscStatus = lens _stscStatus (\s a -> s { _stscStatus = a })

-- | The task UUID or full Amazon Resource Name (ARN) of the task in the state
-- change request.
stscTask :: Lens' SubmitTaskStateChange (Maybe Text)
stscTask = lens _stscTask (\s a -> s { _stscTask = a })

newtype SubmitTaskStateChangeResponse = SubmitTaskStateChangeResponse
    { _stscrAcknowledgment :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'SubmitTaskStateChangeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stscrAcknowledgment' @::@ 'Maybe' 'Text'
--
submitTaskStateChangeResponse :: SubmitTaskStateChangeResponse
submitTaskStateChangeResponse = SubmitTaskStateChangeResponse
    { _stscrAcknowledgment = Nothing
    }

-- | Acknowledgement of the state change.
stscrAcknowledgment :: Lens' SubmitTaskStateChangeResponse (Maybe Text)
stscrAcknowledgment =
    lens _stscrAcknowledgment (\s a -> s { _stscrAcknowledgment = a })

instance ToPath SubmitTaskStateChange where
    toPath = const "/"

instance ToQuery SubmitTaskStateChange where
    toQuery = const mempty

instance ToHeaders SubmitTaskStateChange

instance ToJSON SubmitTaskStateChange where
    toJSON SubmitTaskStateChange{..} = object
        [ "cluster" .= _stscCluster
        , "task"    .= _stscTask
        , "status"  .= _stscStatus
        , "reason"  .= _stscReason
        ]

instance AWSRequest SubmitTaskStateChange where
    type Sv SubmitTaskStateChange = ECS
    type Rs SubmitTaskStateChange = SubmitTaskStateChangeResponse

    request  = post "SubmitTaskStateChange"
    response = jsonResponse

instance FromJSON SubmitTaskStateChangeResponse where
    parseJSON = withObject "SubmitTaskStateChangeResponse" $ \o -> SubmitTaskStateChangeResponse
        <$> o .:? "acknowledgment"
