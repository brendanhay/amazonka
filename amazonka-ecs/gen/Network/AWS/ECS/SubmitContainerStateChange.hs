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

-- Module      : Network.AWS.ECS.SubmitContainerStateChange
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
-- Sent to acknowledge that a container changed states.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_SubmitContainerStateChange.html>
module Network.AWS.ECS.SubmitContainerStateChange
    (
    -- * Request
      SubmitContainerStateChange
    -- ** Request constructor
    , submitContainerStateChange
    -- ** Request lenses
    , scscCluster
    , scscContainerName
    , scscExitCode
    , scscNetworkBindings
    , scscReason
    , scscStatus
    , scscTask

    -- * Response
    , SubmitContainerStateChangeResponse
    -- ** Response constructor
    , submitContainerStateChangeResponse
    -- ** Response lenses
    , scscrAcknowledgment
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ECS.Types
import qualified GHC.Exts

data SubmitContainerStateChange = SubmitContainerStateChange
    { _scscCluster         :: Maybe Text
    , _scscContainerName   :: Maybe Text
    , _scscExitCode        :: Maybe Int
    , _scscNetworkBindings :: List "member" NetworkBinding
    , _scscReason          :: Maybe Text
    , _scscStatus          :: Maybe Text
    , _scscTask            :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'SubmitContainerStateChange' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scscCluster' @::@ 'Maybe' 'Text'
--
-- * 'scscContainerName' @::@ 'Maybe' 'Text'
--
-- * 'scscExitCode' @::@ 'Maybe' 'Int'
--
-- * 'scscNetworkBindings' @::@ ['NetworkBinding']
--
-- * 'scscReason' @::@ 'Maybe' 'Text'
--
-- * 'scscStatus' @::@ 'Maybe' 'Text'
--
-- * 'scscTask' @::@ 'Maybe' 'Text'
--
submitContainerStateChange :: SubmitContainerStateChange
submitContainerStateChange = SubmitContainerStateChange
    { _scscCluster         = Nothing
    , _scscTask            = Nothing
    , _scscContainerName   = Nothing
    , _scscStatus          = Nothing
    , _scscExitCode        = Nothing
    , _scscReason          = Nothing
    , _scscNetworkBindings = mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts
-- the container.
scscCluster :: Lens' SubmitContainerStateChange (Maybe Text)
scscCluster = lens _scscCluster (\s a -> s { _scscCluster = a })

-- | The name of the container.
scscContainerName :: Lens' SubmitContainerStateChange (Maybe Text)
scscContainerName =
    lens _scscContainerName (\s a -> s { _scscContainerName = a })

-- | The exit code returned for the state change request.
scscExitCode :: Lens' SubmitContainerStateChange (Maybe Int)
scscExitCode = lens _scscExitCode (\s a -> s { _scscExitCode = a })

-- | The network bindings of the container.
scscNetworkBindings :: Lens' SubmitContainerStateChange [NetworkBinding]
scscNetworkBindings =
    lens _scscNetworkBindings (\s a -> s { _scscNetworkBindings = a })
        . _List

-- | The reason for the state change request.
scscReason :: Lens' SubmitContainerStateChange (Maybe Text)
scscReason = lens _scscReason (\s a -> s { _scscReason = a })

-- | The status of the state change request.
scscStatus :: Lens' SubmitContainerStateChange (Maybe Text)
scscStatus = lens _scscStatus (\s a -> s { _scscStatus = a })

-- | The task UUID or full Amazon Resource Name (ARN) of the task that hosts the
-- container.
scscTask :: Lens' SubmitContainerStateChange (Maybe Text)
scscTask = lens _scscTask (\s a -> s { _scscTask = a })

newtype SubmitContainerStateChangeResponse = SubmitContainerStateChangeResponse
    { _scscrAcknowledgment :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'SubmitContainerStateChangeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scscrAcknowledgment' @::@ 'Maybe' 'Text'
--
submitContainerStateChangeResponse :: SubmitContainerStateChangeResponse
submitContainerStateChangeResponse = SubmitContainerStateChangeResponse
    { _scscrAcknowledgment = Nothing
    }

-- | Acknowledgement of the state change.
scscrAcknowledgment :: Lens' SubmitContainerStateChangeResponse (Maybe Text)
scscrAcknowledgment =
    lens _scscrAcknowledgment (\s a -> s { _scscrAcknowledgment = a })

instance ToPath SubmitContainerStateChange where
    toPath = const "/"

instance ToQuery SubmitContainerStateChange where
    toQuery SubmitContainerStateChange{..} = mconcat
        [ "cluster"         =? _scscCluster
        , "containerName"   =? _scscContainerName
        , "exitCode"        =? _scscExitCode
        , "networkBindings" =? _scscNetworkBindings
        , "reason"          =? _scscReason
        , "status"          =? _scscStatus
        , "task"            =? _scscTask
        ]

instance ToHeaders SubmitContainerStateChange

instance AWSRequest SubmitContainerStateChange where
    type Sv SubmitContainerStateChange = ECS
    type Rs SubmitContainerStateChange = SubmitContainerStateChangeResponse

    request  = post "SubmitContainerStateChange"
    response = xmlResponse

instance FromXML SubmitContainerStateChangeResponse where
    parseXML = withElement "SubmitContainerStateChangeResult" $ \x -> SubmitContainerStateChangeResponse
        <$> x .@? "acknowledgment"
