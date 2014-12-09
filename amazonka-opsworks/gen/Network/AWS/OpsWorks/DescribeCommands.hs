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

-- Module      : Network.AWS.OpsWorks.DescribeCommands
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

-- | Describes the results of specified commands.
--
-- You must specify at least one of the parameters.
--
-- Required Permissions: To use this action, an IAM user must have a Show,
-- Deploy, or Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeCommands.html>
module Network.AWS.OpsWorks.DescribeCommands
    (
    -- * Request
      DescribeCommands
    -- ** Request constructor
    , describeCommands
    -- ** Request lenses
    , dcCommandIds
    , dcDeploymentId
    , dcInstanceId

    -- * Response
    , DescribeCommandsResponse
    -- ** Response constructor
    , describeCommandsResponse
    -- ** Response lenses
    , dcrCommands
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data DescribeCommands = DescribeCommands
    { _dcCommandIds   :: List "InstanceIds" Text
    , _dcDeploymentId :: Maybe Text
    , _dcInstanceId   :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeCommands' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcCommandIds' @::@ ['Text']
--
-- * 'dcDeploymentId' @::@ 'Maybe' 'Text'
--
-- * 'dcInstanceId' @::@ 'Maybe' 'Text'
--
describeCommands :: DescribeCommands
describeCommands = DescribeCommands
    { _dcDeploymentId = Nothing
    , _dcInstanceId   = Nothing
    , _dcCommandIds   = mempty
    }

-- | An array of command IDs. If you include this parameter, 'DescribeCommands'
-- returns a description of the specified commands. Otherwise, it returns a
-- description of every command.
dcCommandIds :: Lens' DescribeCommands [Text]
dcCommandIds = lens _dcCommandIds (\s a -> s { _dcCommandIds = a }) . _List

-- | The deployment ID. If you include this parameter, 'DescribeCommands' returns a
-- description of the commands associated with the specified deployment.
dcDeploymentId :: Lens' DescribeCommands (Maybe Text)
dcDeploymentId = lens _dcDeploymentId (\s a -> s { _dcDeploymentId = a })

-- | The instance ID. If you include this parameter, 'DescribeCommands' returns a
-- description of the commands associated with the specified instance.
dcInstanceId :: Lens' DescribeCommands (Maybe Text)
dcInstanceId = lens _dcInstanceId (\s a -> s { _dcInstanceId = a })

newtype DescribeCommandsResponse = DescribeCommandsResponse
    { _dcrCommands :: List "Commands" Command
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeCommandsResponse where
    type Item DescribeCommandsResponse = Command

    fromList = DescribeCommandsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dcrCommands

-- | 'DescribeCommandsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrCommands' @::@ ['Command']
--
describeCommandsResponse :: DescribeCommandsResponse
describeCommandsResponse = DescribeCommandsResponse
    { _dcrCommands = mempty
    }

-- | An array of 'Command' objects that describe each of the specified commands.
dcrCommands :: Lens' DescribeCommandsResponse [Command]
dcrCommands = lens _dcrCommands (\s a -> s { _dcrCommands = a }) . _List

instance ToPath DescribeCommands where
    toPath = const "/"

instance ToQuery DescribeCommands where
    toQuery = const mempty

instance ToHeaders DescribeCommands

instance ToJSON DescribeCommands where
    toJSON DescribeCommands{..} = object
        [ "DeploymentId" .= _dcDeploymentId
        , "InstanceId"   .= _dcInstanceId
        , "CommandIds"   .= _dcCommandIds
        ]

instance AWSRequest DescribeCommands where
    type Sv DescribeCommands = OpsWorks
    type Rs DescribeCommands = DescribeCommandsResponse

    request  = post "DescribeCommands"
    response = jsonResponse

instance FromJSON DescribeCommandsResponse where
    parseJSON = withObject "DescribeCommandsResponse" $ \o -> DescribeCommandsResponse
        <$> o .:? "Commands" .!= mempty
