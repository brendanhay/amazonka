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

-- Module      : Network.AWS.OpsWorks.StopInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Stops a specified instance. When you stop a standard instance, the data
-- disappears and must be reinstalled when you restart the instance. You can
-- stop an Amazon EBS-backed instance without losing data. For more
-- information, see Starting, Stopping, and Rebooting Instances. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_StopInstance.html>
module Network.AWS.OpsWorks.StopInstance
    (
    -- * Request
      StopInstance
    -- ** Request constructor
    , stopInstance
    -- ** Request lenses
    , siInstanceId

    -- * Response
    , StopInstanceResponse
    -- ** Response constructor
    , stopInstanceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype StopInstance = StopInstance
    { _siInstanceId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'StopInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'siInstanceId' @::@ 'Text'
--
stopInstance :: Text -- ^ 'siInstanceId'
             -> StopInstance
stopInstance p1 = StopInstance
    { _siInstanceId = p1
    }

-- | The instance ID.
siInstanceId :: Lens' StopInstance Text
siInstanceId = lens _siInstanceId (\s a -> s { _siInstanceId = a })

data StopInstanceResponse = StopInstanceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'StopInstanceResponse' constructor.
stopInstanceResponse :: StopInstanceResponse
stopInstanceResponse = StopInstanceResponse

instance ToPath StopInstance where
    toPath = const "/"

instance ToQuery StopInstance where
    toQuery = const mempty

instance ToHeaders StopInstance

instance ToJSON StopInstance where
    toJSON StopInstance{..} = object
        [ "InstanceId" .= _siInstanceId
        ]

instance AWSRequest StopInstance where
    type Sv StopInstance = OpsWorks
    type Rs StopInstance = StopInstanceResponse

    request  = post "StopInstance"
    response = nullResponse StopInstanceResponse
