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

-- Module      : Network.AWS.OpsWorks.StartInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts a specified instance. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping,and Rebooting Instances>.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_StartInstance.html>
module Network.AWS.OpsWorks.StartInstance
    (
    -- * Request
      StartInstance
    -- ** Request constructor
    , startInstance
    -- ** Request lenses
    , si1InstanceId

    -- * Response
    , StartInstanceResponse
    -- ** Response constructor
    , startInstanceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype StartInstance = StartInstance
    { _si1InstanceId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'StartInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'si1InstanceId' @::@ 'Text'
--
startInstance :: Text -- ^ 'si1InstanceId'
              -> StartInstance
startInstance p1 = StartInstance
    { _si1InstanceId = p1
    }

-- | The instance ID.
si1InstanceId :: Lens' StartInstance Text
si1InstanceId = lens _si1InstanceId (\s a -> s { _si1InstanceId = a })

data StartInstanceResponse = StartInstanceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'StartInstanceResponse' constructor.
startInstanceResponse :: StartInstanceResponse
startInstanceResponse = StartInstanceResponse

instance ToPath StartInstance where
    toPath = const "/"

instance ToQuery StartInstance where
    toQuery = const mempty

instance ToHeaders StartInstance

instance ToJSON StartInstance where
    toJSON StartInstance{..} = object
        [ "InstanceId" .= _si1InstanceId
        ]

instance AWSRequest StartInstance where
    type Sv StartInstance = OpsWorks
    type Rs StartInstance = StartInstanceResponse

    request  = post "StartInstance"
    response = nullResponse StartInstanceResponse
