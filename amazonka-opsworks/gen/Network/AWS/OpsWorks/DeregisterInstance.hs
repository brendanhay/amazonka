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

-- Module      : Network.AWS.OpsWorks.DeregisterInstance
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

-- | Deregister a registered Amazon EC2 or on-premises instance. This action
-- removes the instance from the stack and returns it to your control. This
-- action can not be used with instances that were created with AWS OpsWorks.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeregisterInstance.html>
module Network.AWS.OpsWorks.DeregisterInstance
    (
    -- * Request
      DeregisterInstance
    -- ** Request constructor
    , deregisterInstance
    -- ** Request lenses
    , di1InstanceId

    -- * Response
    , DeregisterInstanceResponse
    -- ** Response constructor
    , deregisterInstanceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DeregisterInstance = DeregisterInstance
    { _di1InstanceId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeregisterInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'di1InstanceId' @::@ 'Text'
--
deregisterInstance :: Text -- ^ 'di1InstanceId'
                   -> DeregisterInstance
deregisterInstance p1 = DeregisterInstance
    { _di1InstanceId = p1
    }

-- | The instance ID.
di1InstanceId :: Lens' DeregisterInstance Text
di1InstanceId = lens _di1InstanceId (\s a -> s { _di1InstanceId = a })

data DeregisterInstanceResponse = DeregisterInstanceResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeregisterInstanceResponse' constructor.
deregisterInstanceResponse :: DeregisterInstanceResponse
deregisterInstanceResponse = DeregisterInstanceResponse

instance ToPath DeregisterInstance where
    toPath = const "/"

instance ToQuery DeregisterInstance where
    toQuery = const mempty

instance ToHeaders DeregisterInstance

instance ToJSON DeregisterInstance where
    toJSON DeregisterInstance{..} = object
        [ "InstanceId" .= _di1InstanceId
        ]

instance AWSRequest DeregisterInstance where
    type Sv DeregisterInstance = OpsWorks
    type Rs DeregisterInstance = DeregisterInstanceResponse

    request  = post "DeregisterInstance"
    response = nullResponse DeregisterInstanceResponse
