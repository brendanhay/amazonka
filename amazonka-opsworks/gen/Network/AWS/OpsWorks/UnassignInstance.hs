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

-- Module      : Network.AWS.OpsWorks.UnassignInstance
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

-- | Unassigns a registered instance from all of it's layers. The instance remains
-- in the stack as an unassigned instance and can be assigned to another layer,
-- as needed. You cannot use this action with instances that were created with
-- AWS OpsWorks.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UnassignInstance.html>
module Network.AWS.OpsWorks.UnassignInstance
    (
    -- * Request
      UnassignInstance
    -- ** Request constructor
    , unassignInstance
    -- ** Request lenses
    , ui1InstanceId

    -- * Response
    , UnassignInstanceResponse
    -- ** Response constructor
    , unassignInstanceResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype UnassignInstance = UnassignInstance
    { _ui1InstanceId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'UnassignInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ui1InstanceId' @::@ 'Text'
--
unassignInstance :: Text -- ^ 'ui1InstanceId'
                 -> UnassignInstance
unassignInstance p1 = UnassignInstance
    { _ui1InstanceId = p1
    }

-- | The instance ID.
ui1InstanceId :: Lens' UnassignInstance Text
ui1InstanceId = lens _ui1InstanceId (\s a -> s { _ui1InstanceId = a })

data UnassignInstanceResponse = UnassignInstanceResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'UnassignInstanceResponse' constructor.
unassignInstanceResponse :: UnassignInstanceResponse
unassignInstanceResponse = UnassignInstanceResponse

instance ToPath UnassignInstance where
    toPath = const "/"

instance ToQuery UnassignInstance where
    toQuery = const mempty

instance ToHeaders UnassignInstance

instance ToJSON UnassignInstance where
    toJSON UnassignInstance{..} = object
        [ "InstanceId" .= _ui1InstanceId
        ]

instance AWSRequest UnassignInstance where
    type Sv UnassignInstance = OpsWorks
    type Rs UnassignInstance = UnassignInstanceResponse

    request  = post "UnassignInstance"
    response = nullResponse UnassignInstanceResponse
