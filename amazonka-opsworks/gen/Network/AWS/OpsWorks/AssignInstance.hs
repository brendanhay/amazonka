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

-- Module      : Network.AWS.OpsWorks.AssignInstance
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Assign a registered instance to a custom layer. You cannot use this action
-- with instances that were created with AWS OpsWorks.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_AssignInstance.html>
module Network.AWS.OpsWorks.AssignInstance
    (
    -- * Request
      AssignInstance
    -- ** Request constructor
    , assignInstance
    -- ** Request lenses
    , aiInstanceId
    , aiLayerIds

    -- * Response
    , AssignInstanceResponse
    -- ** Response constructor
    , assignInstanceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data AssignInstance = AssignInstance
    { _aiInstanceId :: Text
    , _aiLayerIds   :: List "LayerIds" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AssignInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aiInstanceId' @::@ 'Text'
--
-- * 'aiLayerIds' @::@ ['Text']
--
assignInstance :: Text -- ^ 'aiInstanceId'
               -> AssignInstance
assignInstance p1 = AssignInstance
    { _aiInstanceId = p1
    , _aiLayerIds   = mempty
    }

-- | The instance ID.
aiInstanceId :: Lens' AssignInstance Text
aiInstanceId = lens _aiInstanceId (\s a -> s { _aiInstanceId = a })

-- | The layer ID, which must correspond to a custom layer. You cannot assign a
-- registered instance to a built-in layer.
aiLayerIds :: Lens' AssignInstance [Text]
aiLayerIds = lens _aiLayerIds (\s a -> s { _aiLayerIds = a }) . _List

data AssignInstanceResponse = AssignInstanceResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AssignInstanceResponse' constructor.
assignInstanceResponse :: AssignInstanceResponse
assignInstanceResponse = AssignInstanceResponse

instance ToPath AssignInstance where
    toPath = const "/"

instance ToQuery AssignInstance where
    toQuery = const mempty

instance ToHeaders AssignInstance

instance ToJSON AssignInstance where
    toJSON AssignInstance{..} = object
        [ "InstanceId" .= _aiInstanceId
        , "LayerIds"   .= _aiLayerIds
        ]

instance AWSRequest AssignInstance where
    type Sv AssignInstance = OpsWorks
    type Rs AssignInstance = AssignInstanceResponse

    request  = post "AssignInstance"
    response = nullResponse AssignInstanceResponse
