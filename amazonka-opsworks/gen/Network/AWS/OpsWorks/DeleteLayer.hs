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

-- Module      : Network.AWS.OpsWorks.DeleteLayer
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

-- | Deletes a specified layer. You must first stop and then delete all associated
-- instances or unassign registered instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-delete.html How toDelete a Layer>.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeleteLayer.html>
module Network.AWS.OpsWorks.DeleteLayer
    (
    -- * Request
      DeleteLayer
    -- ** Request constructor
    , deleteLayer
    -- ** Request lenses
    , dlLayerId

    -- * Response
    , DeleteLayerResponse
    -- ** Response constructor
    , deleteLayerResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DeleteLayer = DeleteLayer
    { _dlLayerId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteLayer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlLayerId' @::@ 'Text'
--
deleteLayer :: Text -- ^ 'dlLayerId'
            -> DeleteLayer
deleteLayer p1 = DeleteLayer
    { _dlLayerId = p1
    }

-- | The layer ID.
dlLayerId :: Lens' DeleteLayer Text
dlLayerId = lens _dlLayerId (\s a -> s { _dlLayerId = a })

data DeleteLayerResponse = DeleteLayerResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLayerResponse' constructor.
deleteLayerResponse :: DeleteLayerResponse
deleteLayerResponse = DeleteLayerResponse

instance ToPath DeleteLayer where
    toPath = const "/"

instance ToQuery DeleteLayer where
    toQuery = const mempty

instance ToHeaders DeleteLayer

instance ToJSON DeleteLayer where
    toJSON DeleteLayer{..} = object
        [ "LayerId" .= _dlLayerId
        ]

instance AWSRequest DeleteLayer where
    type Sv DeleteLayer = OpsWorks
    type Rs DeleteLayer = DeleteLayerResponse

    request  = post "DeleteLayer"
    response = nullResponse DeleteLayerResponse
