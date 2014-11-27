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

-- Module      : Network.AWS.OpsWorks.DeleteInstance
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

-- | Deletes a specified instance. You must stop an instance before you can delete
-- it. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-delete.html Deleting Instances>.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeleteInstance.html>
module Network.AWS.OpsWorks.DeleteInstance
    (
    -- * Request
      DeleteInstance
    -- ** Request constructor
    , deleteInstance
    -- ** Request lenses
    , diDeleteElasticIp
    , diDeleteVolumes
    , diInstanceId

    -- * Response
    , DeleteInstanceResponse
    -- ** Response constructor
    , deleteInstanceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data DeleteInstance = DeleteInstance
    { _diDeleteElasticIp :: Maybe Bool
    , _diDeleteVolumes   :: Maybe Bool
    , _diInstanceId      :: Text
    } deriving (Eq, Ord, Show)

-- | 'DeleteInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diDeleteElasticIp' @::@ 'Maybe' 'Bool'
--
-- * 'diDeleteVolumes' @::@ 'Maybe' 'Bool'
--
-- * 'diInstanceId' @::@ 'Text'
--
deleteInstance :: Text -- ^ 'diInstanceId'
               -> DeleteInstance
deleteInstance p1 = DeleteInstance
    { _diInstanceId      = p1
    , _diDeleteElasticIp = Nothing
    , _diDeleteVolumes   = Nothing
    }

-- | Whether to delete the instance Elastic IP address.
diDeleteElasticIp :: Lens' DeleteInstance (Maybe Bool)
diDeleteElasticIp =
    lens _diDeleteElasticIp (\s a -> s { _diDeleteElasticIp = a })

-- | Whether to delete the instance's Amazon EBS volumes.
diDeleteVolumes :: Lens' DeleteInstance (Maybe Bool)
diDeleteVolumes = lens _diDeleteVolumes (\s a -> s { _diDeleteVolumes = a })

-- | The instance ID.
diInstanceId :: Lens' DeleteInstance Text
diInstanceId = lens _diInstanceId (\s a -> s { _diInstanceId = a })

data DeleteInstanceResponse = DeleteInstanceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteInstanceResponse' constructor.
deleteInstanceResponse :: DeleteInstanceResponse
deleteInstanceResponse = DeleteInstanceResponse

instance ToPath DeleteInstance where
    toPath = const "/"

instance ToQuery DeleteInstance where
    toQuery = const mempty

instance ToHeaders DeleteInstance

instance ToJSON DeleteInstance where
    toJSON DeleteInstance{..} = object
        [ "InstanceId"      .= _diInstanceId
        , "DeleteElasticIp" .= _diDeleteElasticIp
        , "DeleteVolumes"   .= _diDeleteVolumes
        ]

instance AWSRequest DeleteInstance where
    type Sv DeleteInstance = OpsWorks
    type Rs DeleteInstance = DeleteInstanceResponse

    request  = post "DeleteInstance"
    response = nullResponse DeleteInstanceResponse
