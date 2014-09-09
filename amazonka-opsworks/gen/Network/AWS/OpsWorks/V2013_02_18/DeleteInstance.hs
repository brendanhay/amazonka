{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DeleteInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified instance. You must stop an instance before you can
-- delete it. For more information, see Deleting Instances. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DeleteInstance
    (
    -- * Request
      DeleteInstance
    -- ** Request constructor
    , mkDeleteInstance
    -- ** Request lenses
    , diInstanceId
    , diDeleteElasticIp
    , diDeleteVolumes

    -- * Response
    , DeleteInstanceResponse
    -- ** Response constructor
    , mkDeleteInstanceResponse
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DeleteInstance = DeleteInstance
    { _diInstanceId :: Text
    , _diDeleteElasticIp :: Maybe Bool
    , _diDeleteVolumes :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteInstance' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
-- * @DeleteElasticIp ::@ @Maybe Bool@
--
-- * @DeleteVolumes ::@ @Maybe Bool@
--
mkDeleteInstance :: Text -- ^ 'diInstanceId'
                 -> DeleteInstance
mkDeleteInstance p1 = DeleteInstance
    { _diInstanceId = p1
    , _diDeleteElasticIp = Nothing
    , _diDeleteVolumes = Nothing
    }

-- | The instance ID.
diInstanceId :: Lens' DeleteInstance Text
diInstanceId = lens _diInstanceId (\s a -> s { _diInstanceId = a })

-- | Whether to delete the instance Elastic IP address.
diDeleteElasticIp :: Lens' DeleteInstance (Maybe Bool)
diDeleteElasticIp =
    lens _diDeleteElasticIp (\s a -> s { _diDeleteElasticIp = a })

-- | Whether to delete the instance's Amazon EBS volumes.
diDeleteVolumes :: Lens' DeleteInstance (Maybe Bool)
diDeleteVolumes = lens _diDeleteVolumes (\s a -> s { _diDeleteVolumes = a })

instance ToPath DeleteInstance

instance ToQuery DeleteInstance

instance ToHeaders DeleteInstance

instance ToJSON DeleteInstance

data DeleteInstanceResponse = DeleteInstanceResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteInstanceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteInstanceResponse :: DeleteInstanceResponse
mkDeleteInstanceResponse = DeleteInstanceResponse

instance AWSRequest DeleteInstance where
    type Sv DeleteInstance = OpsWorks
    type Rs DeleteInstance = DeleteInstanceResponse

    request = get
    response _ = nullaryResponse DeleteInstanceResponse
