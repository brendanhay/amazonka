{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.AssignVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Assigns one of the stack's registered Amazon EBS volumes to a specified
-- instance. The volume must first be registered with the stack by calling
-- RegisterVolume. For more information, see Resource Management. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.AssignVolume
    (
    -- * Request
      AssignVolume
    -- ** Request constructor
    , mkAssignVolume
    -- ** Request lenses
    , avVolumeId
    , avInstanceId

    -- * Response
    , AssignVolumeResponse
    -- ** Response constructor
    , mkAssignVolumeResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data AssignVolume = AssignVolume
    { _avVolumeId :: !Text
    , _avInstanceId :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssignVolume' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
mkAssignVolume :: Text -- ^ 'avVolumeId'
               -> AssignVolume
mkAssignVolume p1 = AssignVolume
    { _avVolumeId = p1
    , _avInstanceId = Nothing
    }

-- | The volume ID.
avVolumeId :: Lens' AssignVolume Text
avVolumeId = lens _avVolumeId (\s a -> s { _avVolumeId = a })

-- | The instance ID.
avInstanceId :: Lens' AssignVolume (Maybe Text)
avInstanceId = lens _avInstanceId (\s a -> s { _avInstanceId = a })

instance ToPath AssignVolume

instance ToQuery AssignVolume

instance ToHeaders AssignVolume

instance ToJSON AssignVolume

data AssignVolumeResponse = AssignVolumeResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssignVolumeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkAssignVolumeResponse :: AssignVolumeResponse
mkAssignVolumeResponse = AssignVolumeResponse

instance AWSRequest AssignVolume where
    type Sv AssignVolume = OpsWorks
    type Rs AssignVolume = AssignVolumeResponse

    request = get
    response _ = nullaryResponse AssignVolumeResponse
