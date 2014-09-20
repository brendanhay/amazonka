{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.RegisterVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers an Amazon EBS volume with a specified stack. A volume can be
-- registered with only one stack at a time. If the volume is already
-- registered, you must first deregister it by calling DeregisterVolume. For
-- more information, see Resource Management. Required Permissions: To use
-- this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.RegisterVolume
    (
    -- * Request
      RegisterVolume
    -- ** Request constructor
    , registerVolume
    -- ** Request lenses
    , rvEc2VolumeId
    , rvStackId

    -- * Response
    , RegisterVolumeResponse
    -- ** Response constructor
    , registerVolumeResponse
    -- ** Response lenses
    , rvrVolumeId
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data RegisterVolume = RegisterVolume
    { _rvEc2VolumeId :: Maybe Text
    , _rvStackId :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterVolume' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Ec2VolumeId ::@ @Maybe Text@
--
-- * @StackId ::@ @Text@
--
registerVolume :: Text -- ^ 'rvStackId'
               -> RegisterVolume
registerVolume p2 = RegisterVolume
    { _rvEc2VolumeId = Nothing
    , _rvStackId = p2
    }

-- | The Amazon EBS volume ID.
rvEc2VolumeId :: Lens' RegisterVolume (Maybe Text)
rvEc2VolumeId = lens _rvEc2VolumeId (\s a -> s { _rvEc2VolumeId = a })

-- | The stack ID.
rvStackId :: Lens' RegisterVolume Text
rvStackId = lens _rvStackId (\s a -> s { _rvStackId = a })

instance ToPath RegisterVolume

instance ToQuery RegisterVolume

instance ToHeaders RegisterVolume

instance ToJSON RegisterVolume

-- | Contains the response to a RegisterVolume request.
newtype RegisterVolumeResponse = RegisterVolumeResponse
    { _rvrVolumeId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterVolumeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Maybe Text@
--
registerVolumeResponse :: RegisterVolumeResponse
registerVolumeResponse = RegisterVolumeResponse
    { _rvrVolumeId = Nothing
    }

-- | The volume ID.
rvrVolumeId :: Lens' RegisterVolumeResponse (Maybe Text)
rvrVolumeId = lens _rvrVolumeId (\s a -> s { _rvrVolumeId = a })

instance FromJSON RegisterVolumeResponse

instance AWSRequest RegisterVolume where
    type Sv RegisterVolume = OpsWorks
    type Rs RegisterVolume = RegisterVolumeResponse

    request = get
    response _ = jsonResponse
