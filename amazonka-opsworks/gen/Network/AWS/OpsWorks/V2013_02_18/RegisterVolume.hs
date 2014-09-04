{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.RegisterVolume
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
module Network.AWS.OpsWorks.V2013_02_18.RegisterVolume
    (
    -- * Request
      RegisterVolume
    -- ** Request constructor
    , mkRegisterVolumeRequest
    -- ** Request lenses
    , rvrEc2VolumeId
    , rvrStackId

    -- * Response
    , RegisterVolumeResponse
    -- ** Response lenses
    , rvsVolumeId
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterVolume' request.
mkRegisterVolumeRequest :: Text -- ^ 'rvrStackId'
                        -> RegisterVolume
mkRegisterVolumeRequest p1 = RegisterVolume
    { _rvrEc2VolumeId = Nothing
    , _rvrStackId = p2
    }
{-# INLINE mkRegisterVolumeRequest #-}

data RegisterVolume = RegisterVolume
    { _rvrEc2VolumeId :: Maybe Text
      -- ^ The Amazon EBS volume ID.
    , _rvrStackId :: Text
      -- ^ The stack ID.
    } deriving (Show, Generic)

-- | The Amazon EBS volume ID.
rvrEc2VolumeId :: Lens' RegisterVolume (Maybe Text)
rvrEc2VolumeId = lens _rvrEc2VolumeId (\s a -> s { _rvrEc2VolumeId = a })
{-# INLINE rvrEc2VolumeId #-}

-- | The stack ID.
rvrStackId :: Lens' RegisterVolume (Text)
rvrStackId = lens _rvrStackId (\s a -> s { _rvrStackId = a })
{-# INLINE rvrStackId #-}

instance ToPath RegisterVolume

instance ToQuery RegisterVolume

instance ToHeaders RegisterVolume

instance ToJSON RegisterVolume

newtype RegisterVolumeResponse = RegisterVolumeResponse
    { _rvsVolumeId :: Maybe Text
      -- ^ The volume ID.
    } deriving (Show, Generic)

-- | The volume ID.
rvsVolumeId :: Lens' RegisterVolumeResponse (Maybe Text)
rvsVolumeId = lens _rvsVolumeId (\s a -> s { _rvsVolumeId = a })
{-# INLINE rvsVolumeId #-}

instance FromJSON RegisterVolumeResponse

instance AWSRequest RegisterVolume where
    type Sv RegisterVolume = OpsWorks
    type Rs RegisterVolume = RegisterVolumeResponse

    request = get
    response _ = jsonResponse
