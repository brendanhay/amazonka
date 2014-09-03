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
    , registerVolume
    -- ** Request lenses
    , rvrStackId
    , rvrEc2VolumeId

    -- * Response
    , RegisterVolumeResponse
    -- ** Response lenses
    , rvsVolumeId
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'RegisterVolume' request.
registerVolume :: Text -- ^ 'rvrStackId'
               -> RegisterVolume
registerVolume p1 = RegisterVolume
    { _rvrStackId = p1
    , _rvrEc2VolumeId = Nothing
    }

data RegisterVolume = RegisterVolume
    { _rvrStackId :: Text
      -- ^ The stack ID.
    , _rvrEc2VolumeId :: Maybe Text
      -- ^ The Amazon EBS volume ID.
    } deriving (Show, Generic)

-- | The stack ID.
rvrStackId
    :: Functor f
    => (Text
    -> f (Text))
    -> RegisterVolume
    -> f RegisterVolume
rvrStackId f x =
    (\y -> x { _rvrStackId = y })
       <$> f (_rvrStackId x)
{-# INLINE rvrStackId #-}

-- | The Amazon EBS volume ID.
rvrEc2VolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RegisterVolume
    -> f RegisterVolume
rvrEc2VolumeId f x =
    (\y -> x { _rvrEc2VolumeId = y })
       <$> f (_rvrEc2VolumeId x)
{-# INLINE rvrEc2VolumeId #-}

instance ToPath RegisterVolume

instance ToQuery RegisterVolume

instance ToHeaders RegisterVolume

instance ToJSON RegisterVolume

data RegisterVolumeResponse = RegisterVolumeResponse
    { _rvsVolumeId :: Maybe Text
      -- ^ The volume ID.
    } deriving (Show, Generic)

-- | The volume ID.
rvsVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RegisterVolumeResponse
    -> f RegisterVolumeResponse
rvsVolumeId f x =
    (\y -> x { _rvsVolumeId = y })
       <$> f (_rvsVolumeId x)
{-# INLINE rvsVolumeId #-}

instance FromJSON RegisterVolumeResponse

instance AWSRequest RegisterVolume where
    type Sv RegisterVolume = OpsWorks
    type Rs RegisterVolume = RegisterVolumeResponse

    request = get
    response _ = jsonResponse
