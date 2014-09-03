{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DeregisterVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters an Amazon EBS volume. The volume can then be registered by
-- another stack. For more information, see Resource Management. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DeregisterVolume
    (
    -- * Request
      DeregisterVolume
    -- ** Request constructor
    , deregisterVolume
    -- ** Request lenses
    , dvrVolumeId

    -- * Response
    , DeregisterVolumeResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DeregisterVolume' request.
deregisterVolume :: Text -- ^ 'dvrVolumeId'
                 -> DeregisterVolume
deregisterVolume p1 = DeregisterVolume
    { _dvrVolumeId = p1
    }

data DeregisterVolume = DeregisterVolume
    { _dvrVolumeId :: Text
      -- ^ The volume ID.
    } deriving (Show, Generic)

-- | The volume ID.
dvrVolumeId
    :: Functor f
    => (Text
    -> f (Text))
    -> DeregisterVolume
    -> f DeregisterVolume
dvrVolumeId f x =
    (\y -> x { _dvrVolumeId = y })
       <$> f (_dvrVolumeId x)
{-# INLINE dvrVolumeId #-}

instance ToPath DeregisterVolume

instance ToQuery DeregisterVolume

instance ToHeaders DeregisterVolume

instance ToJSON DeregisterVolume

data DeregisterVolumeResponse = DeregisterVolumeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeregisterVolume where
    type Sv DeregisterVolume = OpsWorks
    type Rs DeregisterVolume = DeregisterVolumeResponse

    request = get
    response _ = nullaryResponse DeregisterVolumeResponse
