{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.UnassignVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Unassigns an assigned Amazon EBS volume. The volume remains registered with
-- the stack. For more information, see Resource Management. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.V2013_02_18.UnassignVolume
    (
    -- * Request
      UnassignVolume
    -- ** Request constructor
    , unassignVolume
    -- ** Request lenses
    , uvrVolumeId

    -- * Response
    , UnassignVolumeResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'UnassignVolume' request.
unassignVolume :: Text -- ^ 'uvrVolumeId'
               -> UnassignVolume
unassignVolume p1 = UnassignVolume
    { _uvrVolumeId = p1
    }

data UnassignVolume = UnassignVolume
    { _uvrVolumeId :: Text
      -- ^ The volume ID.
    } deriving (Show, Generic)

-- | The volume ID.
uvrVolumeId
    :: Functor f
    => (Text
    -> f (Text))
    -> UnassignVolume
    -> f UnassignVolume
uvrVolumeId f x =
    (\y -> x { _uvrVolumeId = y })
       <$> f (_uvrVolumeId x)
{-# INLINE uvrVolumeId #-}

instance ToPath UnassignVolume

instance ToQuery UnassignVolume

instance ToHeaders UnassignVolume

instance ToJSON UnassignVolume

data UnassignVolumeResponse = UnassignVolumeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UnassignVolume where
    type Sv UnassignVolume = OpsWorks
    type Rs UnassignVolume = UnassignVolumeResponse

    request = get
    response _ = nullaryResponse UnassignVolumeResponse
