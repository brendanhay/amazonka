{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.AssignVolume
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
module Network.AWS.OpsWorks.V2013_02_18.AssignVolume
    (
    -- * Request
      AssignVolume
    -- ** Request constructor
    , assignVolume
    -- ** Request lenses
    , avrVolumeId
    , avrInstanceId

    -- * Response
    , AssignVolumeResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'AssignVolume' request.
assignVolume :: Text -- ^ 'avrVolumeId'
             -> AssignVolume
assignVolume p1 = AssignVolume
    { _avrVolumeId = p1
    , _avrInstanceId = Nothing
    }

data AssignVolume = AssignVolume
    { _avrVolumeId :: Text
      -- ^ The volume ID.
    , _avrInstanceId :: Maybe Text
      -- ^ The instance ID.
    } deriving (Show, Generic)

-- | The volume ID.
avrVolumeId
    :: Functor f
    => (Text
    -> f (Text))
    -> AssignVolume
    -> f AssignVolume
avrVolumeId f x =
    (\y -> x { _avrVolumeId = y })
       <$> f (_avrVolumeId x)
{-# INLINE avrVolumeId #-}

-- | The instance ID.
avrInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AssignVolume
    -> f AssignVolume
avrInstanceId f x =
    (\y -> x { _avrInstanceId = y })
       <$> f (_avrInstanceId x)
{-# INLINE avrInstanceId #-}

instance ToPath AssignVolume

instance ToQuery AssignVolume

instance ToHeaders AssignVolume

instance ToJSON AssignVolume

data AssignVolumeResponse = AssignVolumeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AssignVolume where
    type Sv AssignVolume = OpsWorks
    type Rs AssignVolume = AssignVolumeResponse

    request = get
    response _ = nullaryResponse AssignVolumeResponse
