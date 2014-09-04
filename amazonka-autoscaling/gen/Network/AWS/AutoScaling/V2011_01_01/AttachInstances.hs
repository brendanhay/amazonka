{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.AttachInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches one or more Amazon EC2 instances to an existing Auto Scaling
-- group. After the instance(s) is attached, it becomes a part of the Auto
-- Scaling group. For more information, see Attach Amazon EC2 Instances to
-- Your Existing Auto Scaling Group in the Auto Scaling Developer Guide.
module Network.AWS.AutoScaling.V2011_01_01.AttachInstances
    (
    -- * Request
      AttachInstances
    -- ** Request constructor
    , mkAttachInstancesQuery
    -- ** Request lenses
    , aiqInstanceIds
    , aiqAutoScalingGroupName

    -- * Response
    , AttachInstancesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachInstances' request.
mkAttachInstancesQuery :: Text -- ^ 'aiqAutoScalingGroupName'
                       -> AttachInstances
mkAttachInstancesQuery p1 = AttachInstances
    { _aiqInstanceIds = mempty
    , _aiqAutoScalingGroupName = p2
    }
{-# INLINE mkAttachInstancesQuery #-}

data AttachInstances = AttachInstances
    { _aiqInstanceIds :: [Text]
      -- ^ One or more IDs of the Amazon EC2 instances to attach to the
      -- specified Auto Scaling group. You must specify at least one
      -- instance ID.
    , _aiqAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group to which to attach the
      -- specified instance(s).
    } deriving (Show, Generic)

-- | One or more IDs of the Amazon EC2 instances to attach to the specified Auto
-- Scaling group. You must specify at least one instance ID.
aiqInstanceIds :: Lens' AttachInstances ([Text])
aiqInstanceIds = lens _aiqInstanceIds (\s a -> s { _aiqInstanceIds = a })
{-# INLINE aiqInstanceIds #-}

-- | The name of the Auto Scaling group to which to attach the specified
-- instance(s).
aiqAutoScalingGroupName :: Lens' AttachInstances (Text)
aiqAutoScalingGroupName = lens _aiqAutoScalingGroupName (\s a -> s { _aiqAutoScalingGroupName = a })
{-# INLINE aiqAutoScalingGroupName #-}

instance ToQuery AttachInstances where
    toQuery = genericQuery def

data AttachInstancesResponse = AttachInstancesResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AttachInstances where
    type Sv AttachInstances = AutoScaling
    type Rs AttachInstances = AttachInstancesResponse

    request = post "AttachInstances"
    response _ = nullaryResponse AttachInstancesResponse
