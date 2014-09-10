{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling
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
module Network.AWS.AutoScaling
    (
    -- * Request
      AttachInstances
    -- ** Request constructor
    , mkAttachInstances
    -- ** Request lenses
    , aiInstanceIds
    , aiAutoScalingGroupName

    -- * Response
    , AttachInstancesResponse
    -- ** Response constructor
    , mkAttachInstancesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data AttachInstances = AttachInstances
    { _aiInstanceIds :: List1 Text
    , _aiAutoScalingGroupName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachInstances' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceIds ::@ @List1 Text@
--
-- * @AutoScalingGroupName ::@ @Text@
--
mkAttachInstances :: List1 Text -- ^ 'aiInstanceIds'
                  -> Text -- ^ 'aiAutoScalingGroupName'
                  -> AttachInstances
mkAttachInstances p1 p2 = AttachInstances
    { _aiInstanceIds = p1
    , _aiAutoScalingGroupName = p2
    }

-- | One or more IDs of the Amazon EC2 instances to attach to the specified Auto
-- Scaling group. You must specify at least one instance ID.
aiInstanceIds :: Lens' AttachInstances (List1 Text)
aiInstanceIds = lens _aiInstanceIds (\s a -> s { _aiInstanceIds = a })

-- | The name of the Auto Scaling group to which to attach the specified
-- instance(s).
aiAutoScalingGroupName :: Lens' AttachInstances Text
aiAutoScalingGroupName =
    lens _aiAutoScalingGroupName (\s a -> s { _aiAutoScalingGroupName = a })

instance ToQuery AttachInstances where
    toQuery = genericQuery def

data AttachInstancesResponse = AttachInstancesResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachInstancesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkAttachInstancesResponse :: AttachInstancesResponse
mkAttachInstancesResponse = AttachInstancesResponse

instance AWSRequest AttachInstances where
    type Sv AttachInstances = AutoScaling
    type Rs AttachInstances = AttachInstancesResponse

    request = post "AttachInstances"
    response _ = nullaryResponse AttachInstancesResponse
