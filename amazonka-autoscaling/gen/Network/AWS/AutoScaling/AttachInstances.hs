{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.AttachInstances
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
module Network.AWS.AutoScaling.AttachInstances
    (
    -- * Request
      AttachInstances
    -- ** Request constructor
    , attachInstances
    -- ** Request lenses
    , aiAutoScalingGroupName
    , aiInstanceIds

    -- * Response
    , AttachInstancesResponse
    -- ** Response constructor
    , attachInstancesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data AttachInstances = AttachInstances
    { _aiAutoScalingGroupName :: Text
    , _aiInstanceIds          :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'AttachInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aiAutoScalingGroupName' @::@ 'Text'
--
-- * 'aiInstanceIds' @::@ ['Text']
--
attachInstances :: Text -- ^ 'aiAutoScalingGroupName'
                -> AttachInstances
attachInstances p1 = AttachInstances
    { _aiAutoScalingGroupName = p1
    , _aiInstanceIds          = mempty
    }

-- | The name of the Auto Scaling group to which to attach the specified
-- instance(s).
aiAutoScalingGroupName :: Lens' AttachInstances Text
aiAutoScalingGroupName =
    lens _aiAutoScalingGroupName (\s a -> s { _aiAutoScalingGroupName = a })

-- | One or more IDs of the Amazon EC2 instances to attach to the specified
-- Auto Scaling group. You must specify at least one instance ID.
aiInstanceIds :: Lens' AttachInstances [Text]
aiInstanceIds = lens _aiInstanceIds (\s a -> s { _aiInstanceIds = a })

data AttachInstancesResponse = AttachInstancesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AttachInstancesResponse' constructor.
attachInstancesResponse :: AttachInstancesResponse
attachInstancesResponse = AttachInstancesResponse

instance AWSRequest AttachInstances where
    type Sv AttachInstances = AutoScaling
    type Rs AttachInstances = AttachInstancesResponse

    request  = post "AttachInstances"
    response = nullResponse AttachInstancesResponse

instance ToPath AttachInstances where
    toPath = const "/"

instance ToHeaders AttachInstances

instance ToQuery AttachInstances
