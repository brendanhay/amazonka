{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      AttachInstancesQuery
    -- ** Request constructor
    , attachInstances
    -- ** Request lenses
    , aiqAutoScalingGroupName
    , aiqInstanceIds

    -- * Response
    , AttachInstancesResponse
    -- ** Response constructor
    , attachInstancesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data AttachInstancesQuery = AttachInstancesQuery
    { _aiqAutoScalingGroupName :: Text
    , _aiqInstanceIds          :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'AttachInstancesQuery' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aiqAutoScalingGroupName' @::@ 'Text'
--
-- * 'aiqInstanceIds' @::@ ['Text']
--
attachInstances :: Text -- ^ 'aiqAutoScalingGroupName'
                -> AttachInstancesQuery
attachInstances p1 = AttachInstancesQuery
    { _aiqAutoScalingGroupName = p1
    , _aiqInstanceIds          = mempty
    }

-- | The name of the Auto Scaling group to which to attach the specified
-- instance(s).
aiqAutoScalingGroupName :: Lens' AttachInstancesQuery Text
aiqAutoScalingGroupName =
    lens _aiqAutoScalingGroupName (\s a -> s { _aiqAutoScalingGroupName = a })

-- | One or more IDs of the Amazon EC2 instances to attach to the specified
-- Auto Scaling group. You must specify at least one instance ID.
aiqInstanceIds :: Lens' AttachInstancesQuery [Text]
aiqInstanceIds = lens _aiqInstanceIds (\s a -> s { _aiqInstanceIds = a })

instance ToPath AttachInstancesQuery where
    toPath = const "/"

instance ToQuery AttachInstancesQuery

data AttachInstancesResponse = AttachInstancesResponse

-- | 'AttachInstancesResponse' constructor.
attachInstancesResponse :: AttachInstancesResponse
attachInstancesResponse = AttachInstancesResponse

instance AWSRequest AttachInstancesQuery where
    type Sv AttachInstancesQuery = AutoScaling
    type Rs AttachInstancesQuery = AttachInstancesResponse

    request  = post "AttachInstances"
    response = const (nullaryResponse AttachInstancesResponse)
