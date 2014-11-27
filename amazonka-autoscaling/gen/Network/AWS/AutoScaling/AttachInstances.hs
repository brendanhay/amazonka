{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Attaches one or more EC2 instances to the specified Auto Scaling group.
--
-- For more information, see <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/attach-instance-asg.html Attach Amazon EC2 Instances to Your Existing AutoScaling Group> in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_AttachInstances.html>
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
    , _aiInstanceIds          :: List "InstanceIds" Text
    } deriving (Eq, Ord, Show)

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

-- | The name of the group.
aiAutoScalingGroupName :: Lens' AttachInstances Text
aiAutoScalingGroupName =
    lens _aiAutoScalingGroupName (\s a -> s { _aiAutoScalingGroupName = a })

-- | One or more EC2 instance IDs. You must specify at least one ID.
aiInstanceIds :: Lens' AttachInstances [Text]
aiInstanceIds = lens _aiInstanceIds (\s a -> s { _aiInstanceIds = a }) . _List

data AttachInstancesResponse = AttachInstancesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AttachInstancesResponse' constructor.
attachInstancesResponse :: AttachInstancesResponse
attachInstancesResponse = AttachInstancesResponse

instance ToPath AttachInstances where
    toPath = const "/"

instance ToQuery AttachInstances where
    toQuery AttachInstances{..} = mconcat
        [ "AutoScalingGroupName" =? _aiAutoScalingGroupName
        , "InstanceIds"          =? _aiInstanceIds
        ]

instance ToHeaders AttachInstances

instance AWSRequest AttachInstances where
    type Sv AttachInstances = AutoScaling
    type Rs AttachInstances = AttachInstancesResponse

    request  = post "AttachInstances"
    response = nullResponse AttachInstancesResponse
