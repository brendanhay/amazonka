{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.SetLoadBasedAutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Specify the load-based auto scaling configuration for a specified layer.
-- For more information, see Managing Load with Time-based and Load-based
-- Instances. Required Permissions: To use this action, an IAM user must have
-- a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.SetLoadBasedAutoScaling
    (
    -- * Request
      SetLoadBasedAutoScaling
    -- ** Request constructor
    , setLoadBasedAutoScaling
    -- ** Request lenses
    , slbasDownScaling
    , slbasEnable
    , slbasLayerId
    , slbasUpScaling

    -- * Response
    , SetLoadBasedAutoScalingResponse
    -- ** Response constructor
    , setLoadBasedAutoScalingResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data SetLoadBasedAutoScaling = SetLoadBasedAutoScaling
    { _slbasDownScaling :: Maybe AutoScalingThresholds
    , _slbasEnable      :: Maybe Bool
    , _slbasLayerId     :: Text
    , _slbasUpScaling   :: Maybe AutoScalingThresholds
    } deriving (Eq, Show, Generic)

-- | 'SetLoadBasedAutoScaling' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slbasDownScaling' @::@ 'Maybe' 'AutoScalingThresholds'
--
-- * 'slbasEnable' @::@ 'Maybe' 'Bool'
--
-- * 'slbasLayerId' @::@ 'Text'
--
-- * 'slbasUpScaling' @::@ 'Maybe' 'AutoScalingThresholds'
--
setLoadBasedAutoScaling :: Text -- ^ 'slbasLayerId'
                        -> SetLoadBasedAutoScaling
setLoadBasedAutoScaling p1 = SetLoadBasedAutoScaling
    { _slbasLayerId     = p1
    , _slbasEnable      = Nothing
    , _slbasUpScaling   = Nothing
    , _slbasDownScaling = Nothing
    }

-- | An AutoScalingThresholds object with the downscaling threshold
-- configuration. If the load falls below these thresholds for a specified
-- amount of time, AWS OpsWorks stops a specified number of instances.
slbasDownScaling :: Lens' SetLoadBasedAutoScaling (Maybe AutoScalingThresholds)
slbasDownScaling = lens _slbasDownScaling (\s a -> s { _slbasDownScaling = a })

-- | Enables load-based auto scaling for the layer.
slbasEnable :: Lens' SetLoadBasedAutoScaling (Maybe Bool)
slbasEnable = lens _slbasEnable (\s a -> s { _slbasEnable = a })

-- | The layer ID.
slbasLayerId :: Lens' SetLoadBasedAutoScaling Text
slbasLayerId = lens _slbasLayerId (\s a -> s { _slbasLayerId = a })

-- | An AutoScalingThresholds object with the upscaling threshold
-- configuration. If the load exceeds these thresholds for a specified
-- amount of time, AWS OpsWorks starts a specified number of instances.
slbasUpScaling :: Lens' SetLoadBasedAutoScaling (Maybe AutoScalingThresholds)
slbasUpScaling = lens _slbasUpScaling (\s a -> s { _slbasUpScaling = a })

data SetLoadBasedAutoScalingResponse = SetLoadBasedAutoScalingResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetLoadBasedAutoScalingResponse' constructor.
setLoadBasedAutoScalingResponse :: SetLoadBasedAutoScalingResponse
setLoadBasedAutoScalingResponse = SetLoadBasedAutoScalingResponse

instance AWSRequest SetLoadBasedAutoScaling where
    type Sv SetLoadBasedAutoScaling = OpsWorks
    type Rs SetLoadBasedAutoScaling = SetLoadBasedAutoScalingResponse

    request  = post
    response = nullResponse SetLoadBasedAutoScalingResponse

instance ToPath SetLoadBasedAutoScaling where
    toPath = const "/"

instance ToHeaders SetLoadBasedAutoScaling

instance ToQuery SetLoadBasedAutoScaling where
    toQuery = const mempty

instance ToJSON SetLoadBasedAutoScaling where
    toJSON = genericToJSON jsonOptions
