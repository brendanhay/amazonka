{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeLoadBasedAutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes load-based auto scaling configurations for specified layers. You
-- must specify at least one of the parameters. Required Permissions: To use
-- this action, an IAM user must have a Show, Deploy, or Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeLoadBasedAutoScaling
    (
    -- * Request
      DescribeLoadBasedAutoScaling
    -- ** Request constructor
    , mkDescribeLoadBasedAutoScaling
    -- ** Request lenses
    , dlbasLayerIds

    -- * Response
    , DescribeLoadBasedAutoScalingResponse
    -- ** Response constructor
    , mkDescribeLoadBasedAutoScalingResponse
    -- ** Response lenses
    , dlbasrLoadBasedAutoScalingConfigurations
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling
    { _dlbasLayerIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoadBasedAutoScaling' request.
mkDescribeLoadBasedAutoScaling :: [Text] -- ^ 'dlbasLayerIds'
                               -> DescribeLoadBasedAutoScaling
mkDescribeLoadBasedAutoScaling p1 = DescribeLoadBasedAutoScaling
    { _dlbasLayerIds = p1
    }

-- | An array of layer IDs.
dlbasLayerIds :: Lens' DescribeLoadBasedAutoScaling [Text]
dlbasLayerIds = lens _dlbasLayerIds (\s a -> s { _dlbasLayerIds = a })

instance ToPath DescribeLoadBasedAutoScaling

instance ToQuery DescribeLoadBasedAutoScaling

instance ToHeaders DescribeLoadBasedAutoScaling

instance ToJSON DescribeLoadBasedAutoScaling

-- | Contains the response to a DescribeLoadBasedAutoScaling request.
newtype DescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse
    { _dlbasrLoadBasedAutoScalingConfigurations :: [LoadBasedAutoScalingConfiguration]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoadBasedAutoScalingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeLoadBasedAutoScalingResponse :: DescribeLoadBasedAutoScalingResponse
mkDescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse
    { _dlbasrLoadBasedAutoScalingConfigurations = mempty
    }

-- | An array of LoadBasedAutoScalingConfiguration objects that describe each
-- layer's configuration.
dlbasrLoadBasedAutoScalingConfigurations :: Lens' DescribeLoadBasedAutoScalingResponse [LoadBasedAutoScalingConfiguration]
dlbasrLoadBasedAutoScalingConfigurations =
    lens _dlbasrLoadBasedAutoScalingConfigurations
         (\s a -> s { _dlbasrLoadBasedAutoScalingConfigurations = a })

instance FromJSON DescribeLoadBasedAutoScalingResponse

instance AWSRequest DescribeLoadBasedAutoScaling where
    type Sv DescribeLoadBasedAutoScaling = OpsWorks
    type Rs DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScalingResponse

    request = get
    response _ = jsonResponse
