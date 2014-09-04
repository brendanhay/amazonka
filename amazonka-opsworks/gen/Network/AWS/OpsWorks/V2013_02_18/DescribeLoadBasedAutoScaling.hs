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
    , describeLoadBasedAutoScaling
    -- ** Request lenses
    , dlbasrLayerIds

    -- * Response
    , DescribeLoadBasedAutoScalingResponse
    -- ** Response lenses
    , dlbassLoadBasedAutoScalingConfigurations
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeLoadBasedAutoScaling' request.
describeLoadBasedAutoScaling :: [Text] -- ^ 'dlbasrLayerIds'
                             -> DescribeLoadBasedAutoScaling
describeLoadBasedAutoScaling p1 = DescribeLoadBasedAutoScaling
    { _dlbasrLayerIds = p1
    }
{-# INLINE describeLoadBasedAutoScaling #-}

data DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling
    { _dlbasrLayerIds :: [Text]
      -- ^ An array of layer IDs.
    } deriving (Show, Generic)

-- | An array of layer IDs.
dlbasrLayerIds :: Lens' DescribeLoadBasedAutoScaling ([Text])
dlbasrLayerIds f x =
    f (_dlbasrLayerIds x)
        <&> \y -> x { _dlbasrLayerIds = y }
{-# INLINE dlbasrLayerIds #-}

instance ToPath DescribeLoadBasedAutoScaling

instance ToQuery DescribeLoadBasedAutoScaling

instance ToHeaders DescribeLoadBasedAutoScaling

instance ToJSON DescribeLoadBasedAutoScaling

data DescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse
    { _dlbassLoadBasedAutoScalingConfigurations :: [LoadBasedAutoScalingConfiguration]
      -- ^ An array of LoadBasedAutoScalingConfiguration objects that
      -- describe each layer's configuration.
    } deriving (Show, Generic)

-- | An array of LoadBasedAutoScalingConfiguration objects that describe each
-- layer's configuration.
dlbassLoadBasedAutoScalingConfigurations :: Lens' DescribeLoadBasedAutoScalingResponse ([LoadBasedAutoScalingConfiguration])
dlbassLoadBasedAutoScalingConfigurations f x =
    f (_dlbassLoadBasedAutoScalingConfigurations x)
        <&> \y -> x { _dlbassLoadBasedAutoScalingConfigurations = y }
{-# INLINE dlbassLoadBasedAutoScalingConfigurations #-}

instance FromJSON DescribeLoadBasedAutoScalingResponse

instance AWSRequest DescribeLoadBasedAutoScaling where
    type Sv DescribeLoadBasedAutoScaling = OpsWorks
    type Rs DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScalingResponse

    request = get
    response _ = jsonResponse
