{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeLifecycleHooks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the lifecycle hooks that currently belong to the specified Auto
-- Scaling group.
module Network.AWS.AutoScaling.V2011_01_01.DescribeLifecycleHooks where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeLifecycleHooks' request.
describeLifecycleHooks :: Text -- ^ '_dlhtAutoScalingGroupName'
                       -> DescribeLifecycleHooks
describeLifecycleHooks p1 = DescribeLifecycleHooks
    { _dlhtAutoScalingGroupName = p1
    , _dlhtLifecycleHookNames = mempty
    }

data DescribeLifecycleHooks = DescribeLifecycleHooks
    { _dlhtAutoScalingGroupName :: Text
      -- ^ The name of one or more Auto Scaling groups.
    , _dlhtLifecycleHookNames :: [Text]
      -- ^ The name of one or more lifecycle hooks.
    } deriving (Show, Generic)

makeLenses ''DescribeLifecycleHooks

instance ToQuery DescribeLifecycleHooks where
    toQuery = genericQuery def

data DescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse
    { _dlhaLifecycleHooks :: [LifecycleHook]
      -- ^ A list describing the lifecycle hooks that belong to the
      -- specified Auto Scaling group.
    } deriving (Show, Generic)

makeLenses ''DescribeLifecycleHooksResponse

instance FromXML DescribeLifecycleHooksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLifecycleHooks where
    type Sv DescribeLifecycleHooks = AutoScaling
    type Rs DescribeLifecycleHooks = DescribeLifecycleHooksResponse

    request = post "DescribeLifecycleHooks"
    response _ = xmlResponse
