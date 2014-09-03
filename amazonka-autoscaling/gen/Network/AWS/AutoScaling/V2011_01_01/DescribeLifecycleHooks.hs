{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.AutoScaling.V2011_01_01.DescribeLifecycleHooks
    (
    -- * Request
      DescribeLifecycleHooks
    -- ** Request constructor
    , describeLifecycleHooks
    -- ** Request lenses
    , dlhuAutoScalingGroupName
    , dlhuLifecycleHookNames

    -- * Response
    , DescribeLifecycleHooksResponse
    -- ** Response lenses
    , dlhbLifecycleHooks
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeLifecycleHooks' request.
describeLifecycleHooks :: Text -- ^ 'dlhuAutoScalingGroupName'
                       -> DescribeLifecycleHooks
describeLifecycleHooks p1 = DescribeLifecycleHooks
    { _dlhuAutoScalingGroupName = p1
    , _dlhuLifecycleHookNames = mempty
    }

data DescribeLifecycleHooks = DescribeLifecycleHooks
    { _dlhuAutoScalingGroupName :: Text
      -- ^ The name of one or more Auto Scaling groups.
    , _dlhuLifecycleHookNames :: [Text]
      -- ^ The name of one or more lifecycle hooks.
    } deriving (Show, Generic)

-- | The name of one or more Auto Scaling groups.
dlhuAutoScalingGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeLifecycleHooks
    -> f DescribeLifecycleHooks
dlhuAutoScalingGroupName f x =
    (\y -> x { _dlhuAutoScalingGroupName = y })
       <$> f (_dlhuAutoScalingGroupName x)
{-# INLINE dlhuAutoScalingGroupName #-}

-- | The name of one or more lifecycle hooks.
dlhuLifecycleHookNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeLifecycleHooks
    -> f DescribeLifecycleHooks
dlhuLifecycleHookNames f x =
    (\y -> x { _dlhuLifecycleHookNames = y })
       <$> f (_dlhuLifecycleHookNames x)
{-# INLINE dlhuLifecycleHookNames #-}

instance ToQuery DescribeLifecycleHooks where
    toQuery = genericQuery def

data DescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse
    { _dlhbLifecycleHooks :: [LifecycleHook]
      -- ^ A list describing the lifecycle hooks that belong to the
      -- specified Auto Scaling group.
    } deriving (Show, Generic)

-- | A list describing the lifecycle hooks that belong to the specified Auto
-- Scaling group.
dlhbLifecycleHooks
    :: Functor f
    => ([LifecycleHook]
    -> f ([LifecycleHook]))
    -> DescribeLifecycleHooksResponse
    -> f DescribeLifecycleHooksResponse
dlhbLifecycleHooks f x =
    (\y -> x { _dlhbLifecycleHooks = y })
       <$> f (_dlhbLifecycleHooks x)
{-# INLINE dlhbLifecycleHooks #-}

instance FromXML DescribeLifecycleHooksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLifecycleHooks where
    type Sv DescribeLifecycleHooks = AutoScaling
    type Rs DescribeLifecycleHooks = DescribeLifecycleHooksResponse

    request = post "DescribeLifecycleHooks"
    response _ = xmlResponse
