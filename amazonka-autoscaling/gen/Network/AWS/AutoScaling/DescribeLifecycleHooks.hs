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

-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHooks
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
module Network.AWS.AutoScaling.DescribeLifecycleHooks
    (
    -- * Request
      DescribeLifecycleHooksType
    -- ** Request constructor
    , describeLifecycleHooksType
    -- ** Request lenses
    , dlht1AutoScalingGroupName
    , dlht1LifecycleHookNames

    -- * Response
    , DescribeLifecycleHooksAnswer
    -- ** Response constructor
    , describeLifecycleHooksAnswer
    -- ** Response lenses
    , dlhaLifecycleHooks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeLifecycleHooksType = DescribeLifecycleHooksType
    { _dlht1AutoScalingGroupName :: Text
    , _dlht1LifecycleHookNames   :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeLifecycleHooksType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlht1AutoScalingGroupName' @::@ 'Text'
--
-- * 'dlht1LifecycleHookNames' @::@ ['Text']
--
describeLifecycleHooksType :: Text -- ^ 'dlht1AutoScalingGroupName'
                           -> DescribeLifecycleHooksType
describeLifecycleHooksType p1 = DescribeLifecycleHooksType
    { _dlht1AutoScalingGroupName = p1
    , _dlht1LifecycleHookNames   = mempty
    }

-- | The name of one or more Auto Scaling groups.
dlht1AutoScalingGroupName :: Lens' DescribeLifecycleHooksType Text
dlht1AutoScalingGroupName =
    lens _dlht1AutoScalingGroupName
        (\s a -> s { _dlht1AutoScalingGroupName = a })

-- | The name of one or more lifecycle hooks.
dlht1LifecycleHookNames :: Lens' DescribeLifecycleHooksType [Text]
dlht1LifecycleHookNames =
    lens _dlht1LifecycleHookNames (\s a -> s { _dlht1LifecycleHookNames = a })
instance ToQuery DescribeLifecycleHooksType

instance ToPath DescribeLifecycleHooksType where
    toPath = const "/"

newtype DescribeLifecycleHooksAnswer = DescribeLifecycleHooksAnswer
    { _dlhaLifecycleHooks :: [LifecycleHook]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DescribeLifecycleHooksAnswer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhaLifecycleHooks' @::@ ['LifecycleHook']
--
describeLifecycleHooksAnswer :: DescribeLifecycleHooksAnswer
describeLifecycleHooksAnswer = DescribeLifecycleHooksAnswer
    { _dlhaLifecycleHooks = mempty
    }

-- | A list describing the lifecycle hooks that belong to the specified Auto
-- Scaling group.
dlhaLifecycleHooks :: Lens' DescribeLifecycleHooksAnswer [LifecycleHook]
dlhaLifecycleHooks =
    lens _dlhaLifecycleHooks (\s a -> s { _dlhaLifecycleHooks = a })
instance FromXML DescribeLifecycleHooksAnswer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeLifecycleHooksAnswer"

instance AWSRequest DescribeLifecycleHooksType where
    type Sv DescribeLifecycleHooksType = AutoScaling
    type Rs DescribeLifecycleHooksType = DescribeLifecycleHooksAnswer

    request  = post "DescribeLifecycleHooks"
    response = xmlResponse $ \h x -> DescribeLifecycleHooksAnswer
        <$> x %| "LifecycleHooks"
