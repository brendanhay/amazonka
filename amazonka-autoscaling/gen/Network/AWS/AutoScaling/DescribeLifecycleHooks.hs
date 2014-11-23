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

-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHooks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the lifecycle hooks for the specified Auto Scaling group.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLifecycleHooks.html>
module Network.AWS.AutoScaling.DescribeLifecycleHooks
    (
    -- * Request
      DescribeLifecycleHooks
    -- ** Request constructor
    , describeLifecycleHooks
    -- ** Request lenses
    , dlhAutoScalingGroupName
    , dlhLifecycleHookNames

    -- * Response
    , DescribeLifecycleHooksResponse
    -- ** Response constructor
    , describeLifecycleHooksResponse
    -- ** Response lenses
    , dlhrLifecycleHooks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeLifecycleHooks = DescribeLifecycleHooks
    { _dlhAutoScalingGroupName :: Text
    , _dlhLifecycleHookNames   :: List "LifecycleHookNames" Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeLifecycleHooks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhAutoScalingGroupName' @::@ 'Text'
--
-- * 'dlhLifecycleHookNames' @::@ ['Text']
--
describeLifecycleHooks :: Text -- ^ 'dlhAutoScalingGroupName'
                       -> DescribeLifecycleHooks
describeLifecycleHooks p1 = DescribeLifecycleHooks
    { _dlhAutoScalingGroupName = p1
    , _dlhLifecycleHookNames   = mempty
    }

-- | The name of the group.
dlhAutoScalingGroupName :: Lens' DescribeLifecycleHooks Text
dlhAutoScalingGroupName =
    lens _dlhAutoScalingGroupName (\s a -> s { _dlhAutoScalingGroupName = a })

-- | The names of one or more lifecycle hooks.
dlhLifecycleHookNames :: Lens' DescribeLifecycleHooks [Text]
dlhLifecycleHookNames =
    lens _dlhLifecycleHookNames (\s a -> s { _dlhLifecycleHookNames = a })
        . _List

newtype DescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse
    { _dlhrLifecycleHooks :: List "LifecycleHooks" LifecycleHook
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeLifecycleHooksResponse where
    type Item DescribeLifecycleHooksResponse = LifecycleHook

    fromList = DescribeLifecycleHooksResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dlhrLifecycleHooks

-- | 'DescribeLifecycleHooksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhrLifecycleHooks' @::@ ['LifecycleHook']
--
describeLifecycleHooksResponse :: DescribeLifecycleHooksResponse
describeLifecycleHooksResponse = DescribeLifecycleHooksResponse
    { _dlhrLifecycleHooks = mempty
    }

-- | The lifecycle hooks for the specified group.
dlhrLifecycleHooks :: Lens' DescribeLifecycleHooksResponse [LifecycleHook]
dlhrLifecycleHooks =
    lens _dlhrLifecycleHooks (\s a -> s { _dlhrLifecycleHooks = a })
        . _List

instance ToPath DescribeLifecycleHooks where
    toPath = const "/"

instance ToQuery DescribeLifecycleHooks where
    toQuery DescribeLifecycleHooks{..} = mconcat
        [ "AutoScalingGroupName" =? _dlhAutoScalingGroupName
        , "LifecycleHookNames"   =? _dlhLifecycleHookNames
        ]

instance ToHeaders DescribeLifecycleHooks

instance AWSRequest DescribeLifecycleHooks where
    type Sv DescribeLifecycleHooks = AutoScaling
    type Rs DescribeLifecycleHooks = DescribeLifecycleHooksResponse

    request  = post "DescribeLifecycleHooks"
    response = xmlResponse

instance FromXML DescribeLifecycleHooksResponse where
    parseXML = withElement "DescribeLifecycleHooksResult" $ \x -> DescribeLifecycleHooksResponse
        <$> x .@? "LifecycleHooks"
