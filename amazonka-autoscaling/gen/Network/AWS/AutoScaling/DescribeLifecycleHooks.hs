{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling
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
module Network.AWS.AutoScaling
    (
    -- * Request
      DescribeLifecycleHooks
    -- ** Request constructor
    , mkDescribeLifecycleHooks
    -- ** Request lenses
    , dlh1AutoScalingGroupName
    , dlh1LifecycleHookNames

    -- * Response
    , DescribeLifecycleHooksResponse
    -- ** Response constructor
    , mkDescribeLifecycleHooksResponse
    -- ** Response lenses
    , dlhrrLifecycleHooks
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data DescribeLifecycleHooks = DescribeLifecycleHooks
    { _dlh1AutoScalingGroupName :: !Text
    , _dlh1LifecycleHookNames :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLifecycleHooks' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @LifecycleHookNames ::@ @[Text]@
--
mkDescribeLifecycleHooks :: Text -- ^ 'dlh1AutoScalingGroupName'
                         -> DescribeLifecycleHooks
mkDescribeLifecycleHooks p1 = DescribeLifecycleHooks
    { _dlh1AutoScalingGroupName = p1
    , _dlh1LifecycleHookNames = mempty
    }

-- | The name of one or more Auto Scaling groups.
dlh1AutoScalingGroupName :: Lens' DescribeLifecycleHooks Text
dlh1AutoScalingGroupName =
    lens _dlh1AutoScalingGroupName
         (\s a -> s { _dlh1AutoScalingGroupName = a })

-- | The name of one or more lifecycle hooks.
dlh1LifecycleHookNames :: Lens' DescribeLifecycleHooks [Text]
dlh1LifecycleHookNames =
    lens _dlh1LifecycleHookNames (\s a -> s { _dlh1LifecycleHookNames = a })

instance ToQuery DescribeLifecycleHooks where
    toQuery = genericQuery def

-- | The output of the DescribeLifecycleHooks action.
newtype DescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse
    { _dlhrrLifecycleHooks :: [LifecycleHook]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLifecycleHooksResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LifecycleHooks ::@ @[LifecycleHook]@
--
mkDescribeLifecycleHooksResponse :: DescribeLifecycleHooksResponse
mkDescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse
    { _dlhrrLifecycleHooks = mempty
    }

-- | A list describing the lifecycle hooks that belong to the specified Auto
-- Scaling group.
dlhrrLifecycleHooks :: Lens' DescribeLifecycleHooksResponse [LifecycleHook]
dlhrrLifecycleHooks =
    lens _dlhrrLifecycleHooks (\s a -> s { _dlhrrLifecycleHooks = a })

instance FromXML DescribeLifecycleHooksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLifecycleHooks where
    type Sv DescribeLifecycleHooks = AutoScaling
    type Rs DescribeLifecycleHooks = DescribeLifecycleHooksResponse

    request = post "DescribeLifecycleHooks"
    response _ = xmlResponse
