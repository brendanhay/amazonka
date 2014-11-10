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

-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the available types of lifecycle hooks.
module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
    (
    -- * Request
      DescribeLifecycleHookTypes
    -- ** Request constructor
    , describeLifecycleHookTypes

    -- * Response
    , DescribeLifecycleHookTypesAnswer
    -- ** Response constructor
    , describeLifecycleHookTypesResponse
    -- ** Response lenses
    , dlhtaLifecycleHookTypes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeLifecycleHookTypes = DescribeLifecycleHookTypes

-- | 'DescribeLifecycleHookTypes' constructor.
describeLifecycleHookTypes :: DescribeLifecycleHookTypes
describeLifecycleHookTypes = DescribeLifecycleHookTypes

instance ToPath DescribeLifecycleHookTypes where
    toPath = const "/"

instance ToQuery DescribeLifecycleHookTypes

newtype DescribeLifecycleHookTypesAnswer = DescribeLifecycleHookTypesAnswer
    { _dlhtaLifecycleHookTypes :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DescribeLifecycleHookTypesAnswer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhtaLifecycleHookTypes' @::@ ['Text']
--
describeLifecycleHookTypesResponse :: DescribeLifecycleHookTypesAnswer
describeLifecycleHookTypesResponse = DescribeLifecycleHookTypesAnswer
    { _dlhtaLifecycleHookTypes = mempty
    }

-- | Returns a list of all notification types supported by Auto Scaling. They
-- are: autoscaling:EC2_INSTANCE_LAUNCHING
-- autoscaling:EC2_INSTANCE_TERMINATING.
dlhtaLifecycleHookTypes :: Lens' DescribeLifecycleHookTypesAnswer [Text]
dlhtaLifecycleHookTypes =
    lens _dlhtaLifecycleHookTypes (\s a -> s { _dlhtaLifecycleHookTypes = a })

instance AWSRequest DescribeLifecycleHookTypes where
    type Sv DescribeLifecycleHookTypes = AutoScaling
    type Rs DescribeLifecycleHookTypes = DescribeLifecycleHookTypesAnswer

    request  = post "DescribeLifecycleHookTypes"
    response = xmlResponse $ \h x -> DescribeLifecycleHookTypesAnswer
        <$> x %| "LifecycleHookTypes"
