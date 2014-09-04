{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the available types of lifecycle hooks.
module Network.AWS.AutoScaling.V2011_01_01.DescribeLifecycleHookTypes
    (
    -- * Request
      DescribeLifecycleHookTypes
    -- ** Request constructor
    , describeLifecycleHookTypes
    -- * Response
    , DescribeLifecycleHookTypesResponse
    -- ** Response lenses
    , dlhtaLifecycleHookTypes
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeLifecycleHookTypes' request.
describeLifecycleHookTypes :: DescribeLifecycleHookTypes
describeLifecycleHookTypes = DescribeLifecycleHookTypes
{-# INLINE describeLifecycleHookTypes #-}

data DescribeLifecycleHookTypes = DescribeLifecycleHookTypes
    deriving (Eq, Show, Generic)

instance ToQuery DescribeLifecycleHookTypes where
    toQuery = genericQuery def

data DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse
    { _dlhtaLifecycleHookTypes :: [Text]
      -- ^ Returns a list of all notification types supported by Auto
      -- Scaling. They are: autoscaling:EC2_INSTANCE_LAUNCHING
      -- autoscaling:EC2_INSTANCE_TERMINATING.
    } deriving (Show, Generic)

-- | Returns a list of all notification types supported by Auto Scaling. They
-- are: autoscaling:EC2_INSTANCE_LAUNCHING
-- autoscaling:EC2_INSTANCE_TERMINATING.
dlhtaLifecycleHookTypes :: Lens' DescribeLifecycleHookTypesResponse ([Text])
dlhtaLifecycleHookTypes f x =
    f (_dlhtaLifecycleHookTypes x)
        <&> \y -> x { _dlhtaLifecycleHookTypes = y }
{-# INLINE dlhtaLifecycleHookTypes #-}

instance FromXML DescribeLifecycleHookTypesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLifecycleHookTypes where
    type Sv DescribeLifecycleHookTypes = AutoScaling
    type Rs DescribeLifecycleHookTypes = DescribeLifecycleHookTypesResponse

    request = post "DescribeLifecycleHookTypes"
    response _ = xmlResponse
