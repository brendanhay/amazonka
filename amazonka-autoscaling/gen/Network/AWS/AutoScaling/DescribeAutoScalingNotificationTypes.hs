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

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of all notification types that are supported by Auto
-- Scaling.
module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
    (
    -- * Request
      DescribeAutoScalingNotificationTypes
    -- ** Request constructor
    , describeAutoScalingNotificationTypes

    -- * Response
    , DescribeAutoScalingNotificationTypesAnswer
    -- ** Response constructor
    , describeAutoScalingNotificationTypesAnswer
    -- ** Response lenses
    , dasntaAutoScalingNotificationTypes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes

-- | 'DescribeAutoScalingNotificationTypes' constructor.
describeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypes
describeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes

instance ToPath DescribeAutoScalingNotificationTypes where
    toPath = const "/"

instance ToQuery DescribeAutoScalingNotificationTypes

newtype DescribeAutoScalingNotificationTypesAnswer = DescribeAutoScalingNotificationTypesAnswer
    { _dasntaAutoScalingNotificationTypes :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DescribeAutoScalingNotificationTypesAnswer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasntaAutoScalingNotificationTypes' @::@ ['Text']
--
describeAutoScalingNotificationTypesAnswer :: DescribeAutoScalingNotificationTypesAnswer
describeAutoScalingNotificationTypesAnswer = DescribeAutoScalingNotificationTypesAnswer
    { _dasntaAutoScalingNotificationTypes = mempty
    }

-- | Returns a list of all notification types supported by Auto Scaling. They
-- are: autoscaling:EC2_INSTANCE_LAUNCH
-- autoscaling:EC2_INSTANCE_LAUNCH_ERROR autoscaling:EC2_INSTANCE_TERMINATE
-- autoscaling:EC2_INSTANCE_TERMINATE_ERROR autoscaling:TEST_NOTIFICATION.
dasntaAutoScalingNotificationTypes :: Lens' DescribeAutoScalingNotificationTypesAnswer [Text]
dasntaAutoScalingNotificationTypes =
    lens _dasntaAutoScalingNotificationTypes
        (\s a -> s { _dasntaAutoScalingNotificationTypes = a })

instance AWSRequest DescribeAutoScalingNotificationTypes where
    type Sv DescribeAutoScalingNotificationTypes = AutoScaling
    type Rs DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypesAnswer

    request  = post "DescribeAutoScalingNotificationTypes"
    response = const . xmlResponse $ \h x -> DescribeAutoScalingNotificationTypesAnswer
newtype
