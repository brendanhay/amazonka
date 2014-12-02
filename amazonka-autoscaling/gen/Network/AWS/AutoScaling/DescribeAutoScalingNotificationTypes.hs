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

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists the notification types that are supported by Auto Scaling.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingNotificationTypes.html>
module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
    (
    -- * Request
      DescribeAutoScalingNotificationTypes
    -- ** Request constructor
    , describeAutoScalingNotificationTypes

    -- * Response
    , DescribeAutoScalingNotificationTypesResponse
    -- ** Response constructor
    , describeAutoScalingNotificationTypesResponse
    -- ** Response lenses
    , dasntrAutoScalingNotificationTypes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAutoScalingNotificationTypes' constructor.
describeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypes
describeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes

newtype DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
    { _dasntrAutoScalingNotificationTypes :: List "member" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeAutoScalingNotificationTypesResponse where
    type Item DescribeAutoScalingNotificationTypesResponse = Text

    fromList = DescribeAutoScalingNotificationTypesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dasntrAutoScalingNotificationTypes

-- | 'DescribeAutoScalingNotificationTypesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasntrAutoScalingNotificationTypes' @::@ ['Text']
--
describeAutoScalingNotificationTypesResponse :: DescribeAutoScalingNotificationTypesResponse
describeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
    { _dasntrAutoScalingNotificationTypes = mempty
    }

-- | One or more of the following notification types:
--
-- 'autoscaling:EC2_INSTANCE_LAUNCH'
--
-- 'autoscaling:EC2_INSTANCE_LAUNCH_ERROR'
--
-- 'autoscaling:EC2_INSTANCE_TERMINATE'
--
-- 'autoscaling:EC2_INSTANCE_TERMINATE_ERROR'
--
-- 'autoscaling:TEST_NOTIFICATION'
--
--
dasntrAutoScalingNotificationTypes :: Lens' DescribeAutoScalingNotificationTypesResponse [Text]
dasntrAutoScalingNotificationTypes =
    lens _dasntrAutoScalingNotificationTypes
        (\s a -> s { _dasntrAutoScalingNotificationTypes = a })
            . _List

instance ToPath DescribeAutoScalingNotificationTypes where
    toPath = const "/"

instance ToQuery DescribeAutoScalingNotificationTypes where
    toQuery = const mempty

instance ToHeaders DescribeAutoScalingNotificationTypes

instance AWSRequest DescribeAutoScalingNotificationTypes where
    type Sv DescribeAutoScalingNotificationTypes = AutoScaling
    type Rs DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypesResponse

    request  = post "DescribeAutoScalingNotificationTypes"
    response = xmlResponse

instance FromXML DescribeAutoScalingNotificationTypesResponse where
    parseXML = withElement "DescribeAutoScalingNotificationTypesResult" $ \x -> DescribeAutoScalingNotificationTypesResponse
        <$> x .@? "AutoScalingNotificationTypes" .!@ mempty
