{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingNotificationTypes
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
module Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingNotificationTypes where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.AutoScaling.V2011_01_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes
    deriving (Eq, Show, Generic)

instance ToQuery DescribeAutoScalingNotificationTypes where
    toQuery = genericToQuery def

instance AWSRequest DescribeAutoScalingNotificationTypes where
    type Sv DescribeAutoScalingNotificationTypes = AutoScaling
    type Rs DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypesResponse

    request = post "DescribeAutoScalingNotificationTypes"
    response _ = xmlResponse

data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
    { _dasntaAutoScalingNotificationTypes :: [Text]
      -- ^ Returns a list of all notification types supported by Auto
      -- Scaling. They are: autoscaling:EC2_INSTANCE_LAUNCH
      -- autoscaling:EC2_INSTANCE_LAUNCH_ERROR
      -- autoscaling:EC2_INSTANCE_TERMINATE
      -- autoscaling:EC2_INSTANCE_TERMINATE_ERROR
      -- autoscaling:TEST_NOTIFICATION
      -- https://autoscaling.amazonaws.com/?Version=2011-01-01&Action=DescribeAutoScalingNotificationTypes
      -- &AUTHPARAMS autoscaling:EC2_INSTANCE_LAUNCH
      -- autoscaling:EC2_INSTANCE_LAUNCH_ERROR
      -- autoscaling:EC2_INSTANCE_TERMINATE
      -- autoscaling:EC2_INSTANCE_TERMINATE_ERROR
      -- autoscaling:TEST_NOTIFICATION
      -- 42fc6794-bf21-11e2-a1cf-ff3dEXAMPLE.
    } deriving (Generic)

instance FromXML DescribeAutoScalingNotificationTypesResponse where
    fromXMLOptions = xmlOptions
