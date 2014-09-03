{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of notification actions associated with Auto Scaling groups
-- for specified events.
module Network.AWS.AutoScaling.V2011_01_01.DescribeNotificationConfigurations
    (
    -- * Request
      DescribeNotificationConfigurations
    -- ** Request constructor
    , describeNotificationConfigurations
    -- ** Request lenses
    , dncuAutoScalingGroupNames
    , dncuMaxRecords
    , dncuNextToken

    -- * Response
    , DescribeNotificationConfigurationsResponse
    -- ** Response lenses
    , dncaNotificationConfigurations
    , dncaNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeNotificationConfigurations' request.
describeNotificationConfigurations :: DescribeNotificationConfigurations
describeNotificationConfigurations = DescribeNotificationConfigurations
    { _dncuAutoScalingGroupNames = mempty
    , _dncuMaxRecords = Nothing
    , _dncuNextToken = Nothing
    }

data DescribeNotificationConfigurations = DescribeNotificationConfigurations
    { _dncuAutoScalingGroupNames :: [Text]
      -- ^ The name of the Auto Scaling group.
    , _dncuMaxRecords :: Maybe Integer
      -- ^ Maximum number of records to be returned.
    , _dncuNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of
      -- returned results for pagination.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
dncuAutoScalingGroupNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeNotificationConfigurations
    -> f DescribeNotificationConfigurations
dncuAutoScalingGroupNames f x =
    (\y -> x { _dncuAutoScalingGroupNames = y })
       <$> f (_dncuAutoScalingGroupNames x)
{-# INLINE dncuAutoScalingGroupNames #-}

-- | Maximum number of records to be returned.
dncuMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeNotificationConfigurations
    -> f DescribeNotificationConfigurations
dncuMaxRecords f x =
    (\y -> x { _dncuMaxRecords = y })
       <$> f (_dncuMaxRecords x)
{-# INLINE dncuMaxRecords #-}

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dncuNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeNotificationConfigurations
    -> f DescribeNotificationConfigurations
dncuNextToken f x =
    (\y -> x { _dncuNextToken = y })
       <$> f (_dncuNextToken x)
{-# INLINE dncuNextToken #-}

instance ToQuery DescribeNotificationConfigurations where
    toQuery = genericQuery def

data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
    { _dncaNotificationConfigurations :: [NotificationConfiguration]
      -- ^ The list of notification configurations.
    , _dncaNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of
      -- returned results for pagination.
    } deriving (Show, Generic)

-- | The list of notification configurations.
dncaNotificationConfigurations
    :: Functor f
    => ([NotificationConfiguration]
    -> f ([NotificationConfiguration]))
    -> DescribeNotificationConfigurationsResponse
    -> f DescribeNotificationConfigurationsResponse
dncaNotificationConfigurations f x =
    (\y -> x { _dncaNotificationConfigurations = y })
       <$> f (_dncaNotificationConfigurations x)
{-# INLINE dncaNotificationConfigurations #-}

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dncaNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeNotificationConfigurationsResponse
    -> f DescribeNotificationConfigurationsResponse
dncaNextToken f x =
    (\y -> x { _dncaNextToken = y })
       <$> f (_dncaNextToken x)
{-# INLINE dncaNextToken #-}

instance FromXML DescribeNotificationConfigurationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeNotificationConfigurations where
    type Sv DescribeNotificationConfigurations = AutoScaling
    type Rs DescribeNotificationConfigurations = DescribeNotificationConfigurationsResponse

    request = post "DescribeNotificationConfigurations"
    response _ = xmlResponse

instance AWSPager DescribeNotificationConfigurations where
    next rq rs = (\x -> rq { _dncuNextToken = Just x })
        <$> (_dncaNextToken rs)
