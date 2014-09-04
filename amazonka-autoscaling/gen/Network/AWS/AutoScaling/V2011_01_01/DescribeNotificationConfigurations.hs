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
    , mkDescribeNotificationConfigurationsType
    -- ** Request lenses
    , dncuAutoScalingGroupNames
    , dncuNextToken
    , dncuMaxRecords

    -- * Response
    , DescribeNotificationConfigurationsResponse
    -- ** Response lenses
    , dncaNotificationConfigurations
    , dncaNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeNotificationConfigurations' request.
mkDescribeNotificationConfigurationsType :: DescribeNotificationConfigurations
mkDescribeNotificationConfigurationsType = DescribeNotificationConfigurations
    { _dncuAutoScalingGroupNames = mempty
    , _dncuNextToken = Nothing
    , _dncuMaxRecords = Nothing
    }
{-# INLINE mkDescribeNotificationConfigurationsType #-}

data DescribeNotificationConfigurations = DescribeNotificationConfigurations
    { _dncuAutoScalingGroupNames :: [Text]
      -- ^ The name of the Auto Scaling group.
    , _dncuNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of
      -- returned results for pagination.
    , _dncuMaxRecords :: Maybe Integer
      -- ^ Maximum number of records to be returned.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
dncuAutoScalingGroupNames :: Lens' DescribeNotificationConfigurations ([Text])
dncuAutoScalingGroupNames = lens _dncuAutoScalingGroupNames (\s a -> s { _dncuAutoScalingGroupNames = a })
{-# INLINE dncuAutoScalingGroupNames #-}

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dncuNextToken :: Lens' DescribeNotificationConfigurations (Maybe Text)
dncuNextToken = lens _dncuNextToken (\s a -> s { _dncuNextToken = a })
{-# INLINE dncuNextToken #-}

-- | Maximum number of records to be returned.
dncuMaxRecords :: Lens' DescribeNotificationConfigurations (Maybe Integer)
dncuMaxRecords = lens _dncuMaxRecords (\s a -> s { _dncuMaxRecords = a })
{-# INLINE dncuMaxRecords #-}

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
dncaNotificationConfigurations :: Lens' DescribeNotificationConfigurationsResponse ([NotificationConfiguration])
dncaNotificationConfigurations = lens _dncaNotificationConfigurations (\s a -> s { _dncaNotificationConfigurations = a })
{-# INLINE dncaNotificationConfigurations #-}

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dncaNextToken :: Lens' DescribeNotificationConfigurationsResponse (Maybe Text)
dncaNextToken = lens _dncaNextToken (\s a -> s { _dncaNextToken = a })
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
