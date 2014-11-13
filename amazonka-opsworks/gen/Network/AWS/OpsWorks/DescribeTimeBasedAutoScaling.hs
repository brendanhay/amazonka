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

-- Module      : Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes time-based auto scaling configurations for specified instances.
-- Required Permissions: To use this action, an IAM user must have a Show,
-- Deploy, or Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
    (
    -- * Request
      DescribeTimeBasedAutoScaling
    -- ** Request constructor
    , describeTimeBasedAutoScaling
    -- ** Request lenses
    , dtbasInstanceIds

    -- * Response
    , DescribeTimeBasedAutoScalingResponse
    -- ** Response constructor
    , describeTimeBasedAutoScalingResponse
    -- ** Response lenses
    , dtbasrTimeBasedAutoScalingConfigurations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

newtype DescribeTimeBasedAutoScaling = DescribeTimeBasedAutoScaling
    { _dtbasInstanceIds :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeTimeBasedAutoScaling where
    type Item DescribeTimeBasedAutoScaling = Text

    fromList = DescribeTimeBasedAutoScaling . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dtbasInstanceIds

-- | 'DescribeTimeBasedAutoScaling' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtbasInstanceIds' @::@ ['Text']
--
describeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling
describeTimeBasedAutoScaling = DescribeTimeBasedAutoScaling
    { _dtbasInstanceIds = mempty
    }

-- | An array of instance IDs.
dtbasInstanceIds :: Lens' DescribeTimeBasedAutoScaling [Text]
dtbasInstanceIds = lens _dtbasInstanceIds (\s a -> s { _dtbasInstanceIds = a })

instance ToPath DescribeTimeBasedAutoScaling where
    toPath = const "/"

instance ToQuery DescribeTimeBasedAutoScaling where
    toQuery = const mempty

instance ToHeaders DescribeTimeBasedAutoScaling

instance ToBody DescribeTimeBasedAutoScaling where
    toBody = toBody . encode . _dtbasInstanceIds

newtype DescribeTimeBasedAutoScalingResponse = DescribeTimeBasedAutoScalingResponse
    { _dtbasrTimeBasedAutoScalingConfigurations :: [TimeBasedAutoScalingConfiguration]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeTimeBasedAutoScalingResponse where
    type Item DescribeTimeBasedAutoScalingResponse = TimeBasedAutoScalingConfiguration

    fromList = DescribeTimeBasedAutoScalingResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dtbasrTimeBasedAutoScalingConfigurations

-- | 'DescribeTimeBasedAutoScalingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtbasrTimeBasedAutoScalingConfigurations' @::@ ['TimeBasedAutoScalingConfiguration']
--
describeTimeBasedAutoScalingResponse :: DescribeTimeBasedAutoScalingResponse
describeTimeBasedAutoScalingResponse = DescribeTimeBasedAutoScalingResponse
    { _dtbasrTimeBasedAutoScalingConfigurations = mempty
    }

-- | An array of TimeBasedAutoScalingConfiguration objects that describe the
-- configuration for the specified instances.
dtbasrTimeBasedAutoScalingConfigurations :: Lens' DescribeTimeBasedAutoScalingResponse [TimeBasedAutoScalingConfiguration]
dtbasrTimeBasedAutoScalingConfigurations =
    lens _dtbasrTimeBasedAutoScalingConfigurations
        (\s a -> s { _dtbasrTimeBasedAutoScalingConfigurations = a })

-- FromJSON

instance AWSRequest DescribeTimeBasedAutoScaling where
    type Sv DescribeTimeBasedAutoScaling = OpsWorks
    type Rs DescribeTimeBasedAutoScaling = DescribeTimeBasedAutoScalingResponse

    request  = post'
    response = jsonResponse $ \h o -> DescribeTimeBasedAutoScalingResponse
        <$> o .: "TimeBasedAutoScalingConfigurations"
