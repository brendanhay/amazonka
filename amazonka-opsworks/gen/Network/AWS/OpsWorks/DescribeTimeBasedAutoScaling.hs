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
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeTimeBasedAutoScaling.html>
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
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DescribeTimeBasedAutoScaling = DescribeTimeBasedAutoScaling
    { _dtbasInstanceIds :: List "InstanceIds" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

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
dtbasInstanceIds = lens _dtbasInstanceIds (\s a -> s { _dtbasInstanceIds = a }) . _List

newtype DescribeTimeBasedAutoScalingResponse = DescribeTimeBasedAutoScalingResponse
    { _dtbasrTimeBasedAutoScalingConfigurations :: List "TimeBasedAutoScalingConfigurations" TimeBasedAutoScalingConfiguration
    } deriving (Eq, Show, Monoid, Semigroup)

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
            . _List

instance ToPath DescribeTimeBasedAutoScaling where
    toPath = const "/"

instance ToQuery DescribeTimeBasedAutoScaling where
    toQuery = const mempty

instance ToHeaders DescribeTimeBasedAutoScaling

instance ToJSON DescribeTimeBasedAutoScaling where
    toJSON DescribeTimeBasedAutoScaling{..} = object
        [ "InstanceIds" .= _dtbasInstanceIds
        ]

instance AWSRequest DescribeTimeBasedAutoScaling where
    type Sv DescribeTimeBasedAutoScaling = OpsWorks
    type Rs DescribeTimeBasedAutoScaling = DescribeTimeBasedAutoScalingResponse

    request  = post "DescribeTimeBasedAutoScaling"
    response = jsonResponse

instance FromJSON DescribeTimeBasedAutoScalingResponse where
    parseJSON = withObject "DescribeTimeBasedAutoScalingResponse" $ \o -> DescribeTimeBasedAutoScalingResponse
        <$> o .:? "TimeBasedAutoScalingConfigurations"
