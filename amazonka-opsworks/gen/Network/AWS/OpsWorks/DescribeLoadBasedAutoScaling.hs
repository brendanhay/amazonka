{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes load-based auto scaling configurations for specified layers.
-- Required Permissions: To use this action, an IAM user must have a Show,
-- Deploy, or Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeLoadBasedAutoScaling.html>
module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
    (
    -- * Request
      DescribeLoadBasedAutoScaling
    -- ** Request constructor
    , describeLoadBasedAutoScaling
    -- ** Request lenses
    , dlbasLayerIds

    -- * Response
    , DescribeLoadBasedAutoScalingResponse
    -- ** Response constructor
    , describeLoadBasedAutoScalingResponse
    -- ** Response lenses
    , dlbasrLoadBasedAutoScalingConfigurations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling
    { _dlbasLayerIds :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeLoadBasedAutoScaling where
    type Item DescribeLoadBasedAutoScaling = Text

    fromList = DescribeLoadBasedAutoScaling . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dlbasLayerIds

-- | 'DescribeLoadBasedAutoScaling' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbasLayerIds' @::@ ['Text']
--
describeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling
describeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling
    { _dlbasLayerIds = mempty
    }

-- | An array of layer IDs.
dlbasLayerIds :: Lens' DescribeLoadBasedAutoScaling [Text]
dlbasLayerIds = lens _dlbasLayerIds (\s a -> s { _dlbasLayerIds = a })

newtype DescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse
    { _dlbasrLoadBasedAutoScalingConfigurations :: [LoadBasedAutoScalingConfiguration]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeLoadBasedAutoScalingResponse where
    type Item DescribeLoadBasedAutoScalingResponse = LoadBasedAutoScalingConfiguration

    fromList = DescribeLoadBasedAutoScalingResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dlbasrLoadBasedAutoScalingConfigurations

-- | 'DescribeLoadBasedAutoScalingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbasrLoadBasedAutoScalingConfigurations' @::@ ['LoadBasedAutoScalingConfiguration']
--
describeLoadBasedAutoScalingResponse :: DescribeLoadBasedAutoScalingResponse
describeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse
    { _dlbasrLoadBasedAutoScalingConfigurations = mempty
    }

-- | An array of LoadBasedAutoScalingConfiguration objects that describe each
-- layer's configuration.
dlbasrLoadBasedAutoScalingConfigurations :: Lens' DescribeLoadBasedAutoScalingResponse [LoadBasedAutoScalingConfiguration]
dlbasrLoadBasedAutoScalingConfigurations =
    lens _dlbasrLoadBasedAutoScalingConfigurations
        (\s a -> s { _dlbasrLoadBasedAutoScalingConfigurations = a })

instance ToPath DescribeLoadBasedAutoScaling where
    toPath = const "/"

instance ToQuery DescribeLoadBasedAutoScaling where
    toQuery = const mempty

instance ToHeaders DescribeLoadBasedAutoScaling

instance ToJSON DescribeLoadBasedAutoScaling where
    toJSON DescribeLoadBasedAutoScaling{..} = object
        [ "LayerIds" .= _dlbasLayerIds
        ]

instance AWSRequest DescribeLoadBasedAutoScaling where
    type Sv DescribeLoadBasedAutoScaling = OpsWorks
    type Rs DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScalingResponse

    request  = post "DescribeLoadBasedAutoScaling"
    response = jsonResponse

instance FromJSON DescribeLoadBasedAutoScalingResponse where
    parseJSON = withObject "DescribeLoadBasedAutoScalingResponse" $ \o -> DescribeLoadBasedAutoScalingResponse
        <$> o .: "LoadBasedAutoScalingConfigurations"
