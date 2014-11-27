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

-- Module      : Network.AWS.AutoScaling.EnterStandby
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

-- | Moves the specified instances into 'Standby' mode.
--
-- For more information, see <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingInServiceState.html Auto Scaling InService State> in the /Auto ScalingDeveloper Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_EnterStandby.html>
module Network.AWS.AutoScaling.EnterStandby
    (
    -- * Request
      EnterStandby
    -- ** Request constructor
    , enterStandby
    -- ** Request lenses
    , esAutoScalingGroupName
    , esInstanceIds
    , esShouldDecrementDesiredCapacity

    -- * Response
    , EnterStandbyResponse
    -- ** Response constructor
    , enterStandbyResponse
    -- ** Response lenses
    , esr1Activities
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data EnterStandby = EnterStandby
    { _esAutoScalingGroupName           :: Text
    , _esInstanceIds                    :: List "InstanceIds" Text
    , _esShouldDecrementDesiredCapacity :: Bool
    } deriving (Eq, Ord, Show)

-- | 'EnterStandby' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esAutoScalingGroupName' @::@ 'Text'
--
-- * 'esInstanceIds' @::@ ['Text']
--
-- * 'esShouldDecrementDesiredCapacity' @::@ 'Bool'
--
enterStandby :: Text -- ^ 'esAutoScalingGroupName'
             -> Bool -- ^ 'esShouldDecrementDesiredCapacity'
             -> EnterStandby
enterStandby p1 p2 = EnterStandby
    { _esAutoScalingGroupName           = p1
    , _esShouldDecrementDesiredCapacity = p2
    , _esInstanceIds                    = mempty
    }

-- | The name of the Auto Scaling group.
esAutoScalingGroupName :: Lens' EnterStandby Text
esAutoScalingGroupName =
    lens _esAutoScalingGroupName (\s a -> s { _esAutoScalingGroupName = a })

-- | One or more instances to move into 'Standby' mode. You must specify at least
-- one instance ID.
esInstanceIds :: Lens' EnterStandby [Text]
esInstanceIds = lens _esInstanceIds (\s a -> s { _esInstanceIds = a }) . _List

-- | Specifies whether the instances moved to 'Standby' mode count as part of the
-- Auto Scaling group's desired capacity. If set, the desired capacity for the
-- Auto Scaling group decrements by the number of instances moved to 'Standby'
-- mode.
esShouldDecrementDesiredCapacity :: Lens' EnterStandby Bool
esShouldDecrementDesiredCapacity =
    lens _esShouldDecrementDesiredCapacity
        (\s a -> s { _esShouldDecrementDesiredCapacity = a })

newtype EnterStandbyResponse = EnterStandbyResponse
    { _esr1Activities :: List "Activities" Activity
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList EnterStandbyResponse where
    type Item EnterStandbyResponse = Activity

    fromList = EnterStandbyResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _esr1Activities

-- | 'EnterStandbyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esr1Activities' @::@ ['Activity']
--
enterStandbyResponse :: EnterStandbyResponse
enterStandbyResponse = EnterStandbyResponse
    { _esr1Activities = mempty
    }

-- | The activities related to moving instances into 'Standby' mode.
esr1Activities :: Lens' EnterStandbyResponse [Activity]
esr1Activities = lens _esr1Activities (\s a -> s { _esr1Activities = a }) . _List

instance ToPath EnterStandby where
    toPath = const "/"

instance ToQuery EnterStandby where
    toQuery EnterStandby{..} = mconcat
        [ "AutoScalingGroupName"           =? _esAutoScalingGroupName
        , "InstanceIds"                    =? _esInstanceIds
        , "ShouldDecrementDesiredCapacity" =? _esShouldDecrementDesiredCapacity
        ]

instance ToHeaders EnterStandby

instance AWSRequest EnterStandby where
    type Sv EnterStandby = AutoScaling
    type Rs EnterStandby = EnterStandbyResponse

    request  = post "EnterStandby"
    response = xmlResponse

instance FromXML EnterStandbyResponse where
    parseXML = withElement "EnterStandbyResult" $ \x -> EnterStandbyResponse
        <$> x .@  "Activities"
