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

-- Module      : Network.AWS.AutoScaling.ExitStandby
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Moves the specified instances out of 'Standby' mode.
--
-- For more information, see <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingInServiceState.html Auto Scaling InService State> in the /Auto ScalingDeveloper Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_ExitStandby.html>
module Network.AWS.AutoScaling.ExitStandby
    (
    -- * Request
      ExitStandby
    -- ** Request constructor
    , exitStandby
    -- ** Request lenses
    , es1AutoScalingGroupName
    , es1InstanceIds

    -- * Response
    , ExitStandbyResponse
    -- ** Response constructor
    , exitStandbyResponse
    -- ** Response lenses
    , esrActivities
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data ExitStandby = ExitStandby
    { _es1AutoScalingGroupName :: Text
    , _es1InstanceIds          :: List "InstanceIds" Text
    } deriving (Eq, Ord, Show)

-- | 'ExitStandby' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'es1AutoScalingGroupName' @::@ 'Text'
--
-- * 'es1InstanceIds' @::@ ['Text']
--
exitStandby :: Text -- ^ 'es1AutoScalingGroupName'
            -> ExitStandby
exitStandby p1 = ExitStandby
    { _es1AutoScalingGroupName = p1
    , _es1InstanceIds          = mempty
    }

-- | The name of the Auto Scaling group.
--
es1AutoScalingGroupName :: Lens' ExitStandby Text
es1AutoScalingGroupName =
    lens _es1AutoScalingGroupName (\s a -> s { _es1AutoScalingGroupName = a })

-- | One or more instance IDs. You must specify at least one instance ID.
--
es1InstanceIds :: Lens' ExitStandby [Text]
es1InstanceIds = lens _es1InstanceIds (\s a -> s { _es1InstanceIds = a }) . _List

newtype ExitStandbyResponse = ExitStandbyResponse
    { _esrActivities :: List "Activities" Activity
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList ExitStandbyResponse where
    type Item ExitStandbyResponse = Activity

    fromList = ExitStandbyResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _esrActivities

-- | 'ExitStandbyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esrActivities' @::@ ['Activity']
--
exitStandbyResponse :: ExitStandbyResponse
exitStandbyResponse = ExitStandbyResponse
    { _esrActivities = mempty
    }

-- | The activities related to moving instances out of 'Standby' mode.
--
esrActivities :: Lens' ExitStandbyResponse [Activity]
esrActivities = lens _esrActivities (\s a -> s { _esrActivities = a }) . _List

instance ToPath ExitStandby where
    toPath = const "/"

instance ToQuery ExitStandby where
    toQuery ExitStandby{..} = mconcat
        [ "AutoScalingGroupName" =? _es1AutoScalingGroupName
        , "InstanceIds"          =? _es1InstanceIds
        ]

instance ToHeaders ExitStandby

instance AWSRequest ExitStandby where
    type Sv ExitStandby = AutoScaling
    type Rs ExitStandby = ExitStandbyResponse

    request  = post "ExitStandby"
    response = xmlResponse

instance FromXML ExitStandbyResponse where
    parseXML = withElement "ExitStandbyResult" $ \x -> ExitStandbyResponse
        <$> x .@  "Activities"
