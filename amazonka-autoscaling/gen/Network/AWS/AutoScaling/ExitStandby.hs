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

-- Module      : Network.AWS.AutoScaling.ExitStandby
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Move an instance out of Standby mode. To learn more about how to put
-- instances that are in a Standby mode back into service, see Auto Scaling
-- InService State.
module Network.AWS.AutoScaling.ExitStandby
    (
    -- * Request
      ExitStandbyQuery
    -- ** Request constructor
    , exitStandbyQuery
    -- ** Request lenses
    , esq1AutoScalingGroupName
    , esq1InstanceIds

    -- * Response
    , ExitStandbyAnswer
    -- ** Response constructor
    , exitStandbyAnswer
    -- ** Response lenses
    , esa1Activities
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data ExitStandbyQuery = ExitStandbyQuery
    { _esq1AutoScalingGroupName :: Text
    , _esq1InstanceIds          :: [Text]
    } (Eq, Ord, Show, Generic)

-- | 'ExitStandbyQuery' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esq1AutoScalingGroupName' @::@ 'Text'
--
-- * 'esq1InstanceIds' @::@ ['Text']
--
exitStandbyQuery :: Text -- ^ 'esq1AutoScalingGroupName'
                 -> ExitStandbyQuery
exitStandbyQuery p1 = ExitStandbyQuery
    { _esq1AutoScalingGroupName = p1
    , _esq1InstanceIds          = mempty
    }

-- | The name of the Auto Scaling group from which to move instances out of
-- Standby mode.
esq1AutoScalingGroupName :: Lens' ExitStandbyQuery Text
esq1AutoScalingGroupName =
    lens _esq1AutoScalingGroupName
        (\s a -> s { _esq1AutoScalingGroupName = a })

-- | A list of instances to move out of Standby mode. You must specify at
-- least one instance ID.
esq1InstanceIds :: Lens' ExitStandbyQuery [Text]
esq1InstanceIds = lens _esq1InstanceIds (\s a -> s { _esq1InstanceIds = a })
instance ToQuery ExitStandbyQuery

instance ToPath ExitStandbyQuery where
    toPath = const "/"

newtype ExitStandbyAnswer = ExitStandbyAnswer
    { _esa1Activities :: [Activity]
    } (Eq, Show, Generic, Foldable, Traversable, Monoid, Semigroup)

-- | 'ExitStandbyAnswer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esa1Activities' @::@ ['Activity']
--
exitStandbyAnswer :: ExitStandbyAnswer
exitStandbyAnswer = ExitStandbyAnswer
    { _esa1Activities = mempty
    }

-- | A list describing the activities related to moving instances out of
-- Standby mode.
esa1Activities :: Lens' ExitStandbyAnswer [Activity]
esa1Activities = lens _esa1Activities (\s a -> s { _esa1Activities = a })

instance FromXML ExitStandbyAnswer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ExitStandbyAnswer"

instance AWSRequest ExitStandbyQuery where
    type Sv ExitStandbyQuery = AutoScaling
    type Rs ExitStandbyQuery = ExitStandbyAnswer

    request  = post "ExitStandby"
    response = xmlResponse $ \h x -> ExitStandbyAnswer
        <$> x %| "Activities"
