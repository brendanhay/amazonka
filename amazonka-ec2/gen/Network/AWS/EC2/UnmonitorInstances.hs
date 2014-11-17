{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.UnmonitorInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables monitoring for a running instance. For more information about
-- monitoring instances, see Monitoring Your Instances and Volumes in the
-- Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-UnmonitorInstances.html>
module Network.AWS.EC2.UnmonitorInstances
    (
    -- * Request
      UnmonitorInstances
    -- ** Request constructor
    , unmonitorInstances
    -- ** Request lenses
    , uiDryRun
    , uiInstanceIds

    -- * Response
    , UnmonitorInstancesResponse
    -- ** Response constructor
    , unmonitorInstancesResponse
    -- ** Response lenses
    , uirInstanceMonitorings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data UnmonitorInstances = UnmonitorInstances
    { _uiDryRun      :: Maybe Bool
    , _uiInstanceIds :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'UnmonitorInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'uiInstanceIds' @::@ ['Text']
--
unmonitorInstances :: UnmonitorInstances
unmonitorInstances = UnmonitorInstances
    { _uiDryRun      = Nothing
    , _uiInstanceIds = mempty
    }

uiDryRun :: Lens' UnmonitorInstances (Maybe Bool)
uiDryRun = lens _uiDryRun (\s a -> s { _uiDryRun = a })

-- | One or more instance IDs.
uiInstanceIds :: Lens' UnmonitorInstances [Text]
uiInstanceIds = lens _uiInstanceIds (\s a -> s { _uiInstanceIds = a })

newtype UnmonitorInstancesResponse = UnmonitorInstancesResponse
    { _uirInstanceMonitorings :: [InstanceMonitoring]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList UnmonitorInstancesResponse where
    type Item UnmonitorInstancesResponse = InstanceMonitoring

    fromList = UnmonitorInstancesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _uirInstanceMonitorings

-- | 'UnmonitorInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uirInstanceMonitorings' @::@ ['InstanceMonitoring']
--
unmonitorInstancesResponse :: UnmonitorInstancesResponse
unmonitorInstancesResponse = UnmonitorInstancesResponse
    { _uirInstanceMonitorings = mempty
    }

-- | Monitoring information for one or more instances.
uirInstanceMonitorings :: Lens' UnmonitorInstancesResponse [InstanceMonitoring]
uirInstanceMonitorings =
    lens _uirInstanceMonitorings (\s a -> s { _uirInstanceMonitorings = a })

instance AWSRequest UnmonitorInstances where
    type Sv UnmonitorInstances = EC2
    type Rs UnmonitorInstances = UnmonitorInstancesResponse

    request  = post "UnmonitorInstances"
    response = xmlResponse

instance FromXML UnmonitorInstancesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UnmonitorInstancesResponse"

instance ToPath UnmonitorInstances where
    toPath = const "/"

instance ToHeaders UnmonitorInstances

instance ToQuery UnmonitorInstances where
    toQuery UnmonitorInstances{..} = mconcat
        [ "dryRun"     =? _uiDryRun
        , "InstanceId" =? _uiInstanceIds
        ]

instance ToXML UnmonitorInstances where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UnmonitorInstances"
