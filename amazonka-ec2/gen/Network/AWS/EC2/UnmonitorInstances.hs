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

-- Module      : Network.AWS.EC2.UnmonitorInstances
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

-- | Disables monitoring for a running instance. For more information about
-- monitoring instances, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring Your Instances and Volumes> in the /AmazonElastic Compute Cloud User Guide/.
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
    , _uiInstanceIds :: List "InstanceId" Text
    } deriving (Eq, Ord, Show)

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
uiInstanceIds = lens _uiInstanceIds (\s a -> s { _uiInstanceIds = a }) . _List

newtype UnmonitorInstancesResponse = UnmonitorInstancesResponse
    { _uirInstanceMonitorings :: List "item" InstanceMonitoring
    } deriving (Eq, Show, Monoid, Semigroup)

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
        . _List

instance ToPath UnmonitorInstances where
    toPath = const "/"

instance ToQuery UnmonitorInstances where
    toQuery UnmonitorInstances{..} = mconcat
        [ "dryRun"     =? _uiDryRun
        , toQuery     _uiInstanceIds
        ]

instance ToHeaders UnmonitorInstances

instance AWSRequest UnmonitorInstances where
    type Sv UnmonitorInstances = EC2
    type Rs UnmonitorInstances = UnmonitorInstancesResponse

    request  = post "UnmonitorInstances"
    response = xmlResponse

instance FromXML UnmonitorInstancesResponse where
    parseXML x = UnmonitorInstancesResponse
        <$> parseXML x
