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

-- Module      : Network.AWS.EC2.TerminateInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Shuts down one or more instances. This operation is idempotent; if you
-- terminate an instance more than once, each call succeeds. Terminated
-- instances remain visible after termination (for approximately one hour). By
-- default, Amazon EC2 deletes all Amazon EBS volumes that were attached when
-- the instance launched. Volumes attached after instance launch continue
-- running. You can stop, start, and terminate EBS-backed instances. You can
-- only terminate instance store-backed instances. What happens to an instance
-- differs if you stop it or terminate it. For example, when you stop an
-- instance, the root device and any other devices attached to the instance
-- persist. When you terminate an instance, the root device and any other
-- devices attached during the instance launch are automatically deleted. For
-- more information about the differences between stopping and terminating
-- instances, see Instance Lifecycle in the Amazon Elastic Compute Cloud User
-- Guide. For more information about troubleshooting, see Troubleshooting
-- Terminating Your Instance in the Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.TerminateInstances
    (
    -- * Request
      TerminateInstances
    -- ** Request constructor
    , terminateInstances
    -- ** Request lenses
    , tiDryRun
    , tiInstanceIds

    -- * Response
    , TerminateInstancesResult
    -- ** Response constructor
    , terminateInstancesResult
    -- ** Response lenses
    , tirTerminatingInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data TerminateInstances = TerminateInstances
    { _tiDryRun      :: Maybe Bool
    , _tiInstanceIds :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'TerminateInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tiDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'tiInstanceIds' @::@ ['Text']
--
terminateInstances :: TerminateInstances
terminateInstances = TerminateInstances
    { _tiDryRun      = Nothing
    , _tiInstanceIds = mempty
    }

tiDryRun :: Lens' TerminateInstances (Maybe Bool)
tiDryRun = lens _tiDryRun (\s a -> s { _tiDryRun = a })

-- | One or more instance IDs.
tiInstanceIds :: Lens' TerminateInstances [Text]
tiInstanceIds = lens _tiInstanceIds (\s a -> s { _tiInstanceIds = a })

instance ToPath TerminateInstances where
    toPath = const "/"

instance ToQuery TerminateInstances

newtype TerminateInstancesResult = TerminateInstancesResult
    { _tirTerminatingInstances :: [InstanceStateChange]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'TerminateInstancesResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tirTerminatingInstances' @::@ ['InstanceStateChange']
--
terminateInstancesResult :: TerminateInstancesResult
terminateInstancesResult = TerminateInstancesResult
    { _tirTerminatingInstances = mempty
    }

-- | Information about one or more terminated instances.
tirTerminatingInstances :: Lens' TerminateInstancesResult [InstanceStateChange]
tirTerminatingInstances =
    lens _tirTerminatingInstances (\s a -> s { _tirTerminatingInstances = a })

instance AWSRequest TerminateInstances where
    type Sv TerminateInstances = EC2
    type Rs TerminateInstances = TerminateInstancesResult

    request  = post "TerminateInstances"
    response = const . xmlResponse $ \h x -> TerminateInstancesResult
newtype
