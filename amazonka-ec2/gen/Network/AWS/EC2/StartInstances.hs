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

-- Module      : Network.AWS.EC2.StartInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts an Amazon EBS-backed AMI that you've previously stopped. Instances
-- that use Amazon EBS volumes as their root devices can be quickly stopped
-- and started. When an instance is stopped, the compute resources are
-- released and you are not billed for hourly instance usage. However, your
-- root partition Amazon EBS volume remains, continues to persist your data,
-- and you are charged for Amazon EBS volume usage. You can restart your
-- instance at any time. Each time you transition an instance from stopped to
-- started, Amazon EC2 charges a full instance hour, even if transitions
-- happen multiple times within a single hour. Before stopping an instance,
-- make sure it is in a state from which it can be restarted. Stopping an
-- instance does not preserve data stored in RAM. Performing this operation on
-- an instance that uses an instance store as its root device returns an
-- error. For more information, see Stopping Instances in the Amazon Elastic
-- Compute Cloud User Guide.
module Network.AWS.EC2.StartInstances
    (
    -- * Request
      StartInstances
    -- ** Request constructor
    , startInstances
    -- ** Request lenses
    , siAdditionalInfo
    , siDryRun
    , siInstanceIds

    -- * Response
    , StartInstancesResult
    -- ** Response constructor
    , startInstancesResponse
    -- ** Response lenses
    , sirStartingInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data StartInstances = StartInstances
    { _siAdditionalInfo :: Maybe Text
    , _siDryRun         :: Maybe Bool
    , _siInstanceIds    :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'StartInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'siAdditionalInfo' @::@ 'Maybe' 'Text'
--
-- * 'siDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'siInstanceIds' @::@ ['Text']
--
startInstances :: StartInstances
startInstances = StartInstances
    { _siInstanceIds    = mempty
    , _siAdditionalInfo = Nothing
    , _siDryRun         = Nothing
    }

-- | Reserved.
siAdditionalInfo :: Lens' StartInstances (Maybe Text)
siAdditionalInfo = lens _siAdditionalInfo (\s a -> s { _siAdditionalInfo = a })

siDryRun :: Lens' StartInstances (Maybe Bool)
siDryRun = lens _siDryRun (\s a -> s { _siDryRun = a })

-- | One or more instance IDs.
siInstanceIds :: Lens' StartInstances [Text]
siInstanceIds = lens _siInstanceIds (\s a -> s { _siInstanceIds = a })

instance ToPath StartInstances where
    toPath = const "/"

instance ToQuery StartInstances

newtype StartInstancesResult = StartInstancesResult
    { _sirStartingInstances :: [InstanceStateChange]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'StartInstancesResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sirStartingInstances' @::@ ['InstanceStateChange']
--
startInstancesResponse :: StartInstancesResult
startInstancesResponse = StartInstancesResult
    { _sirStartingInstances = mempty
    }

-- | Information about one or more started instances.
sirStartingInstances :: Lens' StartInstancesResult [InstanceStateChange]
sirStartingInstances =
    lens _sirStartingInstances (\s a -> s { _sirStartingInstances = a })

instance AWSRequest StartInstances where
    type Sv StartInstances = EC2
    type Rs StartInstances = StartInstancesResult

    request  = post "StartInstances"
    response = xmlResponse $ \h x -> StartInstancesResult
        <$> x %| "instancesSet"
