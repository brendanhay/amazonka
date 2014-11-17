{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-StartInstances.html>
module Network.AWS.EC2.StartInstances
    (
    -- * Request
      StartInstances
    -- ** Request constructor
    , startInstances
    -- ** Request lenses
    , si1AdditionalInfo
    , si1DryRun
    , si1InstanceIds

    -- * Response
    , StartInstancesResponse
    -- ** Response constructor
    , startInstancesResponse
    -- ** Response lenses
    , sirStartingInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data StartInstances = StartInstances
    { _si1AdditionalInfo :: Maybe Text
    , _si1DryRun         :: Maybe Bool
    , _si1InstanceIds    :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'StartInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'si1AdditionalInfo' @::@ 'Maybe' 'Text'
--
-- * 'si1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'si1InstanceIds' @::@ ['Text']
--
startInstances :: StartInstances
startInstances = StartInstances
    { _si1InstanceIds    = mempty
    , _si1AdditionalInfo = Nothing
    , _si1DryRun         = Nothing
    }

-- | Reserved.
si1AdditionalInfo :: Lens' StartInstances (Maybe Text)
si1AdditionalInfo =
    lens _si1AdditionalInfo (\s a -> s { _si1AdditionalInfo = a })

si1DryRun :: Lens' StartInstances (Maybe Bool)
si1DryRun = lens _si1DryRun (\s a -> s { _si1DryRun = a })

-- | One or more instance IDs.
si1InstanceIds :: Lens' StartInstances [Text]
si1InstanceIds = lens _si1InstanceIds (\s a -> s { _si1InstanceIds = a })

newtype StartInstancesResponse = StartInstancesResponse
    { _sirStartingInstances :: [InstanceStateChange]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList StartInstancesResponse where
    type Item StartInstancesResponse = InstanceStateChange

    fromList = StartInstancesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _sirStartingInstances

-- | 'StartInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sirStartingInstances' @::@ ['InstanceStateChange']
--
startInstancesResponse :: StartInstancesResponse
startInstancesResponse = StartInstancesResponse
    { _sirStartingInstances = mempty
    }

-- | Information about one or more started instances.
sirStartingInstances :: Lens' StartInstancesResponse [InstanceStateChange]
sirStartingInstances =
    lens _sirStartingInstances (\s a -> s { _sirStartingInstances = a })

instance AWSRequest StartInstances where
    type Sv StartInstances = EC2
    type Rs StartInstances = StartInstancesResponse

    request  = post "StartInstances"
    response = xmlResponse

instance FromXML StartInstancesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StartInstancesResponse"

instance ToPath StartInstances where
    toPath = const "/"

instance ToHeaders StartInstances

instance ToQuery StartInstances where
    toQuery StartInstances{..} = mconcat
        [ "InstanceId"     =? _si1InstanceIds
        , "additionalInfo" =? _si1AdditionalInfo
        , "dryRun"         =? _si1DryRun
        ]

instance ToXML StartInstances where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "StartInstances"
