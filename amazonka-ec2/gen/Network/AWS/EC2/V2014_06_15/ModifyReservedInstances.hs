{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ModifyReservedInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the Availability Zone, instance count, instance type, or network
-- platform (EC2-Classic or EC2-VPC) of your Reserved Instances. The Reserved
-- Instances to be modified must be identical, except for Availability Zone,
-- network platform, and instance type. Example
-- https://ec2.amazonaws.com/?Action=ModifyReservedInstances
-- &amp;ClientToken=myClientToken
-- &amp;ReservedInstancesConfigurationSetItemType.0.AvailabilityZone=us-east-1a
-- &amp;ReservedInstancesConfigurationSetItemType.0.InstanceCount=1
-- &amp;ReservedInstancesConfigurationSetItemType.0.Platform=EC2-VPC
-- &amp;ReservedInstancesConfigurationSetItemType.0.InstanceType=m1.small
-- &amp;ReservedInstancesId.0=d16f7a91-4d0f-4f19-9d7f-a74d26b1ccfa
-- &amp;AUTHPARAMS bef729b6-0731-4489-8881-2258746ae163
-- rimod-3aae219d-3d63-47a9-a7e9-e764example.
module Network.AWS.EC2.V2014_06_15.ModifyReservedInstances
    (
    -- * Request
      ModifyReservedInstances
    -- ** Request constructor
    , mkModifyReservedInstancesRequest
    -- ** Request lenses
    , mrirClientToken
    , mrirReservedInstancesIds
    , mrirTargetConfigurations

    -- * Response
    , ModifyReservedInstancesResponse
    -- ** Response lenses
    , mrisReservedInstancesModificationId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyReservedInstances' request.
mkModifyReservedInstancesRequest :: [Text] -- ^ 'mrirReservedInstancesIds'
                                 -> [ReservedInstancesConfiguration] -- ^ 'mrirTargetConfigurations'
                                 -> ModifyReservedInstances
mkModifyReservedInstancesRequest p1 p2 = ModifyReservedInstances
    { _mrirClientToken = Nothing
    , _mrirReservedInstancesIds = p2
    , _mrirTargetConfigurations = p3
    }
{-# INLINE mkModifyReservedInstancesRequest #-}

data ModifyReservedInstances = ModifyReservedInstances
    { _mrirClientToken :: Maybe Text
      -- ^ A unique, case-sensitive token you provide to ensure idempotency
      -- of your modification request.
    , _mrirReservedInstancesIds :: [Text]
      -- ^ The IDs of the Reserved Instances to modify.
    , _mrirTargetConfigurations :: [ReservedInstancesConfiguration]
      -- ^ The configuration settings for the Reserved Instances to modify.
    } deriving (Show, Generic)

-- | A unique, case-sensitive token you provide to ensure idempotency of your
-- modification request.
mrirClientToken :: Lens' ModifyReservedInstances (Maybe Text)
mrirClientToken = lens _mrirClientToken (\s a -> s { _mrirClientToken = a })
{-# INLINE mrirClientToken #-}

-- | The IDs of the Reserved Instances to modify.
mrirReservedInstancesIds :: Lens' ModifyReservedInstances ([Text])
mrirReservedInstancesIds = lens _mrirReservedInstancesIds (\s a -> s { _mrirReservedInstancesIds = a })
{-# INLINE mrirReservedInstancesIds #-}

-- | The configuration settings for the Reserved Instances to modify.
mrirTargetConfigurations :: Lens' ModifyReservedInstances ([ReservedInstancesConfiguration])
mrirTargetConfigurations = lens _mrirTargetConfigurations (\s a -> s { _mrirTargetConfigurations = a })
{-# INLINE mrirTargetConfigurations #-}

instance ToQuery ModifyReservedInstances where
    toQuery = genericQuery def

newtype ModifyReservedInstancesResponse = ModifyReservedInstancesResponse
    { _mrisReservedInstancesModificationId :: Maybe Text
      -- ^ The ID for the modification.
    } deriving (Show, Generic)

-- | The ID for the modification.
mrisReservedInstancesModificationId :: Lens' ModifyReservedInstancesResponse (Maybe Text)
mrisReservedInstancesModificationId = lens _mrisReservedInstancesModificationId (\s a -> s { _mrisReservedInstancesModificationId = a })
{-# INLINE mrisReservedInstancesModificationId #-}

instance FromXML ModifyReservedInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyReservedInstances where
    type Sv ModifyReservedInstances = EC2
    type Rs ModifyReservedInstances = ModifyReservedInstancesResponse

    request = post "ModifyReservedInstances"
    response _ = xmlResponse
