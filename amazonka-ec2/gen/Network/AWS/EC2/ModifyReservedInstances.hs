{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifyReservedInstances
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
module Network.AWS.EC2.ModifyReservedInstances
    (
    -- * Request
      ModifyReservedInstances
    -- ** Request constructor
    , modifyReservedInstances
    -- ** Request lenses
    , mriClientToken
    , mriReservedInstancesId
    , mriReservedInstancesConfigurationSetItemType

    -- * Response
    , ModifyReservedInstancesResponse
    -- ** Response constructor
    , modifyReservedInstancesResponse
    -- ** Response lenses
    , mrirReservedInstancesModificationId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ModifyReservedInstances = ModifyReservedInstances
    { _mriClientToken :: Maybe Text
    , _mriReservedInstancesId :: [Text]
    , _mriReservedInstancesConfigurationSetItemType :: [ReservedInstancesConfiguration]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyReservedInstances' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClientToken ::@ @Maybe Text@
--
-- * @ReservedInstancesId ::@ @[Text]@
--
-- * @ReservedInstancesConfigurationSetItemType ::@ @[ReservedInstancesConfiguration]@
--
modifyReservedInstances :: [Text] -- ^ 'mriReservedInstancesId'
                        -> [ReservedInstancesConfiguration] -- ^ 'mriReservedInstancesConfigurationSetItemType'
                        -> ModifyReservedInstances
modifyReservedInstances p2 p3 = ModifyReservedInstances
    { _mriClientToken = Nothing
    , _mriReservedInstancesId = p2
    , _mriReservedInstancesConfigurationSetItemType = p3
    }

-- | A unique, case-sensitive token you provide to ensure idempotency of your
-- modification request.
mriClientToken :: Lens' ModifyReservedInstances (Maybe Text)
mriClientToken = lens _mriClientToken (\s a -> s { _mriClientToken = a })

-- | The IDs of the Reserved Instances to modify.
mriReservedInstancesId :: Lens' ModifyReservedInstances [Text]
mriReservedInstancesId =
    lens _mriReservedInstancesId (\s a -> s { _mriReservedInstancesId = a })

-- | The configuration settings for the Reserved Instances to modify.
mriReservedInstancesConfigurationSetItemType :: Lens' ModifyReservedInstances [ReservedInstancesConfiguration]
mriReservedInstancesConfigurationSetItemType =
    lens _mriReservedInstancesConfigurationSetItemType
         (\s a -> s { _mriReservedInstancesConfigurationSetItemType = a })

instance ToQuery ModifyReservedInstances where
    toQuery = genericQuery def

newtype ModifyReservedInstancesResponse = ModifyReservedInstancesResponse
    { _mrirReservedInstancesModificationId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyReservedInstancesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesModificationId ::@ @Maybe Text@
--
modifyReservedInstancesResponse :: ModifyReservedInstancesResponse
modifyReservedInstancesResponse = ModifyReservedInstancesResponse
    { _mrirReservedInstancesModificationId = Nothing
    }

-- | The ID for the modification.
mrirReservedInstancesModificationId :: Lens' ModifyReservedInstancesResponse (Maybe Text)
mrirReservedInstancesModificationId =
    lens _mrirReservedInstancesModificationId
         (\s a -> s { _mrirReservedInstancesModificationId = a })

instance FromXML ModifyReservedInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyReservedInstances where
    type Sv ModifyReservedInstances = EC2
    type Rs ModifyReservedInstances = ModifyReservedInstancesResponse

    request = post "ModifyReservedInstances"
    response _ = xmlResponse
