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

-- Module      : Network.AWS.EC2.AssociateAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates an Elastic IP address with an instance or a network interface.
-- An Elastic IP address is for use in either the EC2-Classic platform or in a
-- VPC. For more information, see Elastic IP Addresses in the Amazon Elastic
-- Compute Cloud User Guide. [EC2-Classic, VPC in an EC2-VPC-only account] If
-- the Elastic IP address is already associated with a different instance, it
-- is disassociated from that instance and associated with the specified
-- instance. [VPC in an EC2-Classic account] If you don't specify a private IP
-- address, the Elastic IP address is associated with the primary IP address.
-- If the Elastic IP address is already associated with a different instance
-- or a network interface, you get an error unless you allow reassociation.
-- This is an idempotent operation. If you perform the operation more than
-- once, Amazon EC2 doesn't return an error.
module Network.AWS.EC2.AssociateAddress
    (
    -- * Request
      AssociateAddress
    -- ** Request constructor
    , associateAddress
    -- ** Request lenses
    , aa1AllocationId
    , aa1AllowReassociation
    , aa1DryRun
    , aa1InstanceId
    , aa1NetworkInterfaceId
    , aa1PrivateIpAddress
    , aa1PublicIp

    -- * Response
    , AssociateAddressResult
    -- ** Response constructor
    , associateAddressResult
    -- ** Response lenses
    , aarAssociationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data AssociateAddress = AssociateAddress
    { _aa1AllocationId       :: Maybe Text
    , _aa1AllowReassociation :: Maybe Bool
    , _aa1DryRun             :: Maybe Bool
    , _aa1InstanceId         :: Maybe Text
    , _aa1NetworkInterfaceId :: Maybe Text
    , _aa1PrivateIpAddress   :: Maybe Text
    , _aa1PublicIp           :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'AssociateAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aa1AllocationId' @::@ 'Maybe' 'Text'
--
-- * 'aa1AllowReassociation' @::@ 'Maybe' 'Bool'
--
-- * 'aa1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'aa1InstanceId' @::@ 'Maybe' 'Text'
--
-- * 'aa1NetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'aa1PrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'aa1PublicIp' @::@ 'Maybe' 'Text'
--
associateAddress :: AssociateAddress
associateAddress = AssociateAddress
    { _aa1DryRun             = Nothing
    , _aa1InstanceId         = Nothing
    , _aa1PublicIp           = Nothing
    , _aa1AllocationId       = Nothing
    , _aa1NetworkInterfaceId = Nothing
    , _aa1PrivateIpAddress   = Nothing
    , _aa1AllowReassociation = Nothing
    }

-- | [EC2-VPC] The allocation ID. This is required for EC2-VPC.
aa1AllocationId :: Lens' AssociateAddress (Maybe Text)
aa1AllocationId = lens _aa1AllocationId (\s a -> s { _aa1AllocationId = a })

-- | [EC2-VPC] Allows an Elastic IP address that is already associated with an
-- instance or network interface to be re-associated with the specified
-- instance or network interface. Otherwise, the operation fails. Default:
-- false.
aa1AllowReassociation :: Lens' AssociateAddress (Maybe Bool)
aa1AllowReassociation =
    lens _aa1AllowReassociation (\s a -> s { _aa1AllowReassociation = a })

aa1DryRun :: Lens' AssociateAddress (Maybe Bool)
aa1DryRun = lens _aa1DryRun (\s a -> s { _aa1DryRun = a })

-- | The ID of the instance. This is required for EC2-Classic. For EC2-VPC,
-- you can specify either the instance ID or the network interface ID, but
-- not both. The operation fails if you specify an instance ID unless
-- exactly one network interface is attached.
aa1InstanceId :: Lens' AssociateAddress (Maybe Text)
aa1InstanceId = lens _aa1InstanceId (\s a -> s { _aa1InstanceId = a })

-- | [EC2-VPC] The ID of the network interface. If the instance has more than
-- one network interface, you must specify a network interface ID.
aa1NetworkInterfaceId :: Lens' AssociateAddress (Maybe Text)
aa1NetworkInterfaceId =
    lens _aa1NetworkInterfaceId (\s a -> s { _aa1NetworkInterfaceId = a })

-- | [EC2-VPC] The primary or secondary private IP address to associate with
-- the Elastic IP address. If no private IP address is specified, the
-- Elastic IP address is associated with the primary private IP address.
aa1PrivateIpAddress :: Lens' AssociateAddress (Maybe Text)
aa1PrivateIpAddress =
    lens _aa1PrivateIpAddress (\s a -> s { _aa1PrivateIpAddress = a })

-- | The Elastic IP address. This is required for EC2-Classic.
aa1PublicIp :: Lens' AssociateAddress (Maybe Text)
aa1PublicIp = lens _aa1PublicIp (\s a -> s { _aa1PublicIp = a })
instance ToQuery AssociateAddress

instance ToPath AssociateAddress where
    toPath = const "/"

newtype AssociateAddressResult = AssociateAddressResult
    { _aarAssociationId :: Maybe Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'AssociateAddressResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aarAssociationId' @::@ 'Maybe' 'Text'
--
associateAddressResult :: AssociateAddressResult
associateAddressResult = AssociateAddressResult
    { _aarAssociationId = Nothing
    }

-- | [EC2-VPC] The ID that represents the association of the Elastic IP
-- address with an instance.
aarAssociationId :: Lens' AssociateAddressResult (Maybe Text)
aarAssociationId = lens _aarAssociationId (\s a -> s { _aarAssociationId = a })

instance FromXML AssociateAddressResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AssociateAddressResult"

instance AWSRequest AssociateAddress where
    type Sv AssociateAddress = EC2
    type Rs AssociateAddress = AssociateAddressResult

    request  = post "AssociateAddress"
    response = xmlResponse $ \h x -> AssociateAddressResult
        <$> x %| "associationId"
