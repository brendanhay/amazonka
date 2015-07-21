{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.Sum where

import           Network.AWS.Prelude

data ChronologicalOrder
    = Forward
    | Reverse
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ChronologicalOrder where
    parser = takeLowerText >>= \case
        "forward" -> pure Forward
        "reverse" -> pure Reverse
        e -> fromTextError $ "Failure parsing ChronologicalOrder from value: '" <> e
           <> "'. Accepted values: forward, reverse"

instance ToText ChronologicalOrder where
    toText = \case
        Forward -> "forward"
        Reverse -> "reverse"

instance Hashable ChronologicalOrder
instance ToQuery ChronologicalOrder
instance ToHeader ChronologicalOrder

instance ToJSON ChronologicalOrder where
    toJSON = toJSONText

data ConfigurationItemStatus
    = OK
    | Discovered
    | Deleted
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ConfigurationItemStatus where
    parser = takeLowerText >>= \case
        "deleted" -> pure Deleted
        "discovered" -> pure Discovered
        "failed" -> pure Failed
        "ok" -> pure OK
        e -> fromTextError $ "Failure parsing ConfigurationItemStatus from value: '" <> e
           <> "'. Accepted values: deleted, discovered, failed, ok"

instance ToText ConfigurationItemStatus where
    toText = \case
        Deleted -> "deleted"
        Discovered -> "discovered"
        Failed -> "failed"
        OK -> "ok"

instance Hashable ConfigurationItemStatus
instance ToQuery ConfigurationItemStatus
instance ToHeader ConfigurationItemStatus

instance FromJSON ConfigurationItemStatus where
    parseJSON = parseJSONText "ConfigurationItemStatus"

data DeliveryStatus
    = Success
    | NotApplicable
    | Failure
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DeliveryStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure Failure
        "not_applicable" -> pure NotApplicable
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing DeliveryStatus from value: '" <> e
           <> "'. Accepted values: failure, not_applicable, success"

instance ToText DeliveryStatus where
    toText = \case
        Failure -> "failure"
        NotApplicable -> "not_applicable"
        Success -> "success"

instance Hashable DeliveryStatus
instance ToQuery DeliveryStatus
instance ToHeader DeliveryStatus

instance FromJSON DeliveryStatus where
    parseJSON = parseJSONText "DeliveryStatus"

data RecorderStatus
    = RSPending
    | RSFailure
    | RSSuccess
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RecorderStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure RSFailure
        "pending" -> pure RSPending
        "success" -> pure RSSuccess
        e -> fromTextError $ "Failure parsing RecorderStatus from value: '" <> e
           <> "'. Accepted values: failure, pending, success"

instance ToText RecorderStatus where
    toText = \case
        RSFailure -> "failure"
        RSPending -> "pending"
        RSSuccess -> "success"

instance Hashable RecorderStatus
instance ToQuery RecorderStatus
instance ToHeader RecorderStatus

instance FromJSON RecorderStatus where
    parseJSON = parseJSONText "RecorderStatus"

data ResourceType
    = AWSCloudTrailTrail
    | AWSEC2VPNConnection
    | AWSEC2SecurityGroup
    | AWSEC2Instance
    | AWSEC2NetworkACL
    | AWSEC2VPNGateway
    | AWSEC2VPC
    | AWSEC2NetworkInterface
    | AWSEC2InternetGateway
    | AWSEC2Subnet
    | AWSEC2EIP
    | AWSEC2CustomerGateway
    | AWSEC2RouteTable
    | AWSEC2Volume
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "aws::cloudtrail::trail" -> pure AWSCloudTrailTrail
        "aws::ec2::customergateway" -> pure AWSEC2CustomerGateway
        "aws::ec2::eip" -> pure AWSEC2EIP
        "aws::ec2::instance" -> pure AWSEC2Instance
        "aws::ec2::internetgateway" -> pure AWSEC2InternetGateway
        "aws::ec2::networkacl" -> pure AWSEC2NetworkACL
        "aws::ec2::networkinterface" -> pure AWSEC2NetworkInterface
        "aws::ec2::routetable" -> pure AWSEC2RouteTable
        "aws::ec2::securitygroup" -> pure AWSEC2SecurityGroup
        "aws::ec2::subnet" -> pure AWSEC2Subnet
        "aws::ec2::vpc" -> pure AWSEC2VPC
        "aws::ec2::vpnconnection" -> pure AWSEC2VPNConnection
        "aws::ec2::vpngateway" -> pure AWSEC2VPNGateway
        "aws::ec2::volume" -> pure AWSEC2Volume
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: aws::cloudtrail::trail, aws::ec2::customergateway, aws::ec2::eip, aws::ec2::instance, aws::ec2::internetgateway, aws::ec2::networkacl, aws::ec2::networkinterface, aws::ec2::routetable, aws::ec2::securitygroup, aws::ec2::subnet, aws::ec2::vpc, aws::ec2::vpnconnection, aws::ec2::vpngateway, aws::ec2::volume"

instance ToText ResourceType where
    toText = \case
        AWSCloudTrailTrail -> "aws::cloudtrail::trail"
        AWSEC2CustomerGateway -> "aws::ec2::customergateway"
        AWSEC2EIP -> "aws::ec2::eip"
        AWSEC2Instance -> "aws::ec2::instance"
        AWSEC2InternetGateway -> "aws::ec2::internetgateway"
        AWSEC2NetworkACL -> "aws::ec2::networkacl"
        AWSEC2NetworkInterface -> "aws::ec2::networkinterface"
        AWSEC2RouteTable -> "aws::ec2::routetable"
        AWSEC2SecurityGroup -> "aws::ec2::securitygroup"
        AWSEC2Subnet -> "aws::ec2::subnet"
        AWSEC2VPC -> "aws::ec2::vpc"
        AWSEC2VPNConnection -> "aws::ec2::vpnconnection"
        AWSEC2VPNGateway -> "aws::ec2::vpngateway"
        AWSEC2Volume -> "aws::ec2::volume"

instance Hashable ResourceType
instance ToQuery ResourceType
instance ToHeader ResourceType

instance ToJSON ResourceType where
    toJSON = toJSONText

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"
