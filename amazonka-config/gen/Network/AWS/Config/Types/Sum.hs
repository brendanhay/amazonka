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
-- Stability   : auto-generated
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
           <> "'. Accepted values: Forward, Reverse"

instance ToText ChronologicalOrder where
    toText = \case
        Forward -> "Forward"
        Reverse -> "Reverse"

instance Hashable     ChronologicalOrder
instance ToByteString ChronologicalOrder
instance ToQuery      ChronologicalOrder
instance ToHeader     ChronologicalOrder

instance ToJSON ChronologicalOrder where
    toJSON = toJSONText

data ConfigurationItemStatus
    = Deleted
    | Discovered
    | Failed
    | OK
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ConfigurationItemStatus where
    parser = takeLowerText >>= \case
        "deleted" -> pure Deleted
        "discovered" -> pure Discovered
        "failed" -> pure Failed
        "ok" -> pure OK
        e -> fromTextError $ "Failure parsing ConfigurationItemStatus from value: '" <> e
           <> "'. Accepted values: Deleted, Discovered, Failed, Ok"

instance ToText ConfigurationItemStatus where
    toText = \case
        Deleted -> "Deleted"
        Discovered -> "Discovered"
        Failed -> "Failed"
        OK -> "Ok"

instance Hashable     ConfigurationItemStatus
instance ToByteString ConfigurationItemStatus
instance ToQuery      ConfigurationItemStatus
instance ToHeader     ConfigurationItemStatus

instance FromJSON ConfigurationItemStatus where
    parseJSON = parseJSONText "ConfigurationItemStatus"

data DeliveryStatus
    = Failure
    | NotApplicable
    | Success
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DeliveryStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure Failure
        "not_applicable" -> pure NotApplicable
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing DeliveryStatus from value: '" <> e
           <> "'. Accepted values: Failure, Not_Applicable, Success"

instance ToText DeliveryStatus where
    toText = \case
        Failure -> "Failure"
        NotApplicable -> "Not_Applicable"
        Success -> "Success"

instance Hashable     DeliveryStatus
instance ToByteString DeliveryStatus
instance ToQuery      DeliveryStatus
instance ToHeader     DeliveryStatus

instance FromJSON DeliveryStatus where
    parseJSON = parseJSONText "DeliveryStatus"

data RecorderStatus
    = RSFailure
    | RSPending
    | RSSuccess
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RecorderStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure RSFailure
        "pending" -> pure RSPending
        "success" -> pure RSSuccess
        e -> fromTextError $ "Failure parsing RecorderStatus from value: '" <> e
           <> "'. Accepted values: Failure, Pending, Success"

instance ToText RecorderStatus where
    toText = \case
        RSFailure -> "Failure"
        RSPending -> "Pending"
        RSSuccess -> "Success"

instance Hashable     RecorderStatus
instance ToByteString RecorderStatus
instance ToQuery      RecorderStatus
instance ToHeader     RecorderStatus

instance FromJSON RecorderStatus where
    parseJSON = parseJSONText "RecorderStatus"

data ResourceType
    = AWSCloudTrailTrail
    | AWSEC2CustomerGateway
    | AWSEC2EIP
    | AWSEC2Instance
    | AWSEC2InternetGateway
    | AWSEC2NetworkACL
    | AWSEC2NetworkInterface
    | AWSEC2RouteTable
    | AWSEC2SecurityGroup
    | AWSEC2Subnet
    | AWSEC2VPC
    | AWSEC2VPNConnection
    | AWSEC2VPNGateway
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
           <> "'. Accepted values: AWS::CloudTrail::Trail, AWS::EC2::CustomerGateway, AWS::EC2::EIP, AWS::EC2::Instance, AWS::EC2::InternetGateway, AWS::EC2::NetworkAcl, AWS::EC2::NetworkInterface, AWS::EC2::RouteTable, AWS::EC2::SecurityGroup, AWS::EC2::Subnet, AWS::EC2::VPC, AWS::EC2::VPNConnection, AWS::EC2::VPNGateway, AWS::EC2::Volume"

instance ToText ResourceType where
    toText = \case
        AWSCloudTrailTrail -> "AWS::CloudTrail::Trail"
        AWSEC2CustomerGateway -> "AWS::EC2::CustomerGateway"
        AWSEC2EIP -> "AWS::EC2::EIP"
        AWSEC2Instance -> "AWS::EC2::Instance"
        AWSEC2InternetGateway -> "AWS::EC2::InternetGateway"
        AWSEC2NetworkACL -> "AWS::EC2::NetworkAcl"
        AWSEC2NetworkInterface -> "AWS::EC2::NetworkInterface"
        AWSEC2RouteTable -> "AWS::EC2::RouteTable"
        AWSEC2SecurityGroup -> "AWS::EC2::SecurityGroup"
        AWSEC2Subnet -> "AWS::EC2::Subnet"
        AWSEC2VPC -> "AWS::EC2::VPC"
        AWSEC2VPNConnection -> "AWS::EC2::VPNConnection"
        AWSEC2VPNGateway -> "AWS::EC2::VPNGateway"
        AWSEC2Volume -> "AWS::EC2::Volume"

instance Hashable     ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance ToJSON ResourceType where
    toJSON = toJSONText

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"
