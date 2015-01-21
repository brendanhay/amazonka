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

-- Module      : Network.AWS.CloudHSM.DescribeHsm
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

-- | Retrieves information about an HSM. You can identify the HSM by its ARN or
-- its serial number.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DescribeHsm.html>
module Network.AWS.CloudHSM.DescribeHsm
    (
    -- * Request
      DescribeHsm
    -- ** Request constructor
    , describeHsm
    -- ** Request lenses
    , dh1HsmArn
    , dh1HsmSerialNumber

    -- * Response
    , DescribeHsmResponse
    -- ** Response constructor
    , describeHsmResponse
    -- ** Response lenses
    , dhr2AvailabilityZone
    , dhr2EniId
    , dhr2EniIp
    , dhr2HsmArn
    , dhr2HsmType
    , dhr2IamRoleArn
    , dhr2Partitions
    , dhr2SerialNumber
    , dhr2ServerCertLastUpdated
    , dhr2ServerCertUri
    , dhr2SoftwareVersion
    , dhr2SshKeyLastUpdated
    , dhr2SshPublicKey
    , dhr2Status
    , dhr2StatusDetails
    , dhr2SubnetId
    , dhr2SubscriptionEndDate
    , dhr2SubscriptionStartDate
    , dhr2SubscriptionType
    , dhr2VendorName
    , dhr2VpcId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

data DescribeHsm = DescribeHsm
    { _dh1HsmArn          :: Maybe Text
    , _dh1HsmSerialNumber :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeHsm' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dh1HsmArn' @::@ 'Maybe' 'Text'
--
-- * 'dh1HsmSerialNumber' @::@ 'Maybe' 'Text'
--
describeHsm :: DescribeHsm
describeHsm = DescribeHsm
    { _dh1HsmArn          = Nothing
    , _dh1HsmSerialNumber = Nothing
    }

-- | The ARN of the HSM. Either the /HsmArn/ or the /SerialNumber/ parameter must be
-- specified.
dh1HsmArn :: Lens' DescribeHsm (Maybe Text)
dh1HsmArn = lens _dh1HsmArn (\s a -> s { _dh1HsmArn = a })

-- | The serial number of the HSM. Either the /HsmArn/ or the /HsmSerialNumber/
-- parameter must be specified.
dh1HsmSerialNumber :: Lens' DescribeHsm (Maybe Text)
dh1HsmSerialNumber =
    lens _dh1HsmSerialNumber (\s a -> s { _dh1HsmSerialNumber = a })

data DescribeHsmResponse = DescribeHsmResponse
    { _dhr2AvailabilityZone      :: Maybe Text
    , _dhr2EniId                 :: Maybe Text
    , _dhr2EniIp                 :: Maybe Text
    , _dhr2HsmArn                :: Maybe Text
    , _dhr2HsmType               :: Maybe Text
    , _dhr2IamRoleArn            :: Maybe Text
    , _dhr2Partitions            :: List "Partitions" Text
    , _dhr2SerialNumber          :: Maybe Text
    , _dhr2ServerCertLastUpdated :: Maybe Text
    , _dhr2ServerCertUri         :: Maybe Text
    , _dhr2SoftwareVersion       :: Maybe Text
    , _dhr2SshKeyLastUpdated     :: Maybe Text
    , _dhr2SshPublicKey          :: Maybe Text
    , _dhr2Status                :: Maybe HsmStatus
    , _dhr2StatusDetails         :: Maybe Text
    , _dhr2SubnetId              :: Maybe Text
    , _dhr2SubscriptionEndDate   :: Maybe Text
    , _dhr2SubscriptionStartDate :: Maybe Text
    , _dhr2SubscriptionType      :: Maybe SubscriptionType
    , _dhr2VendorName            :: Maybe Text
    , _dhr2VpcId                 :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeHsmResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhr2AvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'dhr2EniId' @::@ 'Maybe' 'Text'
--
-- * 'dhr2EniIp' @::@ 'Maybe' 'Text'
--
-- * 'dhr2HsmArn' @::@ 'Maybe' 'Text'
--
-- * 'dhr2HsmType' @::@ 'Maybe' 'Text'
--
-- * 'dhr2IamRoleArn' @::@ 'Maybe' 'Text'
--
-- * 'dhr2Partitions' @::@ ['Text']
--
-- * 'dhr2SerialNumber' @::@ 'Maybe' 'Text'
--
-- * 'dhr2ServerCertLastUpdated' @::@ 'Maybe' 'Text'
--
-- * 'dhr2ServerCertUri' @::@ 'Maybe' 'Text'
--
-- * 'dhr2SoftwareVersion' @::@ 'Maybe' 'Text'
--
-- * 'dhr2SshKeyLastUpdated' @::@ 'Maybe' 'Text'
--
-- * 'dhr2SshPublicKey' @::@ 'Maybe' 'Text'
--
-- * 'dhr2Status' @::@ 'Maybe' 'HsmStatus'
--
-- * 'dhr2StatusDetails' @::@ 'Maybe' 'Text'
--
-- * 'dhr2SubnetId' @::@ 'Maybe' 'Text'
--
-- * 'dhr2SubscriptionEndDate' @::@ 'Maybe' 'Text'
--
-- * 'dhr2SubscriptionStartDate' @::@ 'Maybe' 'Text'
--
-- * 'dhr2SubscriptionType' @::@ 'Maybe' 'SubscriptionType'
--
-- * 'dhr2VendorName' @::@ 'Maybe' 'Text'
--
-- * 'dhr2VpcId' @::@ 'Maybe' 'Text'
--
describeHsmResponse :: DescribeHsmResponse
describeHsmResponse = DescribeHsmResponse
    { _dhr2HsmArn                = Nothing
    , _dhr2Status                = Nothing
    , _dhr2StatusDetails         = Nothing
    , _dhr2AvailabilityZone      = Nothing
    , _dhr2EniId                 = Nothing
    , _dhr2EniIp                 = Nothing
    , _dhr2SubscriptionType      = Nothing
    , _dhr2SubscriptionStartDate = Nothing
    , _dhr2SubscriptionEndDate   = Nothing
    , _dhr2VpcId                 = Nothing
    , _dhr2SubnetId              = Nothing
    , _dhr2IamRoleArn            = Nothing
    , _dhr2SerialNumber          = Nothing
    , _dhr2VendorName            = Nothing
    , _dhr2HsmType               = Nothing
    , _dhr2SoftwareVersion       = Nothing
    , _dhr2SshPublicKey          = Nothing
    , _dhr2SshKeyLastUpdated     = Nothing
    , _dhr2ServerCertUri         = Nothing
    , _dhr2ServerCertLastUpdated = Nothing
    , _dhr2Partitions            = mempty
    }

-- | The Availability Zone that the HSM is in.
dhr2AvailabilityZone :: Lens' DescribeHsmResponse (Maybe Text)
dhr2AvailabilityZone =
    lens _dhr2AvailabilityZone (\s a -> s { _dhr2AvailabilityZone = a })

-- | The identifier of the elastic network interface (ENI) attached to the HSM.
dhr2EniId :: Lens' DescribeHsmResponse (Maybe Text)
dhr2EniId = lens _dhr2EniId (\s a -> s { _dhr2EniId = a })

-- | The IP address assigned to the HSM's ENI.
dhr2EniIp :: Lens' DescribeHsmResponse (Maybe Text)
dhr2EniIp = lens _dhr2EniIp (\s a -> s { _dhr2EniIp = a })

-- | The ARN of the HSM.
dhr2HsmArn :: Lens' DescribeHsmResponse (Maybe Text)
dhr2HsmArn = lens _dhr2HsmArn (\s a -> s { _dhr2HsmArn = a })

-- | The HSM model type.
dhr2HsmType :: Lens' DescribeHsmResponse (Maybe Text)
dhr2HsmType = lens _dhr2HsmType (\s a -> s { _dhr2HsmType = a })

-- | The ARN of the IAM role assigned to the HSM.
dhr2IamRoleArn :: Lens' DescribeHsmResponse (Maybe Text)
dhr2IamRoleArn = lens _dhr2IamRoleArn (\s a -> s { _dhr2IamRoleArn = a })

-- | The list of partitions on the HSM.
dhr2Partitions :: Lens' DescribeHsmResponse [Text]
dhr2Partitions = lens _dhr2Partitions (\s a -> s { _dhr2Partitions = a }) . _List

-- | The serial number of the HSM.
dhr2SerialNumber :: Lens' DescribeHsmResponse (Maybe Text)
dhr2SerialNumber = lens _dhr2SerialNumber (\s a -> s { _dhr2SerialNumber = a })

-- | The date and time the server certificate was last updated.
dhr2ServerCertLastUpdated :: Lens' DescribeHsmResponse (Maybe Text)
dhr2ServerCertLastUpdated =
    lens _dhr2ServerCertLastUpdated
        (\s a -> s { _dhr2ServerCertLastUpdated = a })

-- | The URI of the certificate server.
dhr2ServerCertUri :: Lens' DescribeHsmResponse (Maybe Text)
dhr2ServerCertUri =
    lens _dhr2ServerCertUri (\s a -> s { _dhr2ServerCertUri = a })

-- | The HSM software version.
dhr2SoftwareVersion :: Lens' DescribeHsmResponse (Maybe Text)
dhr2SoftwareVersion =
    lens _dhr2SoftwareVersion (\s a -> s { _dhr2SoftwareVersion = a })

-- | The date and time the SSH key was last updated.
dhr2SshKeyLastUpdated :: Lens' DescribeHsmResponse (Maybe Text)
dhr2SshKeyLastUpdated =
    lens _dhr2SshKeyLastUpdated (\s a -> s { _dhr2SshKeyLastUpdated = a })

-- | The public SSH key.
dhr2SshPublicKey :: Lens' DescribeHsmResponse (Maybe Text)
dhr2SshPublicKey = lens _dhr2SshPublicKey (\s a -> s { _dhr2SshPublicKey = a })

-- | The status of the HSM.
dhr2Status :: Lens' DescribeHsmResponse (Maybe HsmStatus)
dhr2Status = lens _dhr2Status (\s a -> s { _dhr2Status = a })

-- | Contains additional information about the status of the HSM.
dhr2StatusDetails :: Lens' DescribeHsmResponse (Maybe Text)
dhr2StatusDetails =
    lens _dhr2StatusDetails (\s a -> s { _dhr2StatusDetails = a })

-- | The identifier of the subnet the HSM is in.
dhr2SubnetId :: Lens' DescribeHsmResponse (Maybe Text)
dhr2SubnetId = lens _dhr2SubnetId (\s a -> s { _dhr2SubnetId = a })

-- | The subscription end date.
dhr2SubscriptionEndDate :: Lens' DescribeHsmResponse (Maybe Text)
dhr2SubscriptionEndDate =
    lens _dhr2SubscriptionEndDate (\s a -> s { _dhr2SubscriptionEndDate = a })

-- | The subscription start date.
dhr2SubscriptionStartDate :: Lens' DescribeHsmResponse (Maybe Text)
dhr2SubscriptionStartDate =
    lens _dhr2SubscriptionStartDate
        (\s a -> s { _dhr2SubscriptionStartDate = a })

-- | The subscription type.
dhr2SubscriptionType :: Lens' DescribeHsmResponse (Maybe SubscriptionType)
dhr2SubscriptionType =
    lens _dhr2SubscriptionType (\s a -> s { _dhr2SubscriptionType = a })

-- | The name of the HSM vendor.
dhr2VendorName :: Lens' DescribeHsmResponse (Maybe Text)
dhr2VendorName = lens _dhr2VendorName (\s a -> s { _dhr2VendorName = a })

-- | The identifier of the VPC that the HSM is in.
dhr2VpcId :: Lens' DescribeHsmResponse (Maybe Text)
dhr2VpcId = lens _dhr2VpcId (\s a -> s { _dhr2VpcId = a })

instance ToPath DescribeHsm where
    toPath = const "/"

instance ToQuery DescribeHsm where
    toQuery = const mempty

instance ToHeaders DescribeHsm

instance ToJSON DescribeHsm where
    toJSON DescribeHsm{..} = object
        [ "HsmArn"          .= _dh1HsmArn
        , "HsmSerialNumber" .= _dh1HsmSerialNumber
        ]

instance AWSRequest DescribeHsm where
    type Sv DescribeHsm = CloudHSM
    type Rs DescribeHsm = DescribeHsmResponse

    request  = post "DescribeHsm"
    response = jsonResponse

instance FromJSON DescribeHsmResponse where
    parseJSON = withObject "DescribeHsmResponse" $ \o -> DescribeHsmResponse
        <$> o .:? "AvailabilityZone"
        <*> o .:? "EniId"
        <*> o .:? "EniIp"
        <*> o .:? "HsmArn"
        <*> o .:? "HsmType"
        <*> o .:? "IamRoleArn"
        <*> o .:? "Partitions" .!= mempty
        <*> o .:? "SerialNumber"
        <*> o .:? "ServerCertLastUpdated"
        <*> o .:? "ServerCertUri"
        <*> o .:? "SoftwareVersion"
        <*> o .:? "SshKeyLastUpdated"
        <*> o .:? "SshPublicKey"
        <*> o .:? "Status"
        <*> o .:? "StatusDetails"
        <*> o .:? "SubnetId"
        <*> o .:? "SubscriptionEndDate"
        <*> o .:? "SubscriptionStartDate"
        <*> o .:? "SubscriptionType"
        <*> o .:? "VendorName"
        <*> o .:? "VpcId"
