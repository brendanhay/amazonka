{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.DescribeHSM
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves information about an HSM. You can identify the HSM by its ARN
-- or its serial number.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DescribeHSM.html>
module Network.AWS.CloudHSM.DescribeHSM
    (
    -- * Request
      DescribeHSM
    -- ** Request constructor
    , describeHSM
    -- ** Request lenses
    , dHSMSerialNumber
    , dHSMARN

    -- * Response
    , DescribeHSMResponse
    -- ** Response constructor
    , describeHSMResponse
    -- ** Response lenses
    , desIAMRoleARN
    , desStatus
    , desEniId
    , desSubscriptionEndDate
    , desVPCId
    , desSSHKeyLastUpdated
    , desServerCertURI
    , desSubscriptionType
    , desStatusDetails
    , desSSHPublicKey
    , desSubnetId
    , desPartitions
    , desAvailabilityZone
    , desSubscriptionStartDate
    , desServerCertLastUpdated
    , desSoftwareVersion
    , desSerialNumber
    , desVendorName
    , desHSMARN
    , desEniIP
    , desHSMType
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudHSM.Types

-- | /See:/ 'describeHSM' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dHSMSerialNumber'
--
-- * 'dHSMARN'
data DescribeHSM = DescribeHSM'{_dHSMSerialNumber :: Maybe Text, _dHSMARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeHSM' smart constructor.
describeHSM :: DescribeHSM
describeHSM = DescribeHSM'{_dHSMSerialNumber = Nothing, _dHSMARN = Nothing};

-- | The serial number of the HSM. Either the /HsmArn/ or the
-- /HsmSerialNumber/ parameter must be specified.
dHSMSerialNumber :: Lens' DescribeHSM (Maybe Text)
dHSMSerialNumber = lens _dHSMSerialNumber (\ s a -> s{_dHSMSerialNumber = a});

-- | The ARN of the HSM. Either the /HsmArn/ or the /SerialNumber/ parameter
-- must be specified.
dHSMARN :: Lens' DescribeHSM (Maybe Text)
dHSMARN = lens _dHSMARN (\ s a -> s{_dHSMARN = a});

instance AWSRequest DescribeHSM where
        type Sv DescribeHSM = CloudHSM
        type Rs DescribeHSM = DescribeHSMResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeHSMResponse' <$>
                   (x .?> "IamRoleArn") <*> (x .?> "Status") <*>
                     (x .?> "EniId")
                     <*> (x .?> "SubscriptionEndDate")
                     <*> (x .?> "VpcId")
                     <*> (x .?> "SshKeyLastUpdated")
                     <*> (x .?> "ServerCertUri")
                     <*> (x .?> "SubscriptionType")
                     <*> (x .?> "StatusDetails")
                     <*> (x .?> "SshPublicKey")
                     <*> (x .?> "SubnetId")
                     <*> (x .?> "Partitions" .!@ mempty)
                     <*> (x .?> "AvailabilityZone")
                     <*> (x .?> "SubscriptionStartDate")
                     <*> (x .?> "ServerCertLastUpdated")
                     <*> (x .?> "SoftwareVersion")
                     <*> (x .?> "SerialNumber")
                     <*> (x .?> "VendorName")
                     <*> (x .?> "HsmArn")
                     <*> (x .?> "EniIp")
                     <*> (x .?> "HsmType"))

instance ToHeaders DescribeHSM where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DescribeHSM" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeHSM where
        toJSON DescribeHSM'{..}
          = object
              ["HsmSerialNumber" .= _dHSMSerialNumber,
               "HsmArn" .= _dHSMARN]

instance ToPath DescribeHSM where
        toPath = const "/"

instance ToQuery DescribeHSM where
        toQuery = const mempty

-- | /See:/ 'describeHSMResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desIAMRoleARN'
--
-- * 'desStatus'
--
-- * 'desEniId'
--
-- * 'desSubscriptionEndDate'
--
-- * 'desVPCId'
--
-- * 'desSSHKeyLastUpdated'
--
-- * 'desServerCertURI'
--
-- * 'desSubscriptionType'
--
-- * 'desStatusDetails'
--
-- * 'desSSHPublicKey'
--
-- * 'desSubnetId'
--
-- * 'desPartitions'
--
-- * 'desAvailabilityZone'
--
-- * 'desSubscriptionStartDate'
--
-- * 'desServerCertLastUpdated'
--
-- * 'desSoftwareVersion'
--
-- * 'desSerialNumber'
--
-- * 'desVendorName'
--
-- * 'desHSMARN'
--
-- * 'desEniIP'
--
-- * 'desHSMType'
data DescribeHSMResponse = DescribeHSMResponse'{_desIAMRoleARN :: Maybe Text, _desStatus :: Maybe HSMStatus, _desEniId :: Maybe Text, _desSubscriptionEndDate :: Maybe Text, _desVPCId :: Maybe Text, _desSSHKeyLastUpdated :: Maybe Text, _desServerCertURI :: Maybe Text, _desSubscriptionType :: Maybe SubscriptionType, _desStatusDetails :: Maybe Text, _desSSHPublicKey :: Maybe Text, _desSubnetId :: Maybe Text, _desPartitions :: Maybe [Text], _desAvailabilityZone :: Maybe Text, _desSubscriptionStartDate :: Maybe Text, _desServerCertLastUpdated :: Maybe Text, _desSoftwareVersion :: Maybe Text, _desSerialNumber :: Maybe Text, _desVendorName :: Maybe Text, _desHSMARN :: Maybe Text, _desEniIP :: Maybe Text, _desHSMType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeHSMResponse' smart constructor.
describeHSMResponse :: DescribeHSMResponse
describeHSMResponse = DescribeHSMResponse'{_desIAMRoleARN = Nothing, _desStatus = Nothing, _desEniId = Nothing, _desSubscriptionEndDate = Nothing, _desVPCId = Nothing, _desSSHKeyLastUpdated = Nothing, _desServerCertURI = Nothing, _desSubscriptionType = Nothing, _desStatusDetails = Nothing, _desSSHPublicKey = Nothing, _desSubnetId = Nothing, _desPartitions = Nothing, _desAvailabilityZone = Nothing, _desSubscriptionStartDate = Nothing, _desServerCertLastUpdated = Nothing, _desSoftwareVersion = Nothing, _desSerialNumber = Nothing, _desVendorName = Nothing, _desHSMARN = Nothing, _desEniIP = Nothing, _desHSMType = Nothing};

-- | The ARN of the IAM role assigned to the HSM.
desIAMRoleARN :: Lens' DescribeHSMResponse (Maybe Text)
desIAMRoleARN = lens _desIAMRoleARN (\ s a -> s{_desIAMRoleARN = a});

-- | The status of the HSM.
desStatus :: Lens' DescribeHSMResponse (Maybe HSMStatus)
desStatus = lens _desStatus (\ s a -> s{_desStatus = a});

-- | The identifier of the elastic network interface (ENI) attached to the
-- HSM.
desEniId :: Lens' DescribeHSMResponse (Maybe Text)
desEniId = lens _desEniId (\ s a -> s{_desEniId = a});

-- | The subscription end date.
desSubscriptionEndDate :: Lens' DescribeHSMResponse (Maybe Text)
desSubscriptionEndDate = lens _desSubscriptionEndDate (\ s a -> s{_desSubscriptionEndDate = a});

-- | The identifier of the VPC that the HSM is in.
desVPCId :: Lens' DescribeHSMResponse (Maybe Text)
desVPCId = lens _desVPCId (\ s a -> s{_desVPCId = a});

-- | The date and time the SSH key was last updated.
desSSHKeyLastUpdated :: Lens' DescribeHSMResponse (Maybe Text)
desSSHKeyLastUpdated = lens _desSSHKeyLastUpdated (\ s a -> s{_desSSHKeyLastUpdated = a});

-- | The URI of the certificate server.
desServerCertURI :: Lens' DescribeHSMResponse (Maybe Text)
desServerCertURI = lens _desServerCertURI (\ s a -> s{_desServerCertURI = a});

-- | The subscription type.
desSubscriptionType :: Lens' DescribeHSMResponse (Maybe SubscriptionType)
desSubscriptionType = lens _desSubscriptionType (\ s a -> s{_desSubscriptionType = a});

-- | Contains additional information about the status of the HSM.
desStatusDetails :: Lens' DescribeHSMResponse (Maybe Text)
desStatusDetails = lens _desStatusDetails (\ s a -> s{_desStatusDetails = a});

-- | The public SSH key.
desSSHPublicKey :: Lens' DescribeHSMResponse (Maybe Text)
desSSHPublicKey = lens _desSSHPublicKey (\ s a -> s{_desSSHPublicKey = a});

-- | The identifier of the subnet the HSM is in.
desSubnetId :: Lens' DescribeHSMResponse (Maybe Text)
desSubnetId = lens _desSubnetId (\ s a -> s{_desSubnetId = a});

-- | The list of partitions on the HSM.
desPartitions :: Lens' DescribeHSMResponse [Text]
desPartitions = lens _desPartitions (\ s a -> s{_desPartitions = a}) . _Default;

-- | The Availability Zone that the HSM is in.
desAvailabilityZone :: Lens' DescribeHSMResponse (Maybe Text)
desAvailabilityZone = lens _desAvailabilityZone (\ s a -> s{_desAvailabilityZone = a});

-- | The subscription start date.
desSubscriptionStartDate :: Lens' DescribeHSMResponse (Maybe Text)
desSubscriptionStartDate = lens _desSubscriptionStartDate (\ s a -> s{_desSubscriptionStartDate = a});

-- | The date and time the server certificate was last updated.
desServerCertLastUpdated :: Lens' DescribeHSMResponse (Maybe Text)
desServerCertLastUpdated = lens _desServerCertLastUpdated (\ s a -> s{_desServerCertLastUpdated = a});

-- | The HSM software version.
desSoftwareVersion :: Lens' DescribeHSMResponse (Maybe Text)
desSoftwareVersion = lens _desSoftwareVersion (\ s a -> s{_desSoftwareVersion = a});

-- | The serial number of the HSM.
desSerialNumber :: Lens' DescribeHSMResponse (Maybe Text)
desSerialNumber = lens _desSerialNumber (\ s a -> s{_desSerialNumber = a});

-- | The name of the HSM vendor.
desVendorName :: Lens' DescribeHSMResponse (Maybe Text)
desVendorName = lens _desVendorName (\ s a -> s{_desVendorName = a});

-- | The ARN of the HSM.
desHSMARN :: Lens' DescribeHSMResponse (Maybe Text)
desHSMARN = lens _desHSMARN (\ s a -> s{_desHSMARN = a});

-- | The IP address assigned to the HSM\'s ENI.
desEniIP :: Lens' DescribeHSMResponse (Maybe Text)
desEniIP = lens _desEniIP (\ s a -> s{_desEniIP = a});

-- | The HSM model type.
desHSMType :: Lens' DescribeHSMResponse (Maybe Text)
desHSMType = lens _desHSMType (\ s a -> s{_desHSMType = a});
