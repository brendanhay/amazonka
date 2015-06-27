{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , desHSMSerialNumber
    , desHSMARN

    -- * Response
    , DescribeHSMResponse
    -- ** Response constructor
    , describeHSMResponse
    -- ** Response lenses
    , dhsmrIAMRoleARN
    , dhsmrEniId
    , dhsmrSubscriptionEndDate
    , dhsmrVPCId
    , dhsmrSSHKeyLastUpdated
    , dhsmrServerCertURI
    , dhsmrSubscriptionType
    , dhsmrStatusDetails
    , dhsmrSSHPublicKey
    , dhsmrSubnetId
    , dhsmrPartitions
    , dhsmrAvailabilityZone
    , dhsmrSubscriptionStartDate
    , dhsmrServerCertLastUpdated
    , dhsmrSoftwareVersion
    , dhsmrSerialNumber
    , dhsmrVendorName
    , dhsmrHSMARN
    , dhsmrEniIP
    , dhsmrHSMType
    , dhsmrStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the DescribeHsm action.
--
-- /See:/ 'describeHSM' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desHSMSerialNumber'
--
-- * 'desHSMARN'
data DescribeHSM = DescribeHSM'
    { _desHSMSerialNumber :: Maybe Text
    , _desHSMARN          :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'DescribeHSM' smart constructor.
describeHSM :: DescribeHSM
describeHSM =
    DescribeHSM'
    { _desHSMSerialNumber = Nothing
    , _desHSMARN = Nothing
    }

-- | The serial number of the HSM. Either the /HsmArn/ or the
-- /HsmSerialNumber/ parameter must be specified.
desHSMSerialNumber :: Lens' DescribeHSM (Maybe Text)
desHSMSerialNumber = lens _desHSMSerialNumber (\ s a -> s{_desHSMSerialNumber = a});

-- | The ARN of the HSM. Either the /HsmArn/ or the /SerialNumber/ parameter
-- must be specified.
desHSMARN :: Lens' DescribeHSM (Maybe Text)
desHSMARN = lens _desHSMARN (\ s a -> s{_desHSMARN = a});

instance AWSRequest DescribeHSM where
        type Sv DescribeHSM = CloudHSM
        type Rs DescribeHSM = DescribeHSMResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeHSMResponse' <$>
                   (x .?> "IamRoleArn") <*> (x .?> "EniId") <*>
                     (x .?> "SubscriptionEndDate")
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
                     <*> (x .?> "HsmType")
                     <*> (pure (fromEnum s)))

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
              ["HsmSerialNumber" .= _desHSMSerialNumber,
               "HsmArn" .= _desHSMARN]

instance ToPath DescribeHSM where
        toPath = const "/"

instance ToQuery DescribeHSM where
        toQuery = const mempty

-- | Contains the output of the DescribeHsm action.
--
-- /See:/ 'describeHSMResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhsmrIAMRoleARN'
--
-- * 'dhsmrEniId'
--
-- * 'dhsmrSubscriptionEndDate'
--
-- * 'dhsmrVPCId'
--
-- * 'dhsmrSSHKeyLastUpdated'
--
-- * 'dhsmrServerCertURI'
--
-- * 'dhsmrSubscriptionType'
--
-- * 'dhsmrStatusDetails'
--
-- * 'dhsmrSSHPublicKey'
--
-- * 'dhsmrSubnetId'
--
-- * 'dhsmrPartitions'
--
-- * 'dhsmrAvailabilityZone'
--
-- * 'dhsmrSubscriptionStartDate'
--
-- * 'dhsmrServerCertLastUpdated'
--
-- * 'dhsmrSoftwareVersion'
--
-- * 'dhsmrSerialNumber'
--
-- * 'dhsmrVendorName'
--
-- * 'dhsmrHSMARN'
--
-- * 'dhsmrEniIP'
--
-- * 'dhsmrHSMType'
--
-- * 'dhsmrStatus'
data DescribeHSMResponse = DescribeHSMResponse'
    { _dhsmrIAMRoleARN            :: Maybe Text
    , _dhsmrEniId                 :: Maybe Text
    , _dhsmrSubscriptionEndDate   :: Maybe Text
    , _dhsmrVPCId                 :: Maybe Text
    , _dhsmrSSHKeyLastUpdated     :: Maybe Text
    , _dhsmrServerCertURI         :: Maybe Text
    , _dhsmrSubscriptionType      :: Maybe SubscriptionType
    , _dhsmrStatusDetails         :: Maybe Text
    , _dhsmrSSHPublicKey          :: Maybe Text
    , _dhsmrSubnetId              :: Maybe Text
    , _dhsmrPartitions            :: Maybe [Text]
    , _dhsmrAvailabilityZone      :: Maybe Text
    , _dhsmrSubscriptionStartDate :: Maybe Text
    , _dhsmrServerCertLastUpdated :: Maybe Text
    , _dhsmrSoftwareVersion       :: Maybe Text
    , _dhsmrSerialNumber          :: Maybe Text
    , _dhsmrVendorName            :: Maybe Text
    , _dhsmrHSMARN                :: Maybe Text
    , _dhsmrEniIP                 :: Maybe Text
    , _dhsmrHSMType               :: Maybe Text
    , _dhsmrStatus                :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeHSMResponse' smart constructor.
describeHSMResponse :: Int -> DescribeHSMResponse
describeHSMResponse pStatus =
    DescribeHSMResponse'
    { _dhsmrIAMRoleARN = Nothing
    , _dhsmrEniId = Nothing
    , _dhsmrSubscriptionEndDate = Nothing
    , _dhsmrVPCId = Nothing
    , _dhsmrSSHKeyLastUpdated = Nothing
    , _dhsmrServerCertURI = Nothing
    , _dhsmrSubscriptionType = Nothing
    , _dhsmrStatusDetails = Nothing
    , _dhsmrSSHPublicKey = Nothing
    , _dhsmrSubnetId = Nothing
    , _dhsmrPartitions = Nothing
    , _dhsmrAvailabilityZone = Nothing
    , _dhsmrSubscriptionStartDate = Nothing
    , _dhsmrServerCertLastUpdated = Nothing
    , _dhsmrSoftwareVersion = Nothing
    , _dhsmrSerialNumber = Nothing
    , _dhsmrVendorName = Nothing
    , _dhsmrHSMARN = Nothing
    , _dhsmrEniIP = Nothing
    , _dhsmrHSMType = Nothing
    , _dhsmrStatus = pStatus
    }

-- | The ARN of the IAM role assigned to the HSM.
dhsmrIAMRoleARN :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrIAMRoleARN = lens _dhsmrIAMRoleARN (\ s a -> s{_dhsmrIAMRoleARN = a});

-- | The identifier of the elastic network interface (ENI) attached to the
-- HSM.
dhsmrEniId :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrEniId = lens _dhsmrEniId (\ s a -> s{_dhsmrEniId = a});

-- | The subscription end date.
dhsmrSubscriptionEndDate :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrSubscriptionEndDate = lens _dhsmrSubscriptionEndDate (\ s a -> s{_dhsmrSubscriptionEndDate = a});

-- | The identifier of the VPC that the HSM is in.
dhsmrVPCId :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrVPCId = lens _dhsmrVPCId (\ s a -> s{_dhsmrVPCId = a});

-- | The date and time the SSH key was last updated.
dhsmrSSHKeyLastUpdated :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrSSHKeyLastUpdated = lens _dhsmrSSHKeyLastUpdated (\ s a -> s{_dhsmrSSHKeyLastUpdated = a});

-- | The URI of the certificate server.
dhsmrServerCertURI :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrServerCertURI = lens _dhsmrServerCertURI (\ s a -> s{_dhsmrServerCertURI = a});

-- | The subscription type.
dhsmrSubscriptionType :: Lens' DescribeHSMResponse (Maybe SubscriptionType)
dhsmrSubscriptionType = lens _dhsmrSubscriptionType (\ s a -> s{_dhsmrSubscriptionType = a});

-- | Contains additional information about the status of the HSM.
dhsmrStatusDetails :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrStatusDetails = lens _dhsmrStatusDetails (\ s a -> s{_dhsmrStatusDetails = a});

-- | The public SSH key.
dhsmrSSHPublicKey :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrSSHPublicKey = lens _dhsmrSSHPublicKey (\ s a -> s{_dhsmrSSHPublicKey = a});

-- | The identifier of the subnet the HSM is in.
dhsmrSubnetId :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrSubnetId = lens _dhsmrSubnetId (\ s a -> s{_dhsmrSubnetId = a});

-- | The list of partitions on the HSM.
dhsmrPartitions :: Lens' DescribeHSMResponse [Text]
dhsmrPartitions = lens _dhsmrPartitions (\ s a -> s{_dhsmrPartitions = a}) . _Default;

-- | The Availability Zone that the HSM is in.
dhsmrAvailabilityZone :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrAvailabilityZone = lens _dhsmrAvailabilityZone (\ s a -> s{_dhsmrAvailabilityZone = a});

-- | The subscription start date.
dhsmrSubscriptionStartDate :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrSubscriptionStartDate = lens _dhsmrSubscriptionStartDate (\ s a -> s{_dhsmrSubscriptionStartDate = a});

-- | The date and time the server certificate was last updated.
dhsmrServerCertLastUpdated :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrServerCertLastUpdated = lens _dhsmrServerCertLastUpdated (\ s a -> s{_dhsmrServerCertLastUpdated = a});

-- | The HSM software version.
dhsmrSoftwareVersion :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrSoftwareVersion = lens _dhsmrSoftwareVersion (\ s a -> s{_dhsmrSoftwareVersion = a});

-- | The serial number of the HSM.
dhsmrSerialNumber :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrSerialNumber = lens _dhsmrSerialNumber (\ s a -> s{_dhsmrSerialNumber = a});

-- | The name of the HSM vendor.
dhsmrVendorName :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrVendorName = lens _dhsmrVendorName (\ s a -> s{_dhsmrVendorName = a});

-- | The ARN of the HSM.
dhsmrHSMARN :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrHSMARN = lens _dhsmrHSMARN (\ s a -> s{_dhsmrHSMARN = a});

-- | The IP address assigned to the HSM\'s ENI.
dhsmrEniIP :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrEniIP = lens _dhsmrEniIP (\ s a -> s{_dhsmrEniIP = a});

-- | The HSM model type.
dhsmrHSMType :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrHSMType = lens _dhsmrHSMType (\ s a -> s{_dhsmrHSMType = a});

-- | FIXME: Undocumented member.
dhsmrStatus :: Lens' DescribeHSMResponse Int
dhsmrStatus = lens _dhsmrStatus (\ s a -> s{_dhsmrStatus = a});
