{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeHSM
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an HSM. You can identify the HSM by its ARN
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
    , dhsmrqHSMSerialNumber
    , dhsmrqHSMARN

    -- * Response
    , DescribeHSMResponse
    -- ** Response constructor
    , describeHSMResponse
    -- ** Response lenses
    , dhsmrsIAMRoleARN
    , dhsmrsEniId
    , dhsmrsSubscriptionEndDate
    , dhsmrsVPCId
    , dhsmrsSSHKeyLastUpdated
    , dhsmrsServerCertURI
    , dhsmrsSubscriptionType
    , dhsmrsStatusDetails
    , dhsmrsSSHPublicKey
    , dhsmrsSubnetId
    , dhsmrsPartitions
    , dhsmrsAvailabilityZone
    , dhsmrsSubscriptionStartDate
    , dhsmrsServerCertLastUpdated
    , dhsmrsSoftwareVersion
    , dhsmrsSerialNumber
    , dhsmrsVendorName
    , dhsmrsHSMARN
    , dhsmrsEniIP
    , dhsmrsHSMType
    , dhsmrsStatus
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
-- * 'dhsmrqHSMSerialNumber'
--
-- * 'dhsmrqHSMARN'
data DescribeHSM = DescribeHSM'
    { _dhsmrqHSMSerialNumber :: !(Maybe Text)
    , _dhsmrqHSMARN          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeHSM' smart constructor.
describeHSM :: DescribeHSM
describeHSM =
    DescribeHSM'
    { _dhsmrqHSMSerialNumber = Nothing
    , _dhsmrqHSMARN = Nothing
    }

-- | The serial number of the HSM. Either the /HsmArn/ or the
-- /HsmSerialNumber/ parameter must be specified.
dhsmrqHSMSerialNumber :: Lens' DescribeHSM (Maybe Text)
dhsmrqHSMSerialNumber = lens _dhsmrqHSMSerialNumber (\ s a -> s{_dhsmrqHSMSerialNumber = a});

-- | The ARN of the HSM. Either the /HsmArn/ or the /SerialNumber/ parameter
-- must be specified.
dhsmrqHSMARN :: Lens' DescribeHSM (Maybe Text)
dhsmrqHSMARN = lens _dhsmrqHSMARN (\ s a -> s{_dhsmrqHSMARN = a});

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
              ["HsmSerialNumber" .= _dhsmrqHSMSerialNumber,
               "HsmArn" .= _dhsmrqHSMARN]

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
-- * 'dhsmrsIAMRoleARN'
--
-- * 'dhsmrsEniId'
--
-- * 'dhsmrsSubscriptionEndDate'
--
-- * 'dhsmrsVPCId'
--
-- * 'dhsmrsSSHKeyLastUpdated'
--
-- * 'dhsmrsServerCertURI'
--
-- * 'dhsmrsSubscriptionType'
--
-- * 'dhsmrsStatusDetails'
--
-- * 'dhsmrsSSHPublicKey'
--
-- * 'dhsmrsSubnetId'
--
-- * 'dhsmrsPartitions'
--
-- * 'dhsmrsAvailabilityZone'
--
-- * 'dhsmrsSubscriptionStartDate'
--
-- * 'dhsmrsServerCertLastUpdated'
--
-- * 'dhsmrsSoftwareVersion'
--
-- * 'dhsmrsSerialNumber'
--
-- * 'dhsmrsVendorName'
--
-- * 'dhsmrsHSMARN'
--
-- * 'dhsmrsEniIP'
--
-- * 'dhsmrsHSMType'
--
-- * 'dhsmrsStatus'
data DescribeHSMResponse = DescribeHSMResponse'
    { _dhsmrsIAMRoleARN            :: !(Maybe Text)
    , _dhsmrsEniId                 :: !(Maybe Text)
    , _dhsmrsSubscriptionEndDate   :: !(Maybe Text)
    , _dhsmrsVPCId                 :: !(Maybe Text)
    , _dhsmrsSSHKeyLastUpdated     :: !(Maybe Text)
    , _dhsmrsServerCertURI         :: !(Maybe Text)
    , _dhsmrsSubscriptionType      :: !(Maybe SubscriptionType)
    , _dhsmrsStatusDetails         :: !(Maybe Text)
    , _dhsmrsSSHPublicKey          :: !(Maybe Text)
    , _dhsmrsSubnetId              :: !(Maybe Text)
    , _dhsmrsPartitions            :: !(Maybe [Text])
    , _dhsmrsAvailabilityZone      :: !(Maybe Text)
    , _dhsmrsSubscriptionStartDate :: !(Maybe Text)
    , _dhsmrsServerCertLastUpdated :: !(Maybe Text)
    , _dhsmrsSoftwareVersion       :: !(Maybe Text)
    , _dhsmrsSerialNumber          :: !(Maybe Text)
    , _dhsmrsVendorName            :: !(Maybe Text)
    , _dhsmrsHSMARN                :: !(Maybe Text)
    , _dhsmrsEniIP                 :: !(Maybe Text)
    , _dhsmrsHSMType               :: !(Maybe Text)
    , _dhsmrsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeHSMResponse' smart constructor.
describeHSMResponse :: Int -> DescribeHSMResponse
describeHSMResponse pStatus =
    DescribeHSMResponse'
    { _dhsmrsIAMRoleARN = Nothing
    , _dhsmrsEniId = Nothing
    , _dhsmrsSubscriptionEndDate = Nothing
    , _dhsmrsVPCId = Nothing
    , _dhsmrsSSHKeyLastUpdated = Nothing
    , _dhsmrsServerCertURI = Nothing
    , _dhsmrsSubscriptionType = Nothing
    , _dhsmrsStatusDetails = Nothing
    , _dhsmrsSSHPublicKey = Nothing
    , _dhsmrsSubnetId = Nothing
    , _dhsmrsPartitions = Nothing
    , _dhsmrsAvailabilityZone = Nothing
    , _dhsmrsSubscriptionStartDate = Nothing
    , _dhsmrsServerCertLastUpdated = Nothing
    , _dhsmrsSoftwareVersion = Nothing
    , _dhsmrsSerialNumber = Nothing
    , _dhsmrsVendorName = Nothing
    , _dhsmrsHSMARN = Nothing
    , _dhsmrsEniIP = Nothing
    , _dhsmrsHSMType = Nothing
    , _dhsmrsStatus = pStatus
    }

-- | The ARN of the IAM role assigned to the HSM.
dhsmrsIAMRoleARN :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsIAMRoleARN = lens _dhsmrsIAMRoleARN (\ s a -> s{_dhsmrsIAMRoleARN = a});

-- | The identifier of the elastic network interface (ENI) attached to the
-- HSM.
dhsmrsEniId :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsEniId = lens _dhsmrsEniId (\ s a -> s{_dhsmrsEniId = a});

-- | The subscription end date.
dhsmrsSubscriptionEndDate :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsSubscriptionEndDate = lens _dhsmrsSubscriptionEndDate (\ s a -> s{_dhsmrsSubscriptionEndDate = a});

-- | The identifier of the VPC that the HSM is in.
dhsmrsVPCId :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsVPCId = lens _dhsmrsVPCId (\ s a -> s{_dhsmrsVPCId = a});

-- | The date and time the SSH key was last updated.
dhsmrsSSHKeyLastUpdated :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsSSHKeyLastUpdated = lens _dhsmrsSSHKeyLastUpdated (\ s a -> s{_dhsmrsSSHKeyLastUpdated = a});

-- | The URI of the certificate server.
dhsmrsServerCertURI :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsServerCertURI = lens _dhsmrsServerCertURI (\ s a -> s{_dhsmrsServerCertURI = a});

-- | The subscription type.
dhsmrsSubscriptionType :: Lens' DescribeHSMResponse (Maybe SubscriptionType)
dhsmrsSubscriptionType = lens _dhsmrsSubscriptionType (\ s a -> s{_dhsmrsSubscriptionType = a});

-- | Contains additional information about the status of the HSM.
dhsmrsStatusDetails :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsStatusDetails = lens _dhsmrsStatusDetails (\ s a -> s{_dhsmrsStatusDetails = a});

-- | The public SSH key.
dhsmrsSSHPublicKey :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsSSHPublicKey = lens _dhsmrsSSHPublicKey (\ s a -> s{_dhsmrsSSHPublicKey = a});

-- | The identifier of the subnet the HSM is in.
dhsmrsSubnetId :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsSubnetId = lens _dhsmrsSubnetId (\ s a -> s{_dhsmrsSubnetId = a});

-- | The list of partitions on the HSM.
dhsmrsPartitions :: Lens' DescribeHSMResponse [Text]
dhsmrsPartitions = lens _dhsmrsPartitions (\ s a -> s{_dhsmrsPartitions = a}) . _Default;

-- | The Availability Zone that the HSM is in.
dhsmrsAvailabilityZone :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsAvailabilityZone = lens _dhsmrsAvailabilityZone (\ s a -> s{_dhsmrsAvailabilityZone = a});

-- | The subscription start date.
dhsmrsSubscriptionStartDate :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsSubscriptionStartDate = lens _dhsmrsSubscriptionStartDate (\ s a -> s{_dhsmrsSubscriptionStartDate = a});

-- | The date and time the server certificate was last updated.
dhsmrsServerCertLastUpdated :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsServerCertLastUpdated = lens _dhsmrsServerCertLastUpdated (\ s a -> s{_dhsmrsServerCertLastUpdated = a});

-- | The HSM software version.
dhsmrsSoftwareVersion :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsSoftwareVersion = lens _dhsmrsSoftwareVersion (\ s a -> s{_dhsmrsSoftwareVersion = a});

-- | The serial number of the HSM.
dhsmrsSerialNumber :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsSerialNumber = lens _dhsmrsSerialNumber (\ s a -> s{_dhsmrsSerialNumber = a});

-- | The name of the HSM vendor.
dhsmrsVendorName :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsVendorName = lens _dhsmrsVendorName (\ s a -> s{_dhsmrsVendorName = a});

-- | The ARN of the HSM.
dhsmrsHSMARN :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsHSMARN = lens _dhsmrsHSMARN (\ s a -> s{_dhsmrsHSMARN = a});

-- | The IP address assigned to the HSM\'s ENI.
dhsmrsEniIP :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsEniIP = lens _dhsmrsEniIP (\ s a -> s{_dhsmrsEniIP = a});

-- | The HSM model type.
dhsmrsHSMType :: Lens' DescribeHSMResponse (Maybe Text)
dhsmrsHSMType = lens _dhsmrsHSMType (\ s a -> s{_dhsmrsHSMType = a});

-- | FIXME: Undocumented member.
dhsmrsStatus :: Lens' DescribeHSMResponse Int
dhsmrsStatus = lens _dhsmrsStatus (\ s a -> s{_dhsmrsStatus = a});
