{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeHSM
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an HSM. You can identify the HSM by its ARN
-- or its serial number.
--
-- /See:/ <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DescribeHSM.html AWS API Reference> for DescribeHSM.
module Network.AWS.CloudHSM.DescribeHSM
    (
    -- * Creating a Request
      DescribeHSM
    , describeHSM
    -- * Request Lenses
    , dhsmHSMSerialNumber
    , dhsmHSMARN

    -- * Destructuring the Response
    , DescribeHSMResponse
    , describeHSMResponse
    -- * Response Lenses
    , desrsIAMRoleARN
    , desrsEniId
    , desrsSubscriptionEndDate
    , desrsVPCId
    , desrsSSHKeyLastUpdated
    , desrsServerCertURI
    , desrsSubscriptionType
    , desrsStatusDetails
    , desrsSSHPublicKey
    , desrsSubnetId
    , desrsPartitions
    , desrsAvailabilityZone
    , desrsSubscriptionStartDate
    , desrsServerCertLastUpdated
    , desrsSoftwareVersion
    , desrsSerialNumber
    , desrsVendorName
    , desrsHSMARN
    , desrsEniIP
    , desrsHSMType
    , desrsStatus
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the DescribeHsm action.
--
-- /See:/ 'describeHSM' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhsmHSMSerialNumber'
--
-- * 'dhsmHSMARN'
data DescribeHSM = DescribeHSM'
    { _dhsmHSMSerialNumber :: !(Maybe Text)
    , _dhsmHSMARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeHSM' smart constructor.
describeHSM :: DescribeHSM
describeHSM = 
    DescribeHSM'
    { _dhsmHSMSerialNumber = Nothing
    , _dhsmHSMARN = Nothing
    }

-- | The serial number of the HSM. Either the /HsmArn/ or the
-- /HsmSerialNumber/ parameter must be specified.
dhsmHSMSerialNumber :: Lens' DescribeHSM (Maybe Text)
dhsmHSMSerialNumber = lens _dhsmHSMSerialNumber (\ s a -> s{_dhsmHSMSerialNumber = a});

-- | The ARN of the HSM. Either the /HsmArn/ or the /SerialNumber/ parameter
-- must be specified.
dhsmHSMARN :: Lens' DescribeHSM (Maybe Text)
dhsmHSMARN = lens _dhsmHSMARN (\ s a -> s{_dhsmHSMARN = a});

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
                    ("CloudHsmFrontendService.DescribeHsm" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeHSM where
        toJSON DescribeHSM'{..}
          = object
              ["HsmSerialNumber" .= _dhsmHSMSerialNumber,
               "HsmArn" .= _dhsmHSMARN]

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
-- * 'desrsIAMRoleARN'
--
-- * 'desrsEniId'
--
-- * 'desrsSubscriptionEndDate'
--
-- * 'desrsVPCId'
--
-- * 'desrsSSHKeyLastUpdated'
--
-- * 'desrsServerCertURI'
--
-- * 'desrsSubscriptionType'
--
-- * 'desrsStatusDetails'
--
-- * 'desrsSSHPublicKey'
--
-- * 'desrsSubnetId'
--
-- * 'desrsPartitions'
--
-- * 'desrsAvailabilityZone'
--
-- * 'desrsSubscriptionStartDate'
--
-- * 'desrsServerCertLastUpdated'
--
-- * 'desrsSoftwareVersion'
--
-- * 'desrsSerialNumber'
--
-- * 'desrsVendorName'
--
-- * 'desrsHSMARN'
--
-- * 'desrsEniIP'
--
-- * 'desrsHSMType'
--
-- * 'desrsStatus'
data DescribeHSMResponse = DescribeHSMResponse'
    { _desrsIAMRoleARN :: !(Maybe Text)
    , _desrsEniId :: !(Maybe Text)
    , _desrsSubscriptionEndDate :: !(Maybe Text)
    , _desrsVPCId :: !(Maybe Text)
    , _desrsSSHKeyLastUpdated :: !(Maybe Text)
    , _desrsServerCertURI :: !(Maybe Text)
    , _desrsSubscriptionType :: !(Maybe SubscriptionType)
    , _desrsStatusDetails :: !(Maybe Text)
    , _desrsSSHPublicKey :: !(Maybe Text)
    , _desrsSubnetId :: !(Maybe Text)
    , _desrsPartitions :: !(Maybe [Text])
    , _desrsAvailabilityZone :: !(Maybe Text)
    , _desrsSubscriptionStartDate :: !(Maybe Text)
    , _desrsServerCertLastUpdated :: !(Maybe Text)
    , _desrsSoftwareVersion :: !(Maybe Text)
    , _desrsSerialNumber :: !(Maybe Text)
    , _desrsVendorName :: !(Maybe Text)
    , _desrsHSMARN :: !(Maybe Text)
    , _desrsEniIP :: !(Maybe Text)
    , _desrsHSMType :: !(Maybe Text)
    , _desrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeHSMResponse' smart constructor.
describeHSMResponse :: Int -> DescribeHSMResponse
describeHSMResponse pStatus_ = 
    DescribeHSMResponse'
    { _desrsIAMRoleARN = Nothing
    , _desrsEniId = Nothing
    , _desrsSubscriptionEndDate = Nothing
    , _desrsVPCId = Nothing
    , _desrsSSHKeyLastUpdated = Nothing
    , _desrsServerCertURI = Nothing
    , _desrsSubscriptionType = Nothing
    , _desrsStatusDetails = Nothing
    , _desrsSSHPublicKey = Nothing
    , _desrsSubnetId = Nothing
    , _desrsPartitions = Nothing
    , _desrsAvailabilityZone = Nothing
    , _desrsSubscriptionStartDate = Nothing
    , _desrsServerCertLastUpdated = Nothing
    , _desrsSoftwareVersion = Nothing
    , _desrsSerialNumber = Nothing
    , _desrsVendorName = Nothing
    , _desrsHSMARN = Nothing
    , _desrsEniIP = Nothing
    , _desrsHSMType = Nothing
    , _desrsStatus = pStatus_
    }

-- | The ARN of the IAM role assigned to the HSM.
desrsIAMRoleARN :: Lens' DescribeHSMResponse (Maybe Text)
desrsIAMRoleARN = lens _desrsIAMRoleARN (\ s a -> s{_desrsIAMRoleARN = a});

-- | The identifier of the elastic network interface (ENI) attached to the
-- HSM.
desrsEniId :: Lens' DescribeHSMResponse (Maybe Text)
desrsEniId = lens _desrsEniId (\ s a -> s{_desrsEniId = a});

-- | The subscription end date.
desrsSubscriptionEndDate :: Lens' DescribeHSMResponse (Maybe Text)
desrsSubscriptionEndDate = lens _desrsSubscriptionEndDate (\ s a -> s{_desrsSubscriptionEndDate = a});

-- | The identifier of the VPC that the HSM is in.
desrsVPCId :: Lens' DescribeHSMResponse (Maybe Text)
desrsVPCId = lens _desrsVPCId (\ s a -> s{_desrsVPCId = a});

-- | The date and time the SSH key was last updated.
desrsSSHKeyLastUpdated :: Lens' DescribeHSMResponse (Maybe Text)
desrsSSHKeyLastUpdated = lens _desrsSSHKeyLastUpdated (\ s a -> s{_desrsSSHKeyLastUpdated = a});

-- | The URI of the certificate server.
desrsServerCertURI :: Lens' DescribeHSMResponse (Maybe Text)
desrsServerCertURI = lens _desrsServerCertURI (\ s a -> s{_desrsServerCertURI = a});

-- | The subscription type.
desrsSubscriptionType :: Lens' DescribeHSMResponse (Maybe SubscriptionType)
desrsSubscriptionType = lens _desrsSubscriptionType (\ s a -> s{_desrsSubscriptionType = a});

-- | Contains additional information about the status of the HSM.
desrsStatusDetails :: Lens' DescribeHSMResponse (Maybe Text)
desrsStatusDetails = lens _desrsStatusDetails (\ s a -> s{_desrsStatusDetails = a});

-- | The public SSH key.
desrsSSHPublicKey :: Lens' DescribeHSMResponse (Maybe Text)
desrsSSHPublicKey = lens _desrsSSHPublicKey (\ s a -> s{_desrsSSHPublicKey = a});

-- | The identifier of the subnet the HSM is in.
desrsSubnetId :: Lens' DescribeHSMResponse (Maybe Text)
desrsSubnetId = lens _desrsSubnetId (\ s a -> s{_desrsSubnetId = a});

-- | The list of partitions on the HSM.
desrsPartitions :: Lens' DescribeHSMResponse [Text]
desrsPartitions = lens _desrsPartitions (\ s a -> s{_desrsPartitions = a}) . _Default . _Coerce;

-- | The Availability Zone that the HSM is in.
desrsAvailabilityZone :: Lens' DescribeHSMResponse (Maybe Text)
desrsAvailabilityZone = lens _desrsAvailabilityZone (\ s a -> s{_desrsAvailabilityZone = a});

-- | The subscription start date.
desrsSubscriptionStartDate :: Lens' DescribeHSMResponse (Maybe Text)
desrsSubscriptionStartDate = lens _desrsSubscriptionStartDate (\ s a -> s{_desrsSubscriptionStartDate = a});

-- | The date and time the server certificate was last updated.
desrsServerCertLastUpdated :: Lens' DescribeHSMResponse (Maybe Text)
desrsServerCertLastUpdated = lens _desrsServerCertLastUpdated (\ s a -> s{_desrsServerCertLastUpdated = a});

-- | The HSM software version.
desrsSoftwareVersion :: Lens' DescribeHSMResponse (Maybe Text)
desrsSoftwareVersion = lens _desrsSoftwareVersion (\ s a -> s{_desrsSoftwareVersion = a});

-- | The serial number of the HSM.
desrsSerialNumber :: Lens' DescribeHSMResponse (Maybe Text)
desrsSerialNumber = lens _desrsSerialNumber (\ s a -> s{_desrsSerialNumber = a});

-- | The name of the HSM vendor.
desrsVendorName :: Lens' DescribeHSMResponse (Maybe Text)
desrsVendorName = lens _desrsVendorName (\ s a -> s{_desrsVendorName = a});

-- | The ARN of the HSM.
desrsHSMARN :: Lens' DescribeHSMResponse (Maybe Text)
desrsHSMARN = lens _desrsHSMARN (\ s a -> s{_desrsHSMARN = a});

-- | The IP address assigned to the HSM\'s ENI.
desrsEniIP :: Lens' DescribeHSMResponse (Maybe Text)
desrsEniIP = lens _desrsEniIP (\ s a -> s{_desrsEniIP = a});

-- | The HSM model type.
desrsHSMType :: Lens' DescribeHSMResponse (Maybe Text)
desrsHSMType = lens _desrsHSMType (\ s a -> s{_desrsHSMType = a});

-- | Undocumented member.
desrsStatus :: Lens' DescribeHSMResponse Int
desrsStatus = lens _desrsStatus (\ s a -> s{_desrsStatus = a});
